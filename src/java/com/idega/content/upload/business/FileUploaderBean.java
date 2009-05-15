package com.idega.content.upload.business;

import java.io.BufferedInputStream;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.faces.component.UIComponent;

import org.apache.commons.httpclient.UsernamePasswordCredentials;
import org.apache.webdav.lib.WebdavResource;
import org.directwebremoting.WebContext;
import org.directwebremoting.WebContextFactory;
import org.jdom.Document;
import org.springframework.beans.factory.annotation.Autowired;

import com.idega.block.web2.business.JQuery;
import com.idega.block.web2.business.JQueryPlugin;
import com.idega.block.web2.business.Web2Business;
import com.idega.builder.bean.AdvancedProperty;
import com.idega.builder.business.BuilderLogicWrapper;
import com.idega.business.IBOLookup;
import com.idega.business.IBOLookupException;
import com.idega.content.business.ContentConstants;
import com.idega.content.business.ContentUtil;
import com.idega.content.business.WebDAVUploadBean;
import com.idega.content.upload.bean.UploadFile;
import com.idega.core.builder.business.BuilderService;
import com.idega.idegaweb.IWBundle;
import com.idega.idegaweb.IWResourceBundle;
import com.idega.presentation.IWContext;
import com.idega.presentation.Image;
import com.idega.presentation.Layer;
import com.idega.presentation.text.Link;
import com.idega.presentation.text.ListItem;
import com.idega.presentation.text.Lists;
import com.idega.presentation.ui.FileInput;
import com.idega.slide.business.IWSlideService;
import com.idega.slide.business.IWSlideSession;
import com.idega.user.data.User;
import com.idega.util.CoreConstants;
import com.idega.util.CoreUtil;
import com.idega.util.ListUtil;
import com.idega.util.PresentationUtil;
import com.idega.util.StringUtil;

public class FileUploaderBean implements FileUploader {

	private static final long serialVersionUID = 896091693178092417L;
	private static final Logger LOGGER = Logger.getLogger(FileUploaderBean.class.getName());
	
	private BuilderService builder = null;
	private IWSlideService slide = null;
	
	@Autowired
	private BuilderLogicWrapper builderLogicWrapper;
	
	@Autowired
	private Web2Business web2;
	
	@Autowired
	private JQuery jQuery;
	
	public static final String FILE_UPLOAD_INPUT_STYLE = "fileUploadInputStyle";
	
	public void initializeUploader(IWContext iwc) {
		if (iwc == null) {
			return;
		}
		
		if (builder != null && slide != null) {
			return;
		}
		
		getBuilderService(iwc);
		getSlideService(iwc);
	}
	
	public Layer getFileInput(IWContext iwc, String id, boolean addRemoveImage, boolean showProgressBar, boolean addjQuery, boolean autoAddFileInput,
			boolean autoUpload) {
		if (iwc == null) {
			return null;
		}
		
		IWBundle bundle = iwc.getIWMainApplication().getBundle(ContentConstants.IW_BUNDLE_IDENTIFIER);
		IWResourceBundle iwrb = bundle.getResourceBundle(iwc);
		String name = iwrb.getLocalizedString("remove", "Remove");
		String message = iwrb.getLocalizedString("are_you_sure", "Are you sure?");
		
		Layer fileInputContainer = new Layer();
		
		FileInput input = new FileInput();
		input.getId();
		input.setStyleClass(FILE_UPLOAD_INPUT_STYLE);
		input.setName(ContentConstants.UPLOAD_FIELD_NAME);
		if (autoUpload) {
			input.setOnChange(getActionToLoadFilesAndExecuteCustomAction("FileUploadHelper.uploadFiles();", showProgressBar, addjQuery));
		}
		fileInputContainer.add(input);
		
		if (autoAddFileInput) {
			input.setOnChange(getActionToLoadFilesAndExecuteCustomAction(getAddFileInputJavaScriptAction(id, iwrb, showProgressBar, addjQuery, autoAddFileInput,
					autoUpload), showProgressBar, addjQuery));
		}
		
		if (addRemoveImage) {
			Image remove = new Image(bundle.getVirtualPathWithFileNameString("images/delete.png"), name, 18, 18);
			remove.setStyleClass("removeFileUploadInputImageStyle");
			remove.setOnClick(new StringBuffer("removeFileInput('").append(fileInputContainer.getId()).append("', '").append(message).append("');").toString());
			fileInputContainer.add(remove);
		}
		
		return fileInputContainer;
	}

	public String getPropertiesAction(IWContext iwc, String id, String progressBarId, String uploadId, boolean showProgressBar, boolean showLoadingMessage,
			boolean zipFile, String formId, String actionAfterUpload, String actionAfterCounterReset, boolean autoUpload, boolean showUploadedFiles,
			String componentToRerenderId) {
		IWResourceBundle iwrb = ContentUtil.getBundle().getResourceBundle(iwc);
		return new StringBuilder("FileUploadHelper.setProperties({id: '").append(id).append("', showProgressBar: ").append(showProgressBar)
			.append(", showMessage: ").append(showLoadingMessage).append(", zipFile: ").append(zipFile).append(", formId: '").append(formId)
			.append("', progressBarId: '").append(progressBarId).append("', ")
			.append("localizations: {UPLOADING_FILE_PROGRESS_BOX_TEXT: '").append(iwrb.getLocalizedString("uploading_file", "Uploading file"))
			.append("', UPLOADING_FILE_PLEASE_WAIT_PROGRESS_BOX_TEXT: '").append(iwrb.getLocalizedString("completed_please_wait", "completed, please wait..."))
			.append("', UPLOADING_FILE_PROGRESS_BOX_FILE_UPLOADED_TEXT: '")
				.append(iwrb.getLocalizedString("upload_was_successfully_finished", "Upload was successfully finished."))
			.append("', UPLOADING_FILE_MESSAGE: '").append(iwrb.getLocalizedString("uploading", "Uploading..."))
			.append("', UPLOADING_FILE_INVALID_TYPE_MESSAGE: '")
				.append(iwrb.getLocalizedString("incorrect_file_type", "Unsupported file type! Only zip files allowed")).append("'}, ")
			.append("actionAfterUpload: ").append(getJavaScriptAction(actionAfterUpload))
			.append(", actionAfterCounterReset: ").append(getJavaScriptAction(actionAfterCounterReset))
			.append(", uploadId: '").append(uploadId).append("', autoUpload: ").append(autoUpload).append(", showUploadedFiles: ").append(showUploadedFiles)
		.append("});").toString();
	}
	
	public Document getRenderedFileInput(String id, boolean showProgressBar, boolean addjQuery, boolean autoAddFileInput, boolean autoUpload) {
		IWContext iwc = CoreUtil.getIWContext();
		if (iwc == null) {
			return null;
		}
		
		BuilderService builder = getBuilderService(iwc);
		if (builder == null) {
			return null;
		}
		
		return builder.getRenderedComponent(iwc, getFileInput(iwc, id, true, showProgressBar, addjQuery, autoAddFileInput, autoUpload), false);
	}

	public boolean uploadFile(List<UploadFile> files, String uploadPath, boolean isIE) {
		return doUploading(files, uploadPath, false, false, false, isIE);
	}
	
	public boolean uploadThemePack(List<UploadFile> files, String uploadPath, boolean isIE) {
		return doUploading(files, uploadPath, true, true, true, isIE);
	}
	
	public boolean uploadZipFile(List<UploadFile> files, String uploadPath, boolean extractContent, boolean isIE) {
		return doUploading(files, uploadPath, true, false, extractContent, isIE);
	}
	
	private boolean doUploading(List<UploadFile> files, String path, boolean zipFile, boolean themePack, boolean extractContent, boolean isIE) {
		if (files == null || path == null) {
			return false;
		}
		
		if (slide == null) {
			return false;
		}
		
		boolean result = true;
		for (int i = 0; (i < files.size() && result); i++) {
			result = uploadFile(files.get(i), path, zipFile, themePack, extractContent, isIE);
		}
		return result;
	}
	
	private boolean uploadFile(UploadFile file, String path, boolean zipFile, boolean themePack, boolean extractContent, boolean isIE) {
		if (file == null || path == null) {
			return false;
		}
		
		InputStream stream = getInputStream(file.getBytes());
		if (stream == null) {
			return false;
		}
		
		if (!path.endsWith(CoreConstants.SLASH)) {
			path = new StringBuffer(path).append(CoreConstants.SLASH).toString();
		}
		
		String name = file.getName();
		if (name.indexOf(CoreConstants.BACK_SLASH) != -1 && isIE) {
			name = name.substring(name.lastIndexOf(CoreConstants.BACK_SLASH) + 1);
		}
		
		try {
			if (zipFile && extractContent) {
				if (name.indexOf(CoreConstants.DOT) != -1) {
					name = name.substring(0, name.lastIndexOf(CoreConstants.DOT));
				}
				
				WebDAVUploadBean uploadBean = new WebDAVUploadBean();
				uploadBean.setUploadFilePath(path);
				uploadBean.uploadZipFile(themePack, name, stream, slide);
			}
			else {
				return slide.uploadFileAndCreateFoldersFromStringAsRoot(path, name, stream, file.getType(), true);
			}
		} catch (Exception e) {
			LOGGER.log(Level.SEVERE, "Error uploading file " + file.getName(), e);
			return false;
		} finally {
			closeStream(stream);
		}
		
		return true;
	}
	
	private InputStream getInputStream(byte[] bytes) {
		if (bytes == null) {
			return null;
		}
		
		InputStream stream = null;
		try {
			stream = new BufferedInputStream(new ByteArrayInputStream(bytes));
		} catch (Exception e) {
			LOGGER.log(Level.SEVERE, "Error getting InputStream", e);
			return null;
		}
		
		return stream;
	}
	
	private void closeStream(InputStream stream) {
		if (stream == null) {
			return;
		}
		
		try {
			stream.close();
		} catch (IOException e) {}
	}
	
	private BuilderService getBuilderService(IWContext iwc) {
		if (builder == null) {
			builder = getBuilderLogicWrapper().getBuilderService(iwc);
		}
		return builder;
	}
	
	private IWSlideService getSlideService(IWContext iwc) {
		if (slide == null) {
			try {
				slide = IBOLookup.getServiceInstance(iwc, IWSlideService.class);
			} catch (IBOLookupException e) {
				e.printStackTrace();
				return null;
			}
		}
		return slide;
	}

	public String getAddFileInputJavaScriptAction(String containerId, IWResourceBundle iwrb, boolean showProgressBar, boolean addjQuery,
			boolean autoAddFileInput, boolean autoUpload) {
		return new StringBuilder("addFileInputForUpload('").append(containerId).append("', '").append(iwrb.getLocalizedString("loading", "Loading..."))
				.append("', '").append(FILE_UPLOAD_INPUT_STYLE).append("', ").append(showProgressBar).append(", ").append(addjQuery).append(", ")
				.append(autoAddFileInput).append(", ").append(autoUpload).append(");").toString();
	}

	public String getRenderedComponent(String id) {
		if (StringUtil.isEmpty(id)) {
			return null;
		}
		IWContext iwc = CoreUtil.getIWContext();
		if (iwc == null) {
			return null;
		}
		BuilderService builderService = getBuilderService(iwc);
		if (builderService == null) {
			return null;
		}
		
		String currentPageId = null;
		WebContext webContext = WebContextFactory.get();
		if (webContext == null) {
			currentPageId = String.valueOf(iwc.getCurrentIBPageID());
		}
		else {
			currentPageId = builderService.getPageKeyByURI(webContext.getCurrentPage());
		}
		if (StringUtil.isEmpty(currentPageId)) {
			return null;
		}
		
		UIComponent component = builderService.findComponentInPage(iwc, currentPageId, id);
		if (component == null) {
			return null;
		}
		return builderService.getRenderedComponent(component, iwc, false);
	}
	
	public String getActionToLoadFilesAndExecuteCustomAction(String customAction, boolean showProgressBar, boolean addjQuery) {
		List<String> scripts = new ArrayList<String>();
		scripts.add(ContentUtil.getBundle().getVirtualPathWithFileNameString("javascript/FileUploadHelper.js"));
		scripts.add(getWeb2().getBundleURIToYUIScript());
		scripts.add(CoreConstants.DWR_ENGINE_SCRIPT);
		scripts.add(CoreConstants.DWR_UTIL_SCRIPT);
		scripts.add("/dwr/interface/FileUploader.js");
		scripts.add("/dwr/interface/FileUploadListener.js");
		if (addjQuery || showProgressBar) {
			scripts.add(getJQuery().getBundleURIToJQueryLib());
		}
		if (showProgressBar) {
			scripts.add(getJQuery().getBundleURIToJQueryPlugin(JQueryPlugin.PROGRESS_BAR));
		}
		
		return PresentationUtil.getJavaScriptLinesLoadedLazily(scripts, customAction);
	}
	
	public String getUploadAction(IWContext iwc, String id, String progressBarId, String uploadId, boolean showProgressBar, boolean showLoadingMessage,
			boolean zipFile, String formId, String actionAfterUpload, String actionAfterCounterReset, boolean autoUpload, boolean showUploadedFiles,
			String componentToRerenderId) {
		
		StringBuilder action = new StringBuilder(getPropertiesAction(iwc, id, progressBarId, uploadId, showProgressBar, showLoadingMessage, zipFile, formId,
				actionAfterUpload, actionAfterCounterReset, autoUpload, showUploadedFiles, componentToRerenderId)).append("FileUploadHelper.uploadFiles();");
		return getActionToLoadFilesAndExecuteCustomAction(action.toString(), showProgressBar, !StringUtil.isEmpty(componentToRerenderId));
	}
	
	private String getJavaScriptAction(String action) {
		if (action == null) {
			return "null";
		}
		
		StringBuffer script = new StringBuffer();
		if (action.indexOf("\"") != -1) {
			action = action.replaceAll("\"", "'");
		}
		if (action.indexOf("'") != -1) {
			String[] actionParts = action.split("'");
			StringBuffer changedAction = new StringBuffer();
			for (int i = 0; i < actionParts.length; i++) {
				changedAction.append(actionParts[i]);
				if ((i + 1) < actionParts.length) {
					changedAction.append("\\").append("'");
				}
			}
			action = changedAction.toString();
		}
		script.append("'").append(action).append("'");
		
		return script.toString();
	}

	public Web2Business getWeb2() {
		return web2;
	}

	public void setWeb2(Web2Business web2) {
		this.web2 = web2;
	}

	public JQuery getJQuery() {
		return jQuery;
	}

	public void setJQuery(JQuery query) {
		jQuery = query;
	}

	public String getUploadedFilesList(List<String> files, String uploadPath) {
		if (ListUtil.isEmpty(files) || StringUtil.isEmpty(uploadPath)) {
			return null;
		}
		
		IWContext iwc = CoreUtil.getIWContext();
		if (iwc == null) {
			return null;
		}
		
		IWSlideService slide = getSlideService(iwc);
		if (slide == null) {
			return null;
		}
		
		IWBundle bundle = ContentUtil.getBundle();
		IWResourceBundle iwrb = bundle.getResourceBundle(iwc);
		String deleteFileTitle = iwrb.getLocalizedString("files_uploader.delete_uploaded_file", "Delete file");
		
		Lists list = new Lists();
		for (String fileInSlide: files) {
			ListItem listItem = new ListItem();
			
			int index = fileInSlide.lastIndexOf(File.separator);
			if (index < 0 && iwc.isIE()) {
				index = fileInSlide.lastIndexOf("\\");
			}
			String fileName = fileInSlide;
			if (index > 0) {
				fileName = fileInSlide.substring(index + 1);
			}
			fileInSlide = new StringBuilder(CoreConstants.WEBDAV_SERVLET_URI).append(uploadPath).append(fileName).toString();
			listItem.add(new Link(fileName, fileInSlide));
			
			Image deleteFile = bundle.getImage("images/remove.png");
			deleteFile.setOnClick(new StringBuilder("FileUploadHelper.deleteUploadedFile('").append(listItem.getId()).append("', '")
					.append(fileInSlide.replaceFirst(CoreConstants.WEBDAV_SERVLET_URI, CoreConstants.EMPTY)).append("');").toString());
			deleteFile.setTitle(deleteFileTitle);
			deleteFile.setStyleClass("fileUploaderDeleteFileButtonStyle");
			listItem.add(deleteFile);
			
			list.add(listItem);
		}
		
		return getBuilderService(iwc).getRenderedComponent(list, iwc, false);
	}

	public AdvancedProperty deleteFile(String fileInSlide) {
		return deleteFile(CoreUtil.getIWContext(), fileInSlide);
	}
	
	private AdvancedProperty deleteFile(IWContext iwc, String fileInSlide) {
		String errorMessage = "Sorry, file can not be deleted!";
		AdvancedProperty result = new AdvancedProperty(Boolean.FALSE.toString(), errorMessage);
		
		if (iwc == null) {
			return result;
		}
		
		IWResourceBundle iwrb = ContentUtil.getBundle().getResourceBundle(iwc);
		errorMessage = iwrb.getLocalizedString("file_uploader.unable_delete_file", errorMessage);
		result.setValue(errorMessage);
		
		if (StringUtil.isEmpty(fileInSlide)) {
			return result;
		}
		
		if (fileInSlide.startsWith(CoreConstants.WEBDAV_SERVLET_URI)) {
			fileInSlide = fileInSlide.replaceFirst(CoreConstants.WEBDAV_SERVLET_URI, CoreConstants.EMPTY);
		}
		
		WebdavResource resource = getResource(iwc, fileInSlide);
		if (resource == null) {
			return result;
		}
		
		try {
			if (resource.deleteMethod()) {
				result.setId(Boolean.TRUE.toString());
				result.setValue(iwrb.getLocalizedString("file_uploader.success_deleting_file", "File was successfully deleted"));
			}
		} catch (Exception e) {
			LOGGER.log(Level.SEVERE, "Error deleting file: " + fileInSlide, e);
		}
		
		return result;
	}
	
	private WebdavResource getResource(IWContext iwc, String fileInSlide) {
		IWSlideService slide = getSlideService(iwc);
		if (slide == null) {
			return null;
		}
		
		UsernamePasswordCredentials crediantials = getUserCredentials(iwc, slide, fileInSlide);
		if (crediantials == null) {
			return null;
		}
		
		try {
			return slide.getWebdavResource(fileInSlide, crediantials);
		} catch (Exception e) {
			LOGGER.log(Level.SEVERE, "Error getting file: " + fileInSlide, e);
		}
		
		return null;
	}
	
	private UsernamePasswordCredentials getUserCredentials(IWContext iwc, IWSlideService slide, String fileInSlide) {
		if (fileInSlide.startsWith(CoreConstants.PUBLIC_PATH)) {
			//	File is in public folder, accessible for ALL users
			try {
				return slide.getRootUserCredentials();
			} catch (Exception e) {
				LOGGER.log(Level.SEVERE, "Error getting credentials for ROOT user", e);
				return null;
			}
		}
		
		if (!iwc.isLoggedOn()) {
			LOGGER.warning("User must be logged in!");
			return null;
		}
		
		User currentUser = iwc.getCurrentUser();
		if (currentUser == null) {
			return null;
		}
		
		IWSlideSession slideSession = null;
		try {
			slideSession = (IWSlideSession) IBOLookup.getSessionInstance(iwc, IWSlideSession.class);
		} catch(Exception e) {
			LOGGER.log(Level.SEVERE, "Error getting " + IWSlideSession.class, e);
		}
		if (slideSession == null) {
			return null;
		}
		
		try {
			return slideSession.getUserCredentials();
		} catch (Exception e) {
			LOGGER.log(Level.SEVERE, "Error getting credentials for: " + currentUser);
		}
		
		return null;
	}

	public BuilderLogicWrapper getBuilderLogicWrapper() {
		return builderLogicWrapper;
	}

	public void setBuilderLogicWrapper(BuilderLogicWrapper builderLogicWrapper) {
		this.builderLogicWrapper = builderLogicWrapper;
	}

	public AdvancedProperty deleteFiles(List<String> filesInSlide) {
		if (ListUtil.isEmpty(filesInSlide)) {
			return null;
		}
		
		IWContext iwc = CoreUtil.getIWContext();
		
		AdvancedProperty result = null;
		for (String fileInSlide: filesInSlide) {
			result = deleteFile(iwc, fileInSlide);
		}
		
		return result;
	}
	
}