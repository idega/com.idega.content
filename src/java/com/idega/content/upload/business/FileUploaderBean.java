package com.idega.content.upload.business;

import java.io.BufferedInputStream;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.faces.component.UIComponent;

import org.directwebremoting.WebContext;
import org.directwebremoting.WebContextFactory;
import org.jdom2.Document;
import org.springframework.beans.factory.annotation.Autowired;

import com.idega.block.web2.business.JQuery;
import com.idega.block.web2.business.JQueryPlugin;
import com.idega.block.web2.business.Web2Business;
import com.idega.builder.bean.AdvancedProperty;
import com.idega.builder.business.BuilderLogicWrapper;
import com.idega.content.business.ContentConstants;
import com.idega.content.business.ContentUtil;
import com.idega.content.business.WebDAVUploadBean;
import com.idega.content.presentation.WebDAVListManagedBean;
import com.idega.content.repository.download.RepositoryItemDownloader;
import com.idega.content.upload.bean.UploadFile;
import com.idega.core.builder.business.BuilderService;
import com.idega.core.business.DefaultSpringBean;
import com.idega.idegaweb.IWBundle;
import com.idega.idegaweb.IWResourceBundle;
import com.idega.presentation.IWContext;
import com.idega.presentation.Image;
import com.idega.presentation.Layer;
import com.idega.presentation.text.DownloadLink;
import com.idega.presentation.text.ListItem;
import com.idega.presentation.text.Lists;
import com.idega.presentation.text.Text;
import com.idega.presentation.ui.FileInput;
import com.idega.repository.bean.RepositoryItem;
import com.idega.user.data.bean.User;
import com.idega.util.CoreConstants;
import com.idega.util.CoreUtil;
import com.idega.util.IOUtil;
import com.idega.util.ListUtil;
import com.idega.util.PresentationUtil;
import com.idega.util.StringHandler;
import com.idega.util.StringUtil;
import com.idega.util.expression.ELUtil;

public class FileUploaderBean extends DefaultSpringBean implements FileUploader {

	private static final Logger LOGGER = Logger.getLogger(FileUploaderBean.class.getName());

	private BuilderService builder = null;

	@Autowired
	private BuilderLogicWrapper builderLogicWrapper;

	@Autowired
	private Web2Business web2;

	@Autowired
	private JQuery jQuery;

	public static final String FILE_UPLOAD_INPUT_STYLE = "fileUploadInputStyle";

	@Override
	public void initializeUploader(IWContext iwc) {
		if (iwc == null) {
			return;
		}

		if (builder == null)
			getBuilderService(iwc);
	}

	@Override
	public Layer getFileInput(IWContext iwc, String id, boolean addRemoveImage, boolean showProgressBar, boolean addjQuery, boolean autoAddFileInput,
			boolean allowMultiple, boolean autoUpload, String style) {
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
		input.setMultiple(allowMultiple || autoAddFileInput);
		if (autoUpload) {
			input.setOnChange(getActionToLoadFilesAndExecuteCustomAction("FileUploadHelper.uploadFiles();", showProgressBar, addjQuery));
		}
		if (!StringUtil.isEmpty(style)) {
			fileInputContainer.setStyleAttribute(style);
			input.setStyleAttribute(style);
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

	@Override
	public String getPropertiesAction(IWContext iwc, String id, String progressBarId, String uploadId, boolean showProgressBar, boolean showLoadingMessage,
			boolean zipFile, String formId, String actionAfterUpload, String actionAfterCounterReset, boolean autoUpload, boolean showUploadedFiles,
			String componentToRerenderId, boolean fakeFileDeletion, String actionAfterUploadedToRepository, boolean stripNonRomanLetters, String maxUploadSize) {

		IWBundle bundle = ContentUtil.getBundle();
		IWResourceBundle iwrb = bundle.getResourceBundle(iwc);
		return new StringBuilder("FileUploadHelper.setProperties({id: '").append(id).append("', showProgressBar: ").append(showProgressBar)
			.append(", showMessage: ").append(showLoadingMessage).append(", zipFile: ").append(zipFile).append(", formId: '").append(formId)
			.append("', progressBarId: '").append(progressBarId).append("', ")
			.append("localizations: {UPLOADING_FILE_PROGRESS_BOX_TEXT: '").append(iwrb.getLocalizedString("uploading_file", "Uploading file"))
			.append("', UPLOADING_FILE_PLEASE_WAIT_PROGRESS_BOX_TEXT: '").append(iwrb.getLocalizedString("completed_please_wait", "completed, please wait..."))
			.append("', UPLOADING_FILE_PROGRESS_BOX_FILE_UPLOADED_TEXT: '")
				.append(iwrb.getLocalizedString("upload_was_successfully_finished", "Upload was successfully finished."))
			.append("', UPLOADING_FILE_MESSAGE: '").append(iwrb.getLocalizedString("uploading", "Uploading..."))
			.append("', UPLOADING_FILE_INVALID_TYPE_MESSAGE: '")
				.append(iwrb.getLocalizedString("incorrect_file_type", "Unsupported file type! Only zip files allowed"))
			.append("', UPLOADING_FILE_FAILED: '")
				.append(iwrb.getLocalizedString("uploading_file_failed_msg", "Sorry, some error occurred - unable to upload file(s). Please, try again"))
			.append("', UPLOADING_FILE_EXCEEDED_SIZE: '")
				.append(iwrb.getLocalizedString("upload_exceeding_limits", "Sorry, the size of selected file(s) is exceeding the max allowed size"))
			.append("', CHOOSE_FILE: '")
				.append(iwrb.getLocalizedString("choose_file", "Choose file"))
			.append("', UPLOAD_FILE: '")
				.append(iwrb.getLocalizedString("upload_file", "Upload file"))
			.append("', FILES_SELECTED: '")
				.append(iwrb.getLocalizedString("web2_uploader.files_selected", "files selected"))
			.append("', SELECTED_FILE: '")
				.append(iwrb.getLocalizedString("web2_uploader.selected_file", "Selected file"))
			.append("', FLASH_IS_MISSING: '")
				.append(iwrb.getLocalizedString("web2_uploader.flash_is_missing", "Unable to upload file(s): you need to install Flash plug-in"))
			.append("', LOADING: '")
				.append(iwrb.getLocalizedString("loading", "Loading..."))
			.append("', MOVING_DATA_INTO_THE_PLACE: '")
				.append(iwrb.getLocalizedString("preparing_data", "Preparing data..."))
			.append("'}, ")
			.append("actionAfterUpload: ").append(getJavaScriptAction(actionAfterUpload))
			.append(", actionAfterCounterReset: ").append(getJavaScriptAction(actionAfterCounterReset))
			.append(", uploadId: '").append(uploadId).append("', autoUpload: ").append(autoUpload).append(", showUploadedFiles: ").append(showUploadedFiles)
			.append(", fakeFileDeletion: ").append(fakeFileDeletion)
			.append(", actionAfterUploadedToRepository: ").append(getJavaScriptAction(actionAfterUploadedToRepository))
			.append(", stripNonRomanLetters: ").append(stripNonRomanLetters)
			.append(", maxSize: ").append(maxUploadSize)
			.append(", uploadImage: '").append(bundle.getVirtualPathWithFileNameString("images/upload.png")).append("'")
			.append(", sessionId: '").append(iwc.getSession().getId()).append("'")
			.append(", swfObject: '").append(web2.getSWFUploadObjectScript()).append("'")
			.append(", swfUploadScript: '").append(web2.getSWFUploadScript()).append("'")
			.append(", swfUploadPlugin: '").append(web2.getSWFUploadPlugin()).append("'")
			.append(", needFlash: false")
			.append(", initializeScriptsAction: function() {").append(getActionToLoadFilesAndExecuteCustomAction("FileUploadHelper.initializeFlashUploader();",
					showProgressBar, true)).append("}")
		.append("});").toString();
	}

	@Override
	public Document getRenderedFileInput(String id, Boolean showProgressBar,
			Boolean addjQuery, Boolean autoAddFileInput, Boolean allowMultipleFiles,
			Boolean autoUpload, String style) {
		if (autoUpload == null) {
			autoUpload = Boolean.FALSE;
		}

		if (allowMultipleFiles == null) {
			allowMultipleFiles = Boolean.FALSE;
		}

		if (showProgressBar == null) {
			showProgressBar = Boolean.FALSE;
		}

		if (addjQuery == null) {
			addjQuery = Boolean.FALSE;
		}

		if (autoAddFileInput == null) {
			autoAddFileInput = Boolean.FALSE;
		}

		IWContext iwc = CoreUtil.getIWContext();
		if (iwc == null) {
			return null;
		}

		BuilderService builder = getBuilderService(iwc);
		if (builder == null) {
			return null;
		}

		return builder.getRenderedComponent(iwc, getFileInput(iwc, id, true, showProgressBar, addjQuery, autoAddFileInput, allowMultipleFiles,
				autoUpload, style), false);
	}

	@Override
	public boolean uploadFile(List<UploadFile> files, String uploadPath, boolean isIE) {
		return doUploading(files, uploadPath, false, false, false, isIE);
	}

	@Override
	public boolean uploadThemePack(List<UploadFile> files, String uploadPath, boolean isIE) {
		return doUploading(files, uploadPath, true, true, true, isIE);
	}

	@Override
	public boolean uploadZipFile(List<UploadFile> files, String uploadPath, boolean extractContent, boolean isIE) {
		return doUploading(files, uploadPath, true, false, extractContent, isIE);
	}

	private boolean doUploading(List<UploadFile> files, String path, boolean zipFile, boolean themePack, boolean extractContent, boolean isIE) {
		if (files == null || path == null) {
			return false;
		}

		boolean uploadedSuccessfully = true;
		for (Iterator<UploadFile> filesIter = files.iterator(); (filesIter.hasNext() && uploadedSuccessfully);) {
			uploadedSuccessfully = uploadFile(filesIter.next(), path, zipFile, themePack, extractContent, isIE);
		}
		return uploadedSuccessfully;
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
				return uploadBean.uploadZipFile(themePack, name, stream);
			} else {
				return getRepositoryService().uploadFile(path, name, file.getType(), stream);
			}
		} catch (Exception e) {
			String message = "Error uploading file " + file.getName();
			LOGGER.log(Level.SEVERE, message, e);
			CoreUtil.sendExceptionNotification(message, e);
		} finally {
			IOUtil.close(stream);
		}
		return false;
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

	private BuilderService getBuilderService(IWContext iwc) {
		if (builder == null) {
			builder = getBuilderLogicWrapper().getBuilderService(iwc);
		}
		return builder;
	}

	@Override
	public String getAddFileInputJavaScriptAction(String containerId, IWResourceBundle iwrb, boolean showProgressBar, boolean addjQuery,
			boolean autoAddFileInput, boolean autoUpload) {
		return new StringBuilder("addFileInputForUpload('").append(containerId).append("', '").append(iwrb.getLocalizedString("loading", "Loading..."))
				.append("', '").append(FILE_UPLOAD_INPUT_STYLE).append("', ").append(showProgressBar).append(", ").append(addjQuery).append(", ")
				.append(autoAddFileInput).append(", ").append(autoUpload).append(");").toString();
	}

	@Override
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

	@Override
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

	@Override
	public String getUploadAction(IWContext iwc, String id, String progressBarId, String uploadId, boolean showProgressBar, boolean showLoadingMessage,
			boolean zipFile, String formId, String actionAfterUpload, String actionAfterCounterReset, boolean autoUpload, boolean showUploadedFiles,
			String componentToRerenderId, boolean fakeFileDeletion, String actionAfterUploadedToRepository, boolean stripNonRomanLetters, String maxUploadSize) {

		StringBuilder action = new StringBuilder(getPropertiesAction(iwc, id, progressBarId, uploadId, showProgressBar, showLoadingMessage, zipFile, formId,
				actionAfterUpload, actionAfterCounterReset, autoUpload, showUploadedFiles, componentToRerenderId, fakeFileDeletion, actionAfterUploadedToRepository,
				stripNonRomanLetters, maxUploadSize)).append("FileUploadHelper.uploadFiles();");
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

	@Override
	public List<String> getUploadedFilesListById(String uploadId, String uploadPath, Boolean fakeFileDeletion, Boolean stripNonRomanLetters) {
		FileUploadProgressListener fileUploadProgress = ELUtil.getInstance().getBean(FileUploadProgressListener.class);
		if (fileUploadProgress == null)
			return null;

		Collection<AdvancedProperty> files = fileUploadProgress.getUploadedFiles(uploadId);
		if (ListUtil.isEmpty(files))
			return null;

		fakeFileDeletion = fakeFileDeletion == null ? Boolean.FALSE : fakeFileDeletion;
		stripNonRomanLetters = stripNonRomanLetters == null ? Boolean.FALSE : stripNonRomanLetters;

		IWContext iwc = CoreUtil.getIWContext();
		if (iwc == null) {
			return null;
		}

		IWBundle bundle = ContentUtil.getBundle();
		IWResourceBundle iwrb = bundle.getResourceBundle(iwc);
		String deleteFileTitle = iwrb.getLocalizedString("files_uploader.delete_uploaded_file", "Delete file");

		List<String> results = new ArrayList<String>(files.size() + 1);

		Lists list = new Lists();
		for (AdvancedProperty file: files) {

			String fileName = file.getValue();
			
			ListItem listItem = new ListItem();

			fileName = stripNonRomanLetters ? StringHandler.stripNonRomanCharacters(fileName, ContentConstants.UPLOADER_EXCEPTIONS_FOR_LETTERS) : fileName;
			String fileInRepository = new StringBuilder(CoreConstants.WEBDAV_SERVLET_URI).append(file.getId()).append(fileName).toString();
			fileInRepository = stripNonRomanLetters ? StringHandler.stripNonRomanCharacters(fileInRepository, ContentConstants.UPLOADER_EXCEPTIONS_FOR_LETTERS) : fileInRepository;
			String fileInRepositoryWithoutWebDav = fileInRepository.replaceFirst(CoreConstants.WEBDAV_SERVLET_URI, CoreConstants.EMPTY);
			results.add(fileInRepositoryWithoutWebDav);

			DownloadLink link = new DownloadLink(new Text(fileName));
			link.setMediaWriterClass(RepositoryItemDownloader.class);
			link.setParameter(WebDAVListManagedBean.PARAMETER_WEB_DAV_URL, fileInRepository);
			link.setParameter("allowAnonymous", Boolean.TRUE.toString());
			listItem.add(link);

			Image deleteFile = bundle.getImage("images/remove.png");
			deleteFile.setOnClick(new StringBuilder("FileUploadHelper.deleteUploadedFile('").append(listItem.getId()).append("', '")
					.append(fileInRepositoryWithoutWebDav).append("', '").append(uploadId).append("', ").append(fakeFileDeletion)
					.append(");").toString());
			deleteFile.setTitle(deleteFileTitle);
			deleteFile.setStyleClass("fileUploaderDeleteFileButtonStyle");
			listItem.add(deleteFile);

			list.add(listItem);
		}

		results.add(getBuilderService(iwc).getRenderedComponent(list, iwc, false));
		return results;
	}

	@Override
	public List<String> getUploadedFilesList(List<String> files, String uploadPath, Boolean fakeFileDeletion, Boolean stripNonRomanLetters, String uploadId) {
		if (ListUtil.isEmpty(files) || StringUtil.isEmpty(uploadPath)) {
			return null;
		}

		fakeFileDeletion = fakeFileDeletion == null ? Boolean.FALSE : fakeFileDeletion;
		stripNonRomanLetters = stripNonRomanLetters == null ? Boolean.FALSE : stripNonRomanLetters;

		IWContext iwc = CoreUtil.getIWContext();
		if (iwc == null) {
			return null;
		}

		IWBundle bundle = ContentUtil.getBundle();
		IWResourceBundle iwrb = bundle.getResourceBundle(iwc);
		String deleteFileTitle = iwrb.getLocalizedString("files_uploader.delete_uploaded_file", "Delete file");

		List<String> results = new ArrayList<String>(files.size() + 1);

		Lists list = new Lists();
		for (String fileInRepository: files) {
			if (StringUtil.isEmpty(fileInRepository) || fileInRepository.indexOf(CoreConstants.DOT) == -1)
				continue;

			ListItem listItem = new ListItem();

			int index = fileInRepository.lastIndexOf(File.separator);
			if (index < 0 && iwc.isIE()) {
				index = fileInRepository.lastIndexOf("\\");
			}
			String fileName = fileInRepository;
			if (index > 0) {
				fileName = fileInRepository.substring(index + 1);
			}

			if (!uploadPath.endsWith(CoreConstants.SLASH)) {
				uploadPath = uploadPath.concat(CoreConstants.SLASH);
			}
			fileName = stripNonRomanLetters ? StringHandler.stripNonRomanCharacters(fileName, ContentConstants.UPLOADER_EXCEPTIONS_FOR_LETTERS) : fileName;
			fileInRepository = new StringBuilder(CoreConstants.WEBDAV_SERVLET_URI).append(uploadPath).append(fileName).toString();
			fileInRepository = stripNonRomanLetters ? StringHandler.stripNonRomanCharacters(fileInRepository, ContentConstants.UPLOADER_EXCEPTIONS_FOR_LETTERS) : fileInRepository;
			String fileInRepositoryWithoutWebDav = fileInRepository.replaceFirst(CoreConstants.WEBDAV_SERVLET_URI, CoreConstants.EMPTY);
			results.add(fileInRepositoryWithoutWebDav);

			DownloadLink link = new DownloadLink(new Text(fileName));
			link.setMediaWriterClass(RepositoryItemDownloader.class);
			link.setParameter(WebDAVListManagedBean.PARAMETER_WEB_DAV_URL, fileInRepository);
			link.setParameter("allowAnonymous", Boolean.TRUE.toString());
			listItem.add(link);

			Image deleteFile = bundle.getImage("images/remove.png");
			deleteFile.setOnClick(new StringBuilder("FileUploadHelper.deleteUploadedFile('").append(listItem.getId()).append("', '")
					.append(fileInRepositoryWithoutWebDav).append("', '").append(uploadId).append("', ").append(fakeFileDeletion)
					.append(");").toString());
			deleteFile.setTitle(deleteFileTitle);
			deleteFile.setStyleClass("fileUploaderDeleteFileButtonStyle");
			listItem.add(deleteFile);

			list.add(listItem);
		}

		results.add(getBuilderService(iwc).getRenderedComponent(list, iwc, false));
		return results;
	}

	@Override
	public AdvancedProperty deleteFile(String fileInRepository, String uploadId, Boolean fakeFileDeletion) {
		return deleteFile(CoreUtil.getIWContext(), fileInRepository, uploadId, fakeFileDeletion == null ? Boolean.FALSE : fakeFileDeletion);
	}

	private AdvancedProperty deleteFile(IWContext iwc, String fileInRepository, String uploadId, boolean fakeFileDeletion) {
		String message = fakeFileDeletion ? "File was successfully deleted" : "Sorry, file can not be deleted!" ;
		AdvancedProperty result = new AdvancedProperty(fakeFileDeletion ? Boolean.TRUE.toString() : Boolean.FALSE.toString(), message);

		if (iwc == null) {
			return result;
		}

		IWResourceBundle iwrb = ContentUtil.getBundle().getResourceBundle(iwc);
		message = iwrb.getLocalizedString(fakeFileDeletion ? "file_uploader.success_deleting_file" : "file_uploader.unable_delete_file", message);
		result.setValue(message);

		if (fakeFileDeletion){
			FileUploadProgressListener fileUploadProgressListener = ELUtil.getInstance().getBean(FileUploadProgressListener.class);
			ArrayList<String> filesInRepo = new ArrayList<String>();
			filesInRepo.add(fileInRepository);
			fileUploadProgressListener.removeUploadedFiles(uploadId, filesInRepo);
		}
		
		if (StringUtil.isEmpty(fileInRepository)) {
			return result;
		}

		if (fileInRepository.startsWith(CoreConstants.WEBDAV_SERVLET_URI)) {
			fileInRepository = fileInRepository.replaceFirst(CoreConstants.WEBDAV_SERVLET_URI, CoreConstants.EMPTY);
		}

		RepositoryItem resource = getResource(iwc, fileInRepository, fakeFileDeletion);
		if (resource == null)
			return result;

		try {
			if (resource.delete()) {
				result.setId(Boolean.TRUE.toString());
				result.setValue(iwrb.getLocalizedString("file_uploader.success_deleting_file", "File was successfully deleted"));
				
				FileUploadProgressListener fileUploadProgressListener = ELUtil.getInstance().getBean(FileUploadProgressListener.class);
				ArrayList<String> filesInRepo = new ArrayList<String>();
				filesInRepo.add(fileInRepository);
				fileUploadProgressListener.removeUploadedFiles(uploadId, filesInRepo);
			}
		} catch (Exception e) {
			LOGGER.log(Level.SEVERE, "Error deleting file: " + fileInRepository, e);
		}

		return result;
	}

	private RepositoryItem getResource(IWContext iwc, String fileInRepository, boolean fakeFileDeletion) {
		try {
			User currentUser = getCurrentUser();
			return currentUser == null && fakeFileDeletion ?	getRepositoryService().getRepositoryItemAsRootUser(fileInRepository) :
																getRepositoryService().getRepositoryItem(currentUser, fileInRepository);
		} catch (Exception e) {
			LOGGER.log(Level.SEVERE, "Error getting file: " + fileInRepository, e);
		}

		return null;
	}

	public BuilderLogicWrapper getBuilderLogicWrapper() {
		return builderLogicWrapper;
	}

	public void setBuilderLogicWrapper(BuilderLogicWrapper builderLogicWrapper) {
		this.builderLogicWrapper = builderLogicWrapper;
	}

	@Override
	public AdvancedProperty deleteFiles(List<String> filesInRepository, String uploadId, Boolean fakeFileDeletion) {
		if (ListUtil.isEmpty(filesInRepository)) {
			return null;
		}

		fakeFileDeletion = fakeFileDeletion == null ? Boolean.FALSE : fakeFileDeletion;

		IWContext iwc = CoreUtil.getIWContext();

		AdvancedProperty result = null;
		for (String fileInRepository: filesInRepository) {
			result = deleteFile(iwc, fileInRepository, uploadId,fakeFileDeletion);
		}

		return result;
	}

	@Override
	public Boolean addPreviouslyUploadedFiles(String uploadId, String oldUploadId) {
		if ((uploadId==null) || (oldUploadId==null)) {
			return false;
		}

		FileUploadProgressListener fileUploadProgressListener = ELUtil.getInstance().getBean(FileUploadProgressListener.class);
		fileUploadProgressListener.appendFiles(uploadId, oldUploadId);
		return true;
	}

}