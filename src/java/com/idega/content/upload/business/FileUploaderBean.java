package com.idega.content.upload.business;

import java.io.BufferedInputStream;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.List;

import org.jdom.Document;

import com.idega.business.IBOLookup;
import com.idega.business.IBOLookupException;
import com.idega.content.business.ContentConstants;
import com.idega.content.business.WebDAVUploadBean;
import com.idega.content.upload.bean.UploadFile;
import com.idega.content.upload.presentation.FileUploadViewer;
import com.idega.core.builder.business.BuilderService;
import com.idega.core.builder.business.BuilderServiceFactory;
import com.idega.idegaweb.IWBundle;
import com.idega.idegaweb.IWResourceBundle;
import com.idega.presentation.IWContext;
import com.idega.presentation.Image;
import com.idega.presentation.Layer;
import com.idega.presentation.ui.FileInput;
import com.idega.slide.business.IWSlideService;
import com.idega.util.CoreConstants;
import com.idega.util.CoreUtil;

public class FileUploaderBean implements FileUploader {

	private static final long serialVersionUID = 896091693178092417L;
	
	private BuilderService builder = null;
	private IWSlideService slide = null;
	
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
	
	public Layer getFileInput(IWContext iwc, String id, boolean addRemoveImage) {
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
		input.setStyleClass("fileUploadInputStyle");
		input.setName(ContentConstants.UPLOAD_FIELD_NAME);
		fileInputContainer.add(input);
		input.setOnChange(FileUploadViewer.getActionToLoadFilesAndExecuteCustomAction(getAddFileInputJavaScriptAction(id, iwrb)));
		
		if (addRemoveImage) {
			Image remove = new Image(bundle.getVirtualPathWithFileNameString("images/delete.png"), name, 18, 18);
			remove.setStyleClass("removeFileUploadInputImageStyle");
			remove.setOnClick(new StringBuffer("removeFileInput('").append(fileInputContainer.getId()).append("', '").append(message).append("');").toString());
			fileInputContainer.add(remove);
		}
		
		return fileInputContainer;
	}
	
	public Document getRenderedFileInput(String id) {
		IWContext iwc = CoreUtil.getIWContext();
		if (iwc == null) {
			return null;
		}
		
		BuilderService builder = getBuilderService(iwc);
		if (builder == null) {
			return null;
		}
		
		return builder.getRenderedComponent(iwc, getFileInput(iwc, id, true), false);
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
			e.printStackTrace();
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
			e.printStackTrace();
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
			try {
				builder = BuilderServiceFactory.getBuilderService(iwc);
			} catch (Exception e) {
				e.printStackTrace();
				return null;
			}
		}
		return builder;
	}
	
	private IWSlideService getSlideService(IWContext iwc) {
		if (slide == null) {
			try {
				slide = (IWSlideService) IBOLookup.getServiceInstance(iwc, IWSlideService.class);
			} catch (IBOLookupException e) {
				e.printStackTrace();
				return null;
			}
		}
		return slide;
	}

	public String getAddFileInputJavaScriptAction(String containerId, IWResourceBundle iwrb) {
		return new StringBuilder("addFileInputForUpload('").append(containerId).append("', '").append(iwrb.getLocalizedString("loading", "Loading..."))
				.append("');").toString();
	}
}