package com.idega.content.upload.presentation;

import javax.faces.component.UIComponent;
import javax.faces.webapp.UIComponentELTag;

import com.idega.util.CoreConstants;
import com.idega.webface.WFUtil;

public class FileUploadViewerTag extends UIComponentELTag {
	
	private String actionAfterUpload, actionAfterCounterReset, actionAfterUploadedToRepository = null;
	private String uploadPath = CoreConstants.PUBLIC_PATH;
	private String formId, componentToRerenderId = null;
	
	private boolean zipFile = false;
	private boolean extractContent = false;
	private boolean themePack = false;
	private boolean showProgressBar = true;
	private boolean showLoadingMessage = true;
	private boolean allowMultipleFiles = false;
	private boolean autoAddFileInput = true;
	private boolean autoUpload, showUploadedFiles, fakeFileDeletion, stripNonRomanLetters;
	
	@Override
	public String getComponentType() {
		return FileUploadViewer.class.getSimpleName();
	}

	@Override
	public String getRendererType() {
		return null;
	}
	
	@Override
	protected void setProperties(UIComponent component) {
		if (component instanceof FileUploadViewer) {
			FileUploadViewer uploadViewer = (FileUploadViewer) component;
			super.setProperties(uploadViewer);
			
			uploadViewer.setActionAfterUpload(actionAfterUpload);
			uploadViewer.setActionAfterCounterReset(actionAfterCounterReset);
			uploadViewer.setActionAfterUploadedToRepository(actionAfterUploadedToRepository);
			uploadViewer.setUploadPath(uploadPath);
			uploadViewer.setFormId(formId);
			uploadViewer.setZipFile(zipFile);
			uploadViewer.setExtractContent(extractContent);
			uploadViewer.setThemePack(themePack);
			uploadViewer.setShowProgressBar(showProgressBar);
			uploadViewer.setShowLoadingMessage(showLoadingMessage);
			uploadViewer.setAllowMultipleFiles(allowMultipleFiles);
			uploadViewer.setAutoAddFileInput(autoAddFileInput);
			uploadViewer.setFakeFileDeletion(fakeFileDeletion);
			uploadViewer.setStripNonRomanLetters(stripNonRomanLetters);
		}
	}

	@Override
	public void release() {
		this.actionAfterUpload = null;
		this.actionAfterCounterReset = null;
		this.actionAfterUploadedToRepository = null;
		this.uploadPath = CoreConstants.PUBLIC_PATH;
		this.componentToRerenderId = null;
		
		this.zipFile = false;
		this.extractContent = false;
		this.themePack = false;
		this.showProgressBar = true;
		this.showLoadingMessage = true;
		this.allowMultipleFiles = false;
		this.autoAddFileInput = false;
		this.autoUpload = false;
		this.showUploadedFiles = false;
		this.fakeFileDeletion = false;
		this.stripNonRomanLetters = false;
	}
	
	public String getActionAfterUpload() {
		return actionAfterUpload;
	}

	public void setActionAfterUpload(String actionAfterUpload) {
		this.actionAfterUpload = actionAfterUpload;
	}

	public String getUploadPath() {
		return uploadPath;
	}

	public void setUploadPath(String uploadPath) {
		Object path = null;
		try {
			path = WFUtil.invoke(uploadPath);
		} catch(Exception e) {}
		if (path instanceof String) {
			uploadPath = (String) path;
		}
		
		this.uploadPath = uploadPath;
	}

	public boolean isZipFile() {
		return zipFile;
	}

	public void setZipFile(boolean zipFile) {
		this.zipFile = zipFile;
	}

	public boolean isExtractContent() {
		return extractContent;
	}

	public void setExtractContent(boolean extractContent) {
		this.extractContent = extractContent;
	}

	public boolean isThemePack() {
		return themePack;
	}

	public void setThemePack(boolean themePack) {
		this.themePack = themePack;
	}

	public boolean isShowProgressBar() {
		return showProgressBar;
	}

	public void setShowProgressBar(boolean showProgressBar) {
		this.showProgressBar = showProgressBar;
	}

	public boolean isShowLoadingMessage() {
		return showLoadingMessage;
	}

	public void setShowLoadingMessage(boolean showLoadingMessage) {
		this.showLoadingMessage = showLoadingMessage;
	}

	public boolean isAllowMultipleFiles() {
		return allowMultipleFiles;
	}

	public void setAllowMultipleFiles(boolean allowMultipleFiles) {
		this.allowMultipleFiles = allowMultipleFiles;
	}

	public String getFormId() {
		return formId;
	}

	public void setFormId(String formId) {
		this.formId = formId;
	}

	public String getActionAfterCounterReset() {
		return actionAfterCounterReset;
	}

	public void setActionAfterCounterReset(String actionAfterCounterReset) {
		this.actionAfterCounterReset = actionAfterCounterReset;
	}

	public boolean isAutoAddFileInput() {
		return autoAddFileInput;
	}

	public void setAutoAddFileInput(boolean autoAddFileInput) {
		this.autoAddFileInput = autoAddFileInput;
	}

	public String getComponentToRerenderId() {
		return componentToRerenderId;
	}

	public void setComponentToRerenderId(String componentToRerenderId) {
		this.componentToRerenderId = componentToRerenderId;
	}

	public boolean isAutoUpload() {
		return autoUpload;
	}

	public void setAutoUpload(boolean autoUpload) {
		this.autoUpload = autoUpload;
	}

	public boolean isShowUploadedFiles() {
		return showUploadedFiles;
	}

	public void setShowUploadedFiles(boolean showUploadedFiles) {
		this.showUploadedFiles = showUploadedFiles;
	}

	public boolean isFakeFileDeletion() {
		return fakeFileDeletion;
	}

	public void setFakeFileDeletion(boolean fakeFileDeletion) {
		this.fakeFileDeletion = fakeFileDeletion;
	}

	public String getActionAfterUploadedToRepository() {
		return actionAfterUploadedToRepository;
	}

	public void setActionAfterUploadedToRepository(String actionAfterUploadedToRepository) {
		this.actionAfterUploadedToRepository = actionAfterUploadedToRepository;
	}

	public boolean isStripNonRomanLetters() {
		return stripNonRomanLetters;
	}

	public void setStripNonRomanLetters(boolean stripNonRomanLetters) {
		this.stripNonRomanLetters = stripNonRomanLetters;
	}
	
}