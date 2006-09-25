package com.idega.content.presentation;

import javax.faces.component.UIComponent;
import javax.faces.webapp.UIComponentTag;

public class WebDAVUploadTag extends UIComponentTag {
	
	private String uploadMethod = null;
	
	private String styleClassSelectFile = null;
	
	private String styleClassFileUploadInput = null;
	
	private String accept = null;
	
	private String storage = null;
	
	private String styleClassGiveName = null;
	
	private String styleClassVersionText = null;
	
	private String styleClassFolder = null;
	
	private String fileLinkTarget = null;
	
	private String styleClassButton = null;
	
	private String styleClassWFContainerLine = null;
	
	private boolean useFileName;
	
	private boolean useVersionComment;
	
	private boolean useUploadPath;
	
	private boolean useFileLink;
	
	private boolean useImagePreview;

	private String pathProviderBeanWithMethod;
	
	private String onClickAction;
	
	public String getComponentType() {
		return "WebDAVUpload";
	}
	
	public String getRendererType() {
		return null;
	}

	public String getUploadMethod() {
		return uploadMethod;
	}

	public void setUploadMethod(String uploadMethod) {
		this.uploadMethod = uploadMethod;
	}
	
	public String getAccept() {
		return accept;
	}

	public void setAccept(String accept) {
		this.accept = accept;
	}

	public String getFileLinkTarget() {
		return fileLinkTarget;
	}

	public void setFileLinkTarget(String fileLinkTarget) {
		this.fileLinkTarget = fileLinkTarget;
	}

	public String getStyleClassButton() {
		return styleClassButton;
	}

	public void setStyleClassButton(String styleClassButton) {
		this.styleClassButton = styleClassButton;
	}

	public String getStyleClassFileUploadInput() {
		return styleClassFileUploadInput;
	}

	public void setStyleClassFileUploadInput(String styleClassFileUploadInput) {
		this.styleClassFileUploadInput = styleClassFileUploadInput;
	}

	public String getStyleClassFolder() {
		return styleClassFolder;
	}

	public void setStyleClassFolder(String styleClassFolder) {
		this.styleClassFolder = styleClassFolder;
	}

	public String getStyleClassGiveName() {
		return styleClassGiveName;
	}

	public void setStyleClassGiveName(String styleClassGiveName) {
		this.styleClassGiveName = styleClassGiveName;
	}

	public String getStyleClassSelectFile() {
		return styleClassSelectFile;
	}

	public void setStyleClassSelectFile(String styleClassSelectFile) {
		this.styleClassSelectFile = styleClassSelectFile;
	}

	public String getStyleClassVersionText() {
		return styleClassVersionText;
	}

	public void setStyleClassVersionText(String styleClassVersionText) {
		this.styleClassVersionText = styleClassVersionText;
	}

	public String getStyleClassWFContainerLine() {
		return styleClassWFContainerLine;
	}

	public void setStyleClassWFContainerLine(String styleClassWFContainerLine) {
		this.styleClassWFContainerLine = styleClassWFContainerLine;
	}

	public String getStorage() {
		return storage;
	}

	public void setStorage(String storage) {
		this.storage = storage;
	}

	public boolean isUseFileLink() {
		return useFileLink;
	}

	public void setUseFileLink(boolean useFileLink) {
		this.useFileLink = useFileLink;
	}

	public boolean isUseFileName() {
		return useFileName;
	}

	public void setUseFileName(boolean useFileName) {
		this.useFileName = useFileName;
	}

	public boolean isUseImagePreview() {
		return useImagePreview;
	}

	public void setUseImagePreview(boolean useImagePreview) {
		this.useImagePreview = useImagePreview;
	}

	public boolean isUseUploadPath() {
		return useUploadPath;
	}

	public void setUseUploadPath(boolean useUploadPath) {
		this.useUploadPath = useUploadPath;
	}

	public boolean isUseVersionComment() {
		return useVersionComment;
	}

	public void setUseVersionComment(boolean useVersionComment) {
		this.useVersionComment = useVersionComment;
	}

	public String getPathProviderBeanWithMethod() {
		return pathProviderBeanWithMethod;
	}

	public void setPathProviderBeanWithMethod(String fileUploadFolder) {
		this.pathProviderBeanWithMethod = fileUploadFolder;
	}

	public String getOnClickAction() {
		return onClickAction;
	}

	public void setOnClickAction(String onClickAction) {
		this.onClickAction = onClickAction;
	}

	public void release() {   
		super.release();      
		this.uploadMethod = null;
		this.styleClassSelectFile = null;
		this.styleClassFileUploadInput = null;
		this.accept = null;
		this.storage = null;
		this.styleClassGiveName = null;
		this.styleClassVersionText = null;
		this.styleClassFolder = null;
		this.fileLinkTarget = null;
		this.styleClassButton = null;
		this.styleClassWFContainerLine = null;
		this.useFileName = false;
		this.useVersionComment = false;
		this.useUploadPath = false;
		this.useFileLink = false;
		this.useImagePreview = false;
		this.pathProviderBeanWithMethod = null;
		this.onClickAction = null;
	}

	protected void setProperties(UIComponent component) {      
		if (component instanceof WebDAVUpload) {
			super.setProperties(component);
			
			WebDAVUpload upload = (WebDAVUpload) component;
			upload.setUploadMethod(uploadMethod);
			upload.setStorage(storage);
			upload.setStyleClassButton(styleClassButton);
			upload.setStyleClassFileUploadInput(styleClassFileUploadInput);
			upload.setStyleClassFolder(styleClassFolder);
			upload.setStyleClassGiveName(styleClassGiveName);
			upload.setStyleClassSelectFile(styleClassSelectFile);
			upload.setStyleClassVersionText(styleClassVersionText);
			upload.setStyleClassWFContainerLine(styleClassWFContainerLine);
			upload.setAccept(accept);
			upload.setStorage(storage);
			upload.setFileLinkTarget(fileLinkTarget);
			upload.setUseFileName(useFileName);
			upload.setUseVersionComment(useVersionComment);
			upload.setUseUploadPath(useUploadPath);
			upload.setUseFileLink(useFileLink);
			upload.setUseImagePreview(useImagePreview);
			upload.setPathProviderBeanWithMethod(pathProviderBeanWithMethod);
			upload.setOnClickAction(onClickAction);
		}
	}

}