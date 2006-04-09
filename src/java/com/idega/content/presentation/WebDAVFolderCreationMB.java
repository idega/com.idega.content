package com.idega.content.presentation;

/**
 * @author gimmi
 */
public class WebDAVFolderCreationMB {
	
	String folderName = new String();
	Boolean folderCreated = null;
	String errorMessage = null;
	
	public String getErrorMessage() {
		return this.errorMessage;
	}
	
	public void setErrorMessage(String errorMessage) {
		this.errorMessage = errorMessage;
	}
	
	public Boolean getFolderCreated() {
		return this.folderCreated;
	}
	
	public void setFolderCreated(Boolean folderCreated) {
		this.folderCreated = folderCreated;
	}
	
	public String getFolderName() {
		return this.folderName;
	}
	
	public void setFolderName(String folderName) {
		this.folderName = folderName;
	}
}
