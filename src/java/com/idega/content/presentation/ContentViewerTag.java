package com.idega.content.presentation;

import javax.faces.component.UIComponent;
import javax.faces.webapp.UIComponentTag;

/**
 * @author gimmi
 */
public class ContentViewerTag extends UIComponentTag {

	private String rootFolder;
	private String startFolder;
	private boolean useUserHomeFolder;
	private String iconTheme;
	private boolean showFolders = true;
	private boolean showPublicFolder = true;
	private boolean showDropboxFolder = true;
	private String columnsToHide;
	private boolean useVersionControl = true;
	private boolean showPermissionTab = true;
	private boolean showUploadComponent = true;
	private String onFileClickEvent;
	
	public void setRootPath(String root) {
		this.rootFolder = root;
	}
	
	public String getRootPath() {
		return this.rootFolder;
	}
	
	public void setStartPath(String start) {
		this.startFolder = start;
	}
	
	public String getStartPath() {
		return this.startFolder;
	}
	
	public void setUseUserHomeFolder(boolean useUserFolder) {
		this.useUserHomeFolder = useUserFolder;
	}
	
	public void setIconTheme(String themeName) {
		this.iconTheme = themeName;
	}
	
	public String getIconTheme() {
		return this.iconTheme;
	}
	
	public void setShowFolders(boolean showFolders) {
		this.showFolders = showFolders;
	}
	
	public void setShowPublicFolder(boolean showPublicFolder){
		this.showPublicFolder = showPublicFolder;
	}
	
	public void setShowDropboxFolder(boolean showDropboxFolder){
		this.showDropboxFolder = showDropboxFolder;
	}
	
	public void setColumnsToHide(String columns) {
		this.columnsToHide = columns;
	}
	
	public void setUseVersionControl(boolean useVersionControl) {
		this.useVersionControl = useVersionControl;
	}
	
	public void setOnFileClickEvent(String onclick) {
		this.onFileClickEvent = onclick;
	}
	
	/**
	 * @return Returns the showPermissionTab.
	 */
	public boolean getShowPermissionTab() {
		return this.showPermissionTab;
	}
	/**
	 * @param showPermissionTab The showPermissionTab to set.
	 */
	public void setShowPermissionTab(boolean showPermissionTab) {
		this.showPermissionTab = showPermissionTab;
	}	
	
	/**
	 * @return Returns the showUploadComponent.
	 */
	public boolean getShowUploadComponent() {
		return this.showUploadComponent;
	}
	/**
	 * @param showUploadComponent The showUploadComponent to set.
	 */
	public void setShowUploadComponent(boolean showUploadComponent) {
		this.showUploadComponent = showUploadComponent;
	}
	
	public void release() {      
		super.release();      
		this.rootFolder = null ;
		this.startFolder = null;
		this.useUserHomeFolder = false;
		this.showFolders = true;
		this.showPublicFolder = true;
		this.showDropboxFolder = true;
		this.iconTheme = null;
		this.columnsToHide = null;
		this.useVersionControl = true;
		this.showPermissionTab = true;
		this.showUploadComponent = true;
		this.onFileClickEvent = null;
	}

	protected void setProperties(UIComponent component) {
		if (component != null) {
			ContentViewer viewer = (ContentViewer) component;
			super.setProperties(component);

			viewer.setRootFolder(this.rootFolder);
			viewer.setStartFolder(this.startFolder);
			viewer.setUseUserHomeFolder(this.useUserHomeFolder);
			viewer.setIconTheme(this.iconTheme);
			viewer.setShowFolders(this.showFolders);
			viewer.setShowDropboxFolder(this.showDropboxFolder);
			viewer.setShowPublicFolder(this.showPublicFolder);
			viewer.setColumnsToHide(this.columnsToHide);
			viewer.setUseVersionControl(this.useVersionControl);
			viewer.setShowPermissionTab(this.showPermissionTab);
			viewer.setShowUploadComponent(this.showUploadComponent);
			viewer.setOnFileClickEvent(this.onFileClickEvent);
		}
	}

	
	public String getComponentType() {
		return "ContentViewer";
	}

	public String getRendererType() {
		return null;
	}

}
