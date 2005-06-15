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
		rootFolder = root;
	}
	
	public String getRootPath() {
		return rootFolder;
	}
	
	public void setStartPath(String start) {
		this.startFolder = start;
	}
	
	public String getStartPath() {
		return startFolder;
	}
	
	public void setUseUserHomeFolder(boolean useUserFolder) {
		useUserHomeFolder = useUserFolder;
	}
	
	public void setIconTheme(String themeName) {
		iconTheme = themeName;
	}
	
	public String getIconTheme() {
		return iconTheme;
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
		return showPermissionTab;
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
		return showUploadComponent;
	}
	/**
	 * @param showUploadComponent The showUploadComponent to set.
	 */
	public void setShowUploadComponent(boolean showUploadComponent) {
		this.showUploadComponent = showUploadComponent;
	}
	
	public void release() {      
		super.release();      
		rootFolder = null ;
		startFolder = null;
		useUserHomeFolder = false;
		showFolders = true;
		showPublicFolder = true;
		showDropboxFolder = true;
		iconTheme = null;
		columnsToHide = null;
		useVersionControl = true;
		showPermissionTab = true;
		showUploadComponent = true;
		onFileClickEvent = null;
	}

	protected void setProperties(UIComponent component) {
		if (component != null) {
			ContentViewer viewer = (ContentViewer) component;
			super.setProperties(component);

			viewer.setRootFolder(rootFolder);
			viewer.setStartFolder(startFolder);
			viewer.setUseUserHomeFolder(useUserHomeFolder);
			viewer.setIconTheme(iconTheme);
			viewer.setShowFolders(showFolders);
			viewer.setShowDropboxFolder(showDropboxFolder);
			viewer.setShowPublicFolder(showPublicFolder);
			viewer.setColumnsToHide(columnsToHide);
			viewer.setUseVersionControl(useVersionControl);
			viewer.setShowPermissionTab(showPermissionTab);
			viewer.setShowUploadComponent(showUploadComponent);
			viewer.setOnFileClickEvent(onFileClickEvent);
		}
	}

	
	public String getComponentType() {
		return "ContentViewer";
	}

	public String getRendererType() {
		return null;
	}

}
