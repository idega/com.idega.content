package com.idega.content.presentation;

import javax.faces.component.UIComponent;
import javax.faces.webapp.UIComponentTag;

/**
 * @author gimmi
 */
public class ContentViewerTag extends UIComponentTag {

	private String rootFolder;
	private String startFolder;
	private Boolean useUserHomeFolder;
	private String iconTheme;
	
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
		useUserHomeFolder = new Boolean(useUserFolder);
	}
	
	public void setIconTheme(String themeName) {
		iconTheme = themeName;
	}
	
	public String getIconTheme() {
		return iconTheme;
	}
	
	public void release() {      
		super.release();      
		rootFolder = null ;
		startFolder = null;
		useUserHomeFolder = null;
		iconTheme = null;
	}

	protected void setProperties(UIComponent component) {      
		super.setProperties(component);
		if (component != null) {
			component.getAttributes().put("rootFolder", rootFolder);
			component.getAttributes().put("startFolder", startFolder);
			component.getAttributes().put("useUserHomeFolder", useUserHomeFolder);
			component.getAttributes().put("iconTheme", iconTheme);
		}
	}

	
	public String getComponentType() {
		return "ContentViewer";
	}

	public String getRendererType() {
		return null;
	}

}
