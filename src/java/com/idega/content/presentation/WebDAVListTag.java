/*
 * Created on 11.12.2004
 *
 * TODO To change the template for this generated file go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
package com.idega.content.presentation;

import javax.faces.component.UIComponent;
import javax.faces.webapp.UIComponentTag;

/**
 * @author gimmi
 *
 * TODO To change the template for this generated type comment go to
 * Window - Preferences - Java - Code Style - Code Templates
 */
public class WebDAVListTag extends UIComponentTag {

	private String rootFolder;
	private String startFolder;
	
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
	
	public void release() {      
		super.release();      
		this.rootFolder = null ;
		this.startFolder = null;
	}

	protected void setProperties(UIComponent component) {      
		super.setProperties(component);
		if (component != null) {
			component.getAttributes().put("rootFolder", this.rootFolder);
			component.getAttributes().put("startFolder", this.startFolder);
		}
	}

	
	public String getComponentType() {
		return "WebDAVList";
	}

	public String getRendererType() {
		return null;
	}

}
