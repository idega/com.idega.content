/*
 * Created on 14.12.2004
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
public class WebDAVFileDetailsTag extends UIComponentTag {
	
	String path;
	
	public String getComponentType() {
		return "WebDAVFileDetails";	
	}

	public String getRendererType() {
		return null;
	}
	
	public void release() {      
		super.release();      
		this.path = null ; 
	}

	protected void setProperties(UIComponent component) {      
		super.setProperties(component);
		if (component != null) {
			component.getAttributes().put("path", this.path);
		}
	}
	
	public void setWebDAVPath(String path) {
		this.path = path;
	}
	
	public String getWebDAVPath() {
		return this.path;
	}

}
