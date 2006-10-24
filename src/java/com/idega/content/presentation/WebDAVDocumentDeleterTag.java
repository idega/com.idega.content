package com.idega.content.presentation;

import javax.faces.component.UIComponent;
import javax.faces.webapp.UIComponentTag;

public class WebDAVDocumentDeleterTag extends UIComponentTag {

	private boolean embedInForm;

	public String getComponentType() {
		return "WebDAVDocumentDeleter";
	}

	public String getRendererType() {
		return null;
	}

	public void setEmbeddedInForm(boolean embedInForm) {
		this.embedInForm = embedInForm;
	}
	
	public boolean getEmbeddedInForm() {
		return embedInForm;
	}
	
	public void release() {   
		super.release();      

		this.embedInForm = false;
	}

	protected void setProperties(UIComponent component) {      
		if (component instanceof WebDAVUpload) {
			super.setProperties(component);
			
			WebDAVDocumentDeleter deleter = (WebDAVDocumentDeleter) component;
			deleter.setEmbeddedInForm(embedInForm);
		}
	}

}
