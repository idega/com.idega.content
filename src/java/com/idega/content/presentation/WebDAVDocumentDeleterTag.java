package com.idega.content.presentation;

import javax.faces.component.UIComponent;
import javax.faces.webapp.UIComponentTag;

public class WebDAVDocumentDeleterTag extends UIComponentTag {

	private boolean embedInForm;
	private String redirectOnSuccessURI;
	
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
		this.redirectOnSuccessURI = null;
	}

	public void setRedirectOnSuccessURI(String uri) {
		this.redirectOnSuccessURI = uri;
	}

	public String getRedirectOnSuccessURI() {
		return redirectOnSuccessURI;
	}
	
	protected void setProperties(UIComponent component) {      
		if (component instanceof WebDAVUpload) {
			super.setProperties(component);
			
			WebDAVDocumentDeleter deleter = (WebDAVDocumentDeleter) component;
			deleter.setEmbeddedInForm(embedInForm);
			deleter.setRedirectOnSuccessURI(redirectOnSuccessURI);
		}
	}

}
