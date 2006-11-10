package com.idega.content.presentation;

import javax.faces.component.UIComponent;
import javax.faces.webapp.UIComponentTag;

public class WebDAVDocumentDeleterTag extends UIComponentTag {

	private boolean embedInForm;
	private String redirectOnSuccessURI;
	private boolean useLinkAsSubmit;
	private boolean showFullPath;
	
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

	public void setRedirectOnSuccessURI(String uri) {
		this.redirectOnSuccessURI = uri;
	}

	public String getRedirectOnSuccessURI() {
		return redirectOnSuccessURI;
	}
	
	public boolean getUseLinkAsSubmit() {
		return useLinkAsSubmit;
	}

	public void setUseLinkAsSubmit(boolean useLinkAsSubmit) {
		this.useLinkAsSubmit = useLinkAsSubmit;
	}
	
	public boolean getShowFullPath() {
		return showFullPath;
	}

	public void setShowFullPath(boolean showFullPath) {
		this.showFullPath = showFullPath;
	}
	
	public void release() {   
		super.release();      

		this.embedInForm = false;
		this.redirectOnSuccessURI = null;
		useLinkAsSubmit = false;
		showFullPath = true;
	}
	
	protected void setProperties(UIComponent component) {      
		if (component instanceof WebDAVUpload) {
			super.setProperties(component);
			
			WebDAVDocumentDeleter deleter = (WebDAVDocumentDeleter) component;
			deleter.setEmbeddedInForm(embedInForm);
			deleter.setRedirectOnSuccessURI(redirectOnSuccessURI);
			deleter.setUseLinkAsSubmit(useLinkAsSubmit);
			deleter.setShowFullPath(showFullPath);
		}
	}

}
