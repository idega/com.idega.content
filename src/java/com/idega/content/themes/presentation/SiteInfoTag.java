package com.idega.content.themes.presentation;

import javax.faces.component.UIComponent;
import javax.faces.webapp.UIComponentTag;

public class SiteInfoTag extends UIComponentTag {

	private String styleClass;
	
	@Override
	public String getComponentType() {
		return "SiteInfo";
	}

	@Override
	public String getRendererType() {
		return null;
	}
	
	protected void setProperties(UIComponent component) {
		if (component instanceof SiteInfoJsf) {
			super.setProperties(component);
			SiteInfoJsf siteInfo = (SiteInfoJsf) component;
			siteInfo.setStyleClass(getStyleClass());
		}
	}
	
	public String getStyleClass() {
		return styleClass;
	}

	public void setStyleClass(String styleClass) {
		this.styleClass = styleClass;
	}
	
	public void release() {   
		super.release();
		styleClass = null;
	}

}
