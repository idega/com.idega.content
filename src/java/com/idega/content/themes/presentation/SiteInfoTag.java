package com.idega.content.themes.presentation;

import javax.faces.component.UIComponent;
import javax.faces.webapp.UIComponentELTag;

public class SiteInfoTag extends UIComponentELTag {

	private String styleClass;
	
	@Override
	public String getComponentType() {
		return "SiteInfo";
	}

	@Override
	public String getRendererType() {
		return null;
	}
	
	@Override
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
	
	@Override
	public void release() {   
		super.release();
		styleClass = null;
	}

}
