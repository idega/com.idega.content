package com.idega.content.themes.presentation;

import javax.faces.component.UIComponent;
import javax.faces.webapp.UIComponentELTag;

public class PageInfoTag extends UIComponentELTag {

	private String styleClass;
	
	@Override
	public String getComponentType() {
		return "PageInfo";
	}

	@Override
	public String getRendererType() {
		return null;
	}
	
	@Override
	protected void setProperties(UIComponent component) {
		if (component instanceof PageInfo) {
			super.setProperties(component);
			PageInfo pageInfo = (PageInfo) component;
			pageInfo.setStyleClass(getStyleClass());
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
