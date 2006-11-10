package com.idega.content.presentation;

import javax.faces.component.UIComponent;
import javax.faces.webapp.UIComponentTag;

public class PageInfoTag extends UIComponentTag {

	private String styleClass;
	
	@Override
	public String getComponentType() {
		return "PageInfo";
	}

	@Override
	public String getRendererType() {
		return null;
	}
	
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
	
	public void release() {   
		super.release();
		styleClass = null;
	}

}
