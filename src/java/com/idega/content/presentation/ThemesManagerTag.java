package com.idega.content.presentation;

import javax.faces.component.UIComponent;
import javax.faces.webapp.UIComponentTag;

public class ThemesManagerTag extends UIComponentTag {
	
	public String getComponentType() {
		return "ThemesManager";
	}

	public String getRendererType() {
		return null;
	}
	
	protected void setProperties(UIComponent component) {
		if (component instanceof ThemesManager) {
			super.setProperties(component);
		}
	}

}
