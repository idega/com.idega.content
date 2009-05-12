package com.idega.content.themes.presentation;

import javax.faces.component.UIComponent;
import javax.faces.webapp.UIComponentELTag;

public class ThemesManagerTag extends UIComponentELTag {
	
	@Override
	public String getComponentType() {
		return "ThemesManager";
	}

	@Override
	public String getRendererType() {
		return null;
	}
	
	@Override
	protected void setProperties(UIComponent component) {
		if (component instanceof ThemesManager) {
			super.setProperties(component);
		}
	}

}
