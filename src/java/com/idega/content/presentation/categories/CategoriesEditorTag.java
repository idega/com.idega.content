package com.idega.content.presentation.categories;

import javax.faces.component.UIComponent;
import javax.faces.webapp.UIComponentTag;

public class CategoriesEditorTag extends UIComponentTag {

	protected void setProperties(UIComponent component) {
		if (component != null) {
			super.setProperties(component);
		}
	}

	
	public String getComponentType() {
		return "CategoriesEditor";
	}

	public String getRendererType() {
		return null;
	}

}
