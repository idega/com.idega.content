package com.idega.content.presentation;

import javax.faces.component.UIComponent;
import javax.faces.webapp.UIComponentTag;

/**
 * @author gediminas
 */
public class CategoriesEditorTag extends UIComponentTag {

	protected void setProperties(UIComponent component) {
		if (component != null) {
			CategoriesEditor editor = (CategoriesEditor) component;
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
