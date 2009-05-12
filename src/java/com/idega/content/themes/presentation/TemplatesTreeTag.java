package com.idega.content.themes.presentation;

import javax.faces.component.UIComponent;
import javax.faces.webapp.UIComponentELTag;

public class TemplatesTreeTag extends UIComponentELTag {

	@Override
	public String getComponentType() {
		return TemplatesTree.class.getSimpleName();
	}

	@Override
	public String getRendererType() {
		return null;
	}

	@Override
	protected void setProperties(UIComponent component) {
		if (component instanceof TemplatesTree) {
			super.setProperties(component);
			
		}
	}
	
	@Override
	public void release() { 
		super.release();
	}

}
