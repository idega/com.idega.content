package com.idega.content.presentation;

import javax.faces.webapp.UIComponentTag;

/**
 * @author gimmi
 */
public class ContentViewerTag extends UIComponentTag {

	public String getComponentType() {
		return "ContentViewer";
	}

	public String getRendererType() {
		return null;
	}

}
