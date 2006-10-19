package com.idega.content.presentation;

import javax.faces.component.UIPanel;

public class UIDiv extends UIPanel {
//    public static final String COMPONENT_TYPE = "nl.iteye.jsf.Panel";
//    public static final String RENDERER_TYPE = "nl.iteye.jsf.Div";
	
	public static final String COMPONENT_TYPE = "com.idega.content.presentation.Panel";
	public static final String RENDERER_TYPE = "com.idega.content.presentation.Div";

    public UIDiv() {
        setRendererType(RENDERER_TYPE);
    }

}