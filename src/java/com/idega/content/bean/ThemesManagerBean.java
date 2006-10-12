package com.idega.content.bean;

import java.io.Serializable;

import com.idega.content.themes.helpers.ThemesConstants;

public class ThemesManagerBean implements Serializable {
	
	private static final long serialVersionUID = -187806294166511859L;
	
	public final static String THEMES_MANAGER_BEAN_ID = "ThemesManagerBean";
	
	public String getThemesPath() {
		return ThemesConstants.THEMES_PATH;
	}
	
	public String getThemesPreviewPath() {
		return ThemesConstants.THEMES_PREVIEW_PATH;
	}

	public String getBeanId() {
		return THEMES_MANAGER_BEAN_ID;
	}

}