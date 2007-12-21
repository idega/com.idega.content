package com.idega.content.themes.bean;

import java.io.Serializable;

import com.idega.content.themes.helpers.business.ThemesConstants;

public class ThemesManagerBean implements Serializable {
	
	private static final long serialVersionUID = -187806294166511859L;
	
	private String lastVisitedPageId = null;
	private String themeID = null;
	
	public final static String THEMES_MANAGER_BEAN_ID = "ThemesManagerBean";
	
	public ThemesManagerBean() {
	}
	
	public String getThemesPath() {
		return ThemesConstants.THEMES_PATH;
	}
	
	public String getThemesPreviewPath() {
		return ThemesConstants.THEMES_PREVIEW_PATH;
	}

	public String getBeanId() {
		return THEMES_MANAGER_BEAN_ID;
	}

	public String getLastVisitedPageId() {
		return lastVisitedPageId;
	}

	public void setLastVisitedPageId(String lastVisitedPageId) {
		this.lastVisitedPageId = lastVisitedPageId;
	}

	public String getThemeId() {
		return themeID;
	}

	public void setThemeId(String themeID) {
		this.themeID = themeID;
	}

}