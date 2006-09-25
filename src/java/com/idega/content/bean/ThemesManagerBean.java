package com.idega.content.bean;

import java.io.Serializable;

import com.idega.content.business.ThemesPreviewsProvider;
import com.idega.content.business.ThemesPreviewsProviderBean;

public class ThemesManagerBean implements Serializable {
	
	private static final long serialVersionUID = -187806294166511859L;
	
	public final static String THEMES_MANAGER_BEAN_ID = "ThemesManagerBean";
	
	private volatile static ThemesPreviewsProvider themesProvider = null;
	
	public ThemesPreviewsProvider getThemesProvider() {
		if (themesProvider == null) {
			synchronized (ThemesManagerBean.class) { // Using synchronization only once
				if (themesProvider == null) {
					themesProvider = new ThemesPreviewsProviderBean();
				}
			}
		}
		return themesProvider;
	}
	
	public String getThemesPath() {
		return getThemesProvider().getThemesPath();
	}
	
	public String getThemesPreviewPath() {
		return getThemesProvider().getThemesPreviewPath();
	}

	public String getBeanId() {
		return THEMES_MANAGER_BEAN_ID;
	}

}