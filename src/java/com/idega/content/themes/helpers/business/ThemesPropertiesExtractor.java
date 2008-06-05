package com.idega.content.themes.helpers.business;

import java.util.List;

import com.idega.content.themes.helpers.bean.Theme;

public interface ThemesPropertiesExtractor {

	public static final String SPRING_BEAN_IDENTIFIER = "themesPropertiesExtractor";
	
	public boolean prepareTheme(boolean checkConfigFile, Theme theme, List<String> pLists, List<String> configs) throws Exception;
	
	public void prepareThemes(List<String> pLists, List<String> configs, boolean useThread) throws Exception;

}