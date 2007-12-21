package com.idega.content.themes.helpers.business;

import java.util.List;

import com.idega.business.SpringBeanName;
import com.idega.content.themes.helpers.bean.Theme;

@SpringBeanName("themesPropertiesExtractor")
public interface ThemesPropertiesExtractor {
	
	public boolean prepareTheme(boolean checkConfigFile, Theme theme, List<String> pLists, List<String> configs) throws Exception;
	
	public void prepareThemes(List<String> pLists, List<String> configs, boolean useThread) throws Exception;

}