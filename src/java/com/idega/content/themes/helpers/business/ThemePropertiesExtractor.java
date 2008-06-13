package com.idega.content.themes.helpers.business;

import java.util.List;

import com.idega.content.themes.helpers.bean.Theme;

public class ThemePropertiesExtractor extends Thread implements Runnable {

	private Theme theme = null;
	private ThemesPropertiesExtractor extractor = null;
	private List<String> pLists = null;
	private List<String> configs = null;
	private List<String> predefinedStyles = null;

	public ThemePropertiesExtractor(Theme theme, ThemesPropertiesExtractor extractor, List<String> pLists, List<String> configs, List<String> predefinedStyles) {
		setDaemon(true);
		this.theme = theme;
		this.extractor = extractor;
		this.pLists = pLists;
		this.configs = configs;
		this.predefinedStyles = predefinedStyles;
	}

	public void run() {
		try {
			extractor.prepareTheme(true, theme, pLists, configs, predefinedStyles);
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
	
}