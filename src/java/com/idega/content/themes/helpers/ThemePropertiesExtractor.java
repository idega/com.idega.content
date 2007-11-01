package com.idega.content.themes.helpers;

import java.util.List;

public class ThemePropertiesExtractor extends Thread implements Runnable {

	private Theme theme = null;
	private ThemesPropertiesExtractor extractor = null;
	private List<String> pLists = null;
	private List<String> configs = null;

	public ThemePropertiesExtractor(Theme theme, ThemesPropertiesExtractor extractor, List<String> pLists, List<String> configs) {
		setDaemon(true);
		this.theme = theme;
		this.extractor = extractor;
		this.pLists = pLists;
		this.configs = configs;
	}

	public void run() {
		try {
			extractor.prepareTheme(true, theme, pLists, configs);
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
	
}