package com.idega.content.themes.helpers;

public class ThemePropertiesExtractor extends Thread implements Runnable {

	private Theme theme = null;
	private ThemesPropertiesExtractor extractor = null;
	private ThemesHelper helper = null;

	public ThemePropertiesExtractor(Theme theme, ThemesHelper helper, ThemesPropertiesExtractor extractor) {
		setDaemon(true);
		this.theme = theme;
		this.helper = helper;
		this.extractor = extractor;
	}

	public void run() {
		helper.setFirstThemeWasLoaded(extractor.prepareTheme(theme));
	}
	
}