package com.idega.content.themes.helpers;

public class ThemePropertiesExtractor extends Thread implements Runnable {

	private Theme theme = null;
	private ThemesPropertiesExtractor extractor = null;

	public ThemePropertiesExtractor(Theme theme, ThemesPropertiesExtractor extractor) {
		setDaemon(true);
		this.theme = theme;
		this.extractor = extractor;
	}

	public void run() {
		extractor.prepareTheme(theme);
	}
	
}