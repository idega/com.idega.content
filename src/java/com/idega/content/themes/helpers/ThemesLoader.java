package com.idega.content.themes.helpers;

import java.util.List;
import java.util.Random;

public class ThemesLoader {
	
	private ThemeInfo theme = null;
	private Random generator = new Random();
	private ThemesHelper helper = ThemesHelper.getInstance();
	
	public synchronized boolean loadTheme(String uriToTheme, boolean newTheme) {
		if (uriToTheme == null) {
			return false;
		}
		uriToTheme = getUriWithoutContent(uriToTheme);
		
		initThemeInfo(newTheme);
		theme.setLinkToSkeleton(uriToTheme);
		theme.setLinkToBase(helper.getLinkToBase(uriToTheme));
		addThemeInfo();
		
		return true;
	}
	
	public synchronized boolean loadThemes(List <String> urisToThemes, boolean newThemes) {
		if (urisToThemes == null) {
			return false;
		}
		
		boolean result = true;
		for (int i = 0; (i < urisToThemes.size() && result); i++) {
			result = loadTheme(urisToThemes.get(i), newThemes);
		}
			
		return result;
	}
	
	private void initThemeInfo(boolean newTheme) {
		if (theme == null) {
			theme = new ThemeInfo(String.valueOf(generator.nextInt(Integer.MAX_VALUE)));
			theme.setNewTheme(newTheme);
		}
	}
	
	private void addThemeInfo() {
		if (theme == null) {
			return;
		}
		if (!helper.getThemesCollection().contains(theme)) {
			helper.addTheme(theme);
		}
		theme = null;
	}
	
	private String getUriWithoutContent(String uri) {
		int index = uri.indexOf(ThemesConstants.CONTENT);
		if (index == -1) {
			return uri;
		}
		index += ThemesConstants.CONTENT.length();
		return helper.extractValueFromString(uri, index, uri.length());
	}

}
