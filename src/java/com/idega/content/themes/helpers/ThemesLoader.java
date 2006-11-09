package com.idega.content.themes.helpers;

import java.util.List;
import java.util.Random;

public class ThemesLoader {
	
	private Theme theme = null;
	private Random generator = new Random();
	private ThemesHelper helper = null;
	
	public ThemesLoader(ThemesHelper helper) {
		this.helper = helper;
	}
	
	public synchronized boolean loadTheme(String originalUri, String encodedUri, boolean newTheme) {
		if (encodedUri == null || originalUri == null) {
			return false;
		}
		encodedUri = getUriWithoutContent(encodedUri);
		originalUri = getUriWithoutContent(originalUri);
		
		createNewTheme(originalUri, encodedUri, newTheme);
		
		return true;
	}
	
	public synchronized boolean loadThemes(List <String> urisToThemes, boolean newThemes) {
		if (urisToThemes == null) {
			return false;
		}
		
		boolean result = true;
		for (int i = 0; (i < urisToThemes.size() && result); i++) {
			result = loadTheme(helper.decodeUrl(urisToThemes.get(i)), urisToThemes.get(i), newThemes);
		}
			
		return result;
	}
	
	private void initTheme(boolean newTheme) {
		if (theme == null) {
			theme = new Theme(getThemeId());
			theme.setNewTheme(newTheme);
			theme.setLoading(true);
		}
	}
	
	private String getThemeId() {
		String id = String.valueOf(generator.nextInt(Integer.MAX_VALUE));
		while (helper.getTheme(id) != null) { // Checking if exists Theme with the same ID
			id = String.valueOf(generator.nextInt(Integer.MAX_VALUE));
		}
		return id;
	}
	
	private void addThemeInfo() {
		if (theme == null) {
			return;
		}
		if (!helper.getThemesCollection().contains(theme)) {
			helper.addTheme(theme);
			theme.setLoading(false);
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
	
	public synchronized String createNewTheme(String originalUri, String encodedUri, boolean newTheme) {
		String themeID = null;
		
		helper.addUriToTheme(originalUri);
		
		initTheme(newTheme);
		
		themeID = theme.getThemeId();
		
		theme.setLinkToSkeleton(encodedUri);
		theme.setLinkToBase(helper.getLinkToBase(encodedUri));
		theme.setLinkToBaseAsItIs(helper.getLinkToBase(originalUri));
		addThemeInfo();
		
		return themeID;
	}

}
