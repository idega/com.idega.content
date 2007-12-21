package com.idega.content.themes.helpers.business;

import java.util.List;

import com.idega.content.themes.helpers.bean.Theme;
import com.idega.util.CoreConstants;

public class ThemesLoader {
	
	private Theme theme = null;
	private ThemesHelper helper = null;
	
	public ThemesLoader(ThemesHelper helper) {
		this.helper = helper;
	}
	
	public ThemesLoader() {
		this(ThemesHelper.getInstance(false));
	}
	
	public boolean loadTheme(String originalUri, String encodedUri, boolean newTheme, boolean manuallyCreated) {
		if (encodedUri == null || originalUri == null) {
			return false;
		}

		if (encodedUri.startsWith(CoreConstants.WEBDAV_SERVLET_URI)) {
			encodedUri = getUriWithoutContent(encodedUri);
		}
		if (originalUri.startsWith(CoreConstants.WEBDAV_SERVLET_URI)) {
			originalUri = getUriWithoutContent(originalUri);
		}
		
		if (createNewTheme(originalUri, encodedUri, newTheme, manuallyCreated) == null) {
			return false;
		}
		
		return true;
	}
	
	public synchronized boolean loadThemes(List<String> skeletons, boolean newThemes, boolean manuallyCreated) {
		if (skeletons == null) {
			return false;
		}
		boolean result = true;
		for (int i = 0; (i < skeletons.size() && result); i++) {
			result = loadTheme(helper.decodeUrl(skeletons.get(i)), skeletons.get(i), newThemes, manuallyCreated);
		}
		return result;
	}
	
	private void initTheme(boolean newTheme) {
		if (theme == null) {
			theme = new Theme(getThemeId());
			theme.setNewTheme(newTheme);
		}
	}
	
	private boolean addThemeInfo() {
		if (theme == null) {
			return false;
		}
		List<Theme> themes = helper.getAvailableThemes();
		if (themes == null) {
			return false;
		}
		
		if (!themes.contains(theme)) {
			helper.addTheme(theme);
		}
		
		theme = null;
	
		return true;
	}
	
	private String getUriWithoutContent(String uri) {
		int index = uri.indexOf(CoreConstants.WEBDAV_SERVLET_URI);
		if (index == -1) {
			return uri;
		}
		index += CoreConstants.WEBDAV_SERVLET_URI.length();
		return helper.extractValueFromString(uri, index, uri.length());
	}
	
	protected synchronized String createNewTheme(String originalUri, String encodedUri, boolean newTheme, boolean manuallyCreated) {
		helper.addUriToTheme(originalUri);
		
		initTheme(newTheme);
		
		String themeID = theme.getId();
		
		theme.setLinkToSkeleton(encodedUri);
		theme.setLinkToBase(helper.getLinkToBase(encodedUri));
		theme.setLinkToBaseAsItIs(helper.getLinkToBase(originalUri));
		
		if (manuallyCreated) {
			theme.setLoading(false);
		}
		
		if (addThemeInfo()) {
			return themeID;
		}
		
		return null;
	}
	
	private String getThemeId() {
		String id = String.valueOf(helper.getRandomNumber(Integer.MAX_VALUE));
		while (helper.getTheme(id) != null) { // Checking if exists Theme with the same ID
			id = String.valueOf(helper.getRandomNumber(Integer.MAX_VALUE));
		}
		return id;
	}

}