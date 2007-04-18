package com.idega.content.themes.helpers;

import java.util.List;

import com.idega.content.business.ContentConstants;

public class ThemesLoader {
	
	private Theme theme = null;
	private ThemesHelper helper = null;
	
	public ThemesLoader(ThemesHelper helper) {
		this.helper = helper;
	}
	
	public boolean loadTheme(String originalUri, String encodedUri, boolean newTheme, boolean manuallyCreated) {
		if (encodedUri == null || originalUri == null) {
			return false;
		}

		if (encodedUri.startsWith(ContentConstants.CONTENT)) {
			encodedUri = getUriWithoutContent(encodedUri);
		}
		if (originalUri.startsWith(ContentConstants.CONTENT)) {
			originalUri = getUriWithoutContent(originalUri);
		}
		
		if (createNewTheme(originalUri, encodedUri, newTheme, manuallyCreated) == null) {
			return false;
		}
		
		return true;
	}
	
	public synchronized boolean loadThemes(List <String> urisToThemes, boolean newThemes, boolean manuallyCreated) {
		if (urisToThemes == null) {
			return false;
		}
		
		boolean result = true;
		for (int i = 0; (i < urisToThemes.size() && result); i++) {
			//TODO load each one in a thread
			result = loadTheme(helper.decodeUrl(urisToThemes.get(i)), urisToThemes.get(i), newThemes, manuallyCreated);
		
//			ThemeLoaderThread loader = new ThemeLoaderThread(this, helper,urisToThemes.get(i), newThemes, manuallyCreated);
//			loader.start();
			
		}
			
		return result;
	}
	
	private void initTheme(boolean newTheme) {
		if (theme == null) {
			theme = new Theme(getThemeId());
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
		int index = uri.indexOf(ContentConstants.CONTENT);
		if (index == -1) {
			return uri;
		}
		index += ContentConstants.CONTENT.length();
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
		
		addThemeInfo();
		
		return themeID;
	}
	
	private String getThemeId() {
		String id = String.valueOf(helper.getRandomNumber(Integer.MAX_VALUE));
		while (helper.getTheme(id) != null) { // Checking if exists Theme with the same ID
			id = String.valueOf(helper.getRandomNumber(Integer.MAX_VALUE));
		}
		return id;
	}

}

class ThemeLoaderThread extends Thread implements Runnable {
	private ThemesLoader loader;
	boolean success = false;
	ThemesHelper helper;
	String uriToTheme;
	boolean newTheme;
	boolean manuallyCreated;

	public ThemeLoaderThread(ThemesLoader loader,ThemesHelper helper,String uriToTheme, boolean newTheme, boolean manuallyCreated){
		setDaemon(true);
		this.loader = loader;
		this.helper = helper;
		this.uriToTheme = uriToTheme;
		this.newTheme = newTheme;
		this.manuallyCreated = manuallyCreated;
	}

	public void run() {
		success = loader.loadTheme(helper.decodeUrl(uriToTheme), uriToTheme, newTheme, manuallyCreated);
	}
	
}
			
