package com.idega.content.themes.business;


import com.idega.business.IBOService;
import java.rmi.RemoteException;

public interface ThemesEngine extends IBOService {
	/**
	 * @see com.idega.content.themes.business.ThemesEngineBean#getThemesPreviewsInfo
	 */
	public String getThemesPreviewsInfo() throws RemoteException;

	/**
	 * @see com.idega.content.themes.business.ThemesEngineBean#getThemeStyleVariations
	 */
	public String getThemeStyleVariations(String themeID) throws RemoteException;

	/**
	 * @see com.idega.content.themes.business.ThemesEngineBean#changeTheme
	 */
	public String changeTheme(String themeID, String styleGroupName, String styleMember, String themeName, boolean radio, boolean checked) throws RemoteException;

	/**
	 * @see com.idega.content.themes.business.ThemesEngineBean#saveTheme
	 */
	public boolean saveTheme(String themeID, String themeName) throws RemoteException;

	/**
	 * @see com.idega.content.themes.business.ThemesEngineBean#setSelectedStyle
	 */
	public String setSelectedStyle(String themeID, boolean applyToPage) throws RemoteException;

	/**
	 * @see com.idega.content.themes.business.ThemesEngineBean#savePageInfo
	 */
	public boolean savePageInfo(String pageID, String[] keywords, String[] values) throws RemoteException;

	/**
	 * @see com.idega.content.themes.business.ThemesEngineBean#getPageInfoElements
	 */
	public String[] getPageInfoElements() throws RemoteException;

	/**
	 * @see com.idega.content.themes.business.ThemesEngineBean#restoreTheme
	 */
	public boolean restoreTheme(String themeID) throws RemoteException;
}