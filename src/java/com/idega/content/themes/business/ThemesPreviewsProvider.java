package com.idega.content.themes.business;


import com.idega.business.IBOService;
import java.rmi.RemoteException;

public interface ThemesPreviewsProvider extends IBOService {
	/**
	 * @see com.idega.content.themes.business.ThemesPreviewsProviderBean#getThemesPreviewsInfo
	 */
	public String getThemesPreviewsInfo() throws RemoteException;

	/**
	 * @see com.idega.content.themes.business.ThemesPreviewsProviderBean#getThemeStyleVariations
	 */
	public String getThemeStyleVariations(String themeID) throws RemoteException;

	/**
	 * @see com.idega.content.themes.business.ThemesPreviewsProviderBean#changeTheme
	 */
	public String changeTheme(String themeID, String styleGroupName, String styleMember, String themeName, boolean radio, boolean checked) throws RemoteException;

	/**
	 * @see com.idega.content.themes.business.ThemesPreviewsProviderBean#saveTheme
	 */
	public boolean saveTheme(String themeID, String themeName) throws RemoteException;

	/**
	 * @see com.idega.content.themes.business.ThemesPreviewsProviderBean#setSelectedStyle
	 */
	public String setSelectedStyle(String themeID, boolean applyToPage) throws RemoteException;
}