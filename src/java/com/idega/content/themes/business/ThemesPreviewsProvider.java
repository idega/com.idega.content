package com.idega.content.themes.business;


import com.idega.business.IBOService;
import java.rmi.RemoteException;

public interface ThemesPreviewsProvider extends IBOService {
	/**
	 * @see com.idega.content.business.ThemesPreviewsProviderBean#getThemesPreviewsInfo
	 */
	public String getThemesPreviewsInfo() throws RemoteException;

	/**
	 * @see com.idega.content.business.ThemesPreviewsProviderBean#getThemeStyleVariations
	 */
	public String getThemeStyleVariations(String themeID) throws RemoteException;

	/**
	 * @see com.idega.content.business.ThemesPreviewsProviderBean#changeTheme
	 */
	public String changeTheme(String themeID, String styleGroupName, String newStyleMember) throws RemoteException;

	/**
	 * @see com.idega.content.business.ThemesPreviewsProviderBean#saveTheme
	 */
	public boolean saveTheme(String themeID) throws RemoteException;
}