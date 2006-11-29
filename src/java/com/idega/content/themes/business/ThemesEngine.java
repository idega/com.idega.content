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
	public String changeTheme(String themeID, String styleGroupName, String styleMember, String themeName, boolean isRadio, boolean isChecked) throws RemoteException;

	/**
	 * @see com.idega.content.themes.business.ThemesEngineBean#saveTheme
	 */
	public boolean saveTheme(String themeID, String themeName) throws RemoteException;

	/**
	 * @see com.idega.content.themes.business.ThemesEngineBean#setSelectedStyle
	 */
	public boolean setSelectedStyle(String themeID, String pageID, boolean applyToPage) throws RemoteException;

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

	/**
	 * @see com.idega.content.themes.business.ThemesEngineBean#getSiteInfoElements
	 */
	public String[] getSiteInfoElements() throws RemoteException;

	/**
	 * @see com.idega.content.themes.business.ThemesEngineBean#getSiteInfoValues
	 */
	public String[] getSiteInfoValues(String[] keywords, String language) throws RemoteException;

	/**
	 * @see com.idega.content.themes.business.ThemesEngineBean#saveSiteInfo
	 */
	public boolean saveSiteInfo(String language, String[] keywords, String[] values);

	/**
	 * @see com.idega.content.themes.business.ThemesEngineBean#createPage
	 */
	public int createPage(String parentId, String name, String type, String templateId, String pageUri, String subType, int domainId, String format, String sourceMarkup) throws RemoteException;

	/**
	 * @see com.idega.content.themes.business.ThemesEngineBean#deletePage
	 */
	public boolean deletePage(String pageId, boolean deleteChildren) throws RemoteException;

	/**
	 * @see com.idega.content.themes.business.ThemesEngineBean#getPageId
	 */
	public String getPageId();

	/**
	 * @see com.idega.content.themes.business.ThemesEngineBean#setPageId
	 */
	public boolean setPageId(String id);
}