package com.idega.content.themes.business;


import com.idega.content.themes.helpers.ThemeChange;
import com.idega.content.themes.helpers.TreeNodeStructure;
import com.idega.core.builder.data.ICDomain;
import com.idega.idegaweb.IWMainApplicationSettings;
import com.idega.core.builder.business.BuilderService;
import com.idega.business.IBOService;
import java.util.List;
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
	 * @see com.idega.content.themes.business.ThemesEngineBean#changePageUri
	 */
	public String changePageUri(String pageID, String pageTitle, boolean needSetPageTitle) throws RemoteException;

	/**
	 * @see com.idega.content.themes.business.ThemesEngineBean#savePageInfo
	 */
	public String savePageInfo(String pageID, String[] keywords, String[] values) throws RemoteException;

	/**
	 * @see com.idega.content.themes.business.ThemesEngineBean#getPageInfoValues
	 */
	public String[] getPageInfoValues(String pageID, String[] keywords) throws RemoteException;

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
	 * @see com.idega.content.themes.business.ThemesEngineBean#getSiteInfoValue
	 */
	public String getSiteInfoValue(String keyword, String language, IWMainApplicationSettings settings, ICDomain domain);

	/**
	 * @see com.idega.content.themes.business.ThemesEngineBean#saveSiteInfoValue
	 */
	public boolean saveSiteInfoValue(String keyword, String value) throws RemoteException;

	/**
	 * @see com.idega.content.themes.business.ThemesEngineBean#saveSiteInfo
	 */
	public boolean saveSiteInfo(String language, String[] keywords, String[] values);

	/**
	 * @see com.idega.content.themes.business.ThemesEngineBean#beforeCreatePage
	 */
	public List<String> beforeCreatePage(List<TreeNodeStructure> struct, Boolean isFirst) throws RemoteException;
	
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
	public String getPageId() throws RemoteException;

	/**
	 * @see com.idega.content.themes.business.ThemesEngineBean#setPageId
	 */
	public boolean setPageId(String id) throws RemoteException;

	/**
	 * @see com.idega.content.themes.business.ThemesEngineBean#movePage
	 */
	public boolean movePage(int newParentId, int nodeId) throws RemoteException;

	/**
	 * @see com.idega.content.themes.business.ThemesEngineBean#getPathToImageFolder
	 */
	public String getPathToImageFolder() throws RemoteException;

	/**
	 * @see com.idega.content.themes.business.ThemesEngineBean#isStartPage
	 */
	public boolean isStartPage(String pageID) throws RemoteException;

	/**
	 * @see com.idega.content.themes.business.ThemesEngineBean#setAsStartPage
	 */
	public String setAsStartPage(String pageID) throws RemoteException;

	/**
	 * @see com.idega.content.themes.business.ThemesEngineBean#createRootTemplate
	 */
	public int createRootTemplate(ICDomain domain, BuilderService builder, int domainID, String format);

	/**
	 * @see com.idega.content.themes.business.ThemesEngineBean#initializeCachedDomain
	 */
	public boolean initializeCachedDomain(String domainName, ICDomain domain);

	/**
	 * @see com.idega.content.themes.business.ThemesEngineBean#applyMultipleChangesToTheme
	 */
	public String applyMultipleChangesToTheme(String themeID, List<ThemeChange> changes, String themeName);
}