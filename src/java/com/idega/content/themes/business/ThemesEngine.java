package com.idega.content.themes.business;

import java.rmi.RemoteException;
import java.util.ArrayList;
import java.util.List;

import org.jdom.Document;

import com.idega.content.themes.helpers.bean.PageAccessibilityProperty;
import com.idega.content.themes.helpers.bean.SimplifiedTheme;
import com.idega.content.themes.helpers.bean.ThemeChange;
import com.idega.content.themes.helpers.bean.TreeNodeStructure;
import com.idega.content.themes.helpers.business.ThemeChanger;
import com.idega.content.themes.helpers.business.ThemesPropertiesExtractor;
import com.idega.core.builder.business.BuilderService;
import com.idega.core.builder.data.ICDomain;
import com.idega.idegaweb.IWMainApplicationSettings;
import com.idega.presentation.IWContext;

public interface ThemesEngine {
	
	public static final String SPRING_BEAN_IDENTIFIER = "themesEngine";
	
	/**
	 * @see com.idega.content.themes.business.ThemesEngineBean#getThemes
	 */
	public List<SimplifiedTheme> getThemes();
	
	/**
	 * @see com.idega.content.themes.business.ThemesEngineBean#getTheme
	 */
	public SimplifiedTheme getTheme(String themeKey);

	/**
	 * @see com.idega.content.themes.business.ThemesEngineBean#getThemeStyleVariations
	 */
	public String getThemeStyleVariations(String themeKey) throws RemoteException;

	/**
	 * @see com.idega.content.themes.business.ThemesEngineBean#changeTheme
	 */
	public String changeTheme(String themeKey, String themeName, ThemeChange change) throws RemoteException;

	/**
	 * @see com.idega.content.themes.business.ThemesEngineBean#saveTheme
	 */
	public boolean saveTheme(String themeKey, String themeName) throws RemoteException;

	/**
	 * @see com.idega.content.themes.business.ThemesEngineBean#setSelectedStyle
	 */
	public boolean setSelectedStyle(String themeKey, String pageKey, Integer type, Integer templateId) throws RemoteException;

	/**
	 * @see com.idega.content.themes.business.ThemesEngineBean#changePageUri
	 */
	public String changePageUri(String pageKey, String pageTitle, boolean needSetPageTitle) throws RemoteException;

	/**
	 * @see com.idega.content.themes.business.ThemesEngineBean#setNewLinkInArticleFile
	 */
	public boolean setNewLinkInArticleFile(String pageKey, String moduleClass, String pageUri) throws RemoteException;

	/**
	 * @see com.idega.content.themes.business.ThemesEngineBean#savePageInfo
	 */
	public String savePageInfo(String pageKey, String[] keywords, String[] values) throws RemoteException;

	/**
	 * @see com.idega.content.themes.business.ThemesEngineBean#getPageInfoValues
	 */
	public String[] getPageInfoValues(String pageKey, String[] keywords) throws RemoteException;

	/**
	 * @see com.idega.content.themes.business.ThemesEngineBean#getPageInfoElements
	 */
	public String[] getPageInfoElements() throws RemoteException;

	/**
	 * @see com.idega.content.themes.business.ThemesEngineBean#restoreTheme
	 */
	public boolean restoreTheme(String themeKey) throws RemoteException;

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
	 * @see com.idega.content.themes.business.ThemesEngineBean#createPage
	 */
	public List<String> createPage(List<TreeNodeStructure> struct, Boolean isTopLevelPage, String numberInLevel, List<String> followingNodes) throws RemoteException;

	/**
	 * @see com.idega.content.themes.business.ThemesEngineBean#deletePage
	 */
	public boolean deletePage(String pageKey, boolean deleteChildren) throws RemoteException;

	/**
	 * @see com.idega.content.themes.business.ThemesEngineBean#deletePageAndDecrease
	 */
	public boolean deletePageAndDecrease(String pageKey, boolean deleteChildren, ArrayList<String> followingNodes) throws RemoteException;

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
	public boolean movePage(int newParentId, int nodeId, int numberInLevel, ArrayList<String> nodesToIncrease, ArrayList<String> nodesToDecrease) throws RemoteException;

	/**
	 * @see com.idega.content.themes.business.ThemesEngineBean#getPathToImageFolder
	 */
	public String getPathToImageFolder() throws RemoteException;

	/**
	 * @see com.idega.content.themes.business.ThemesEngineBean#isStartPage
	 */
	public boolean isStartPage(String pageKey) throws RemoteException;

	/**
	 * @see com.idega.content.themes.business.ThemesEngineBean#setAsStartPage
	 */
	public boolean setAsStartPage(String pageKey) throws RemoteException;

	/**
	 * @see com.idega.content.themes.business.ThemesEngineBean#createRootTemplate
	 */
	public String createRootTemplate(ICDomain domain, BuilderService builder, int domainID, String format);

	/**
	 * @see com.idega.content.themes.business.ThemesEngineBean#initializeCachedDomain
	 */
	public boolean initializeCachedDomain(String domainName, ICDomain domain);

	/**
	 * @see com.idega.content.themes.business.ThemesEngineBean#applyMultipleChangesToTheme
	 */
	public String applyMultipleChangesToTheme(String themeKey, List<ThemeChange> changes, String themeName) throws RemoteException;
	
	/**
	 * @see com.idega.content.themes.business.ThemesEngineBean#getLocalizedText
	 */
	public List<String> getLocalizedText();
	
	/**
	 * @see com.idega.content.themes.business.ThemesEngineBean#clearVariationFromCache
	 */
	public boolean clearVariationFromCache(String themeKey, IWContext iwc);
	
	/**
	 * @see ThemesEngineBean#startBuilderApplication
	 */
	public boolean startBuilderApplication();
	
	/**
	 * @see ThemesEngineBean#reloadThemeProperties
	 */
	public String reloadThemeProperties(String themeKey);
	
	/**
	 * @see ThemesEngineBean#isUserAdmin
	 */
	public boolean canUserActAsBuilderUser();
	
	/**
	 * @see ThemesEngineBean#getPageUri
	 */
	public String getPageUri(String pageKey);
	
	/**
	 * @see ThemesEngineBean#changePageName
	 */
	public boolean changePageName(int id, String newName);
	
	public boolean deleteArticlesFromDeletedPages(String pageKey);
	
	public boolean deleteArticle(String resourcePath);
	
	public void updateSiteTemplatesTree(IWContext iwc, boolean sendToAllSessions);
	
	public String createChildTemplateForThisTemplate(String parentTemplateKey);

	public boolean isUserContentEditor();
	
	public Document getRenderedPageInfo(String pageKey, String id, String styleClass);
	
	public String changePageUriAfterPageWasMoved(String pageKey);
	
	public Document getUpdatedSiteTree();
	
	public Document getUpdatedSiteTemplatesTree();
	
	public Document getReRenderedSiteInfo(String id, String styleClass);
	
	public boolean deleteTheme(String themeId);
	
	public String getPageIdByUri(String uri);
	
	public boolean setBuiltInStyle(String themeId, String builtInStyleId);
	
	public List<PageAccessibilityProperty> getPageAccessibilityProperties(String pageKey);
	
	public boolean setPageAccessibilityProperty(String pageKey, String code, String value, String columnName);
	
	public ThemeChanger getThemeChanger();
	
	public ThemesPropertiesExtractor getThemesPropertiesExtractor();
}