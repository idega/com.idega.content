package com.idega.content.themes.business;

import java.rmi.RemoteException;
import java.util.List;

import org.jdom.Document;

import com.idega.builder.bean.AdvancedProperty;
import com.idega.content.themes.helpers.bean.SimplifiedTheme;
import com.idega.content.themes.helpers.bean.ThemeChange;
import com.idega.content.themes.helpers.business.ThemeChanger;
import com.idega.content.themes.helpers.business.ThemesPropertiesExtractor;
import com.idega.core.builder.business.BuilderService;
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
	 * @see com.idega.content.themes.business.ThemesEngineBean#restoreTheme
	 */
	public boolean restoreTheme(String themeKey) throws RemoteException;

	/**
	 * @see com.idega.content.themes.business.ThemesEngineBean#applyMultipleChangesToTheme
	 */
	public String applyMultipleChangesToTheme(String themeKey, List<ThemeChange> changes, String themeName) throws RemoteException;
	
	/**
	 * @see com.idega.content.themes.business.ThemesEngineBean#clearVariationFromCache
	 */
	public boolean clearVariationFromCache(String themeKey, IWContext iwc);
	
	/**
	 * @see ThemesEngineBean#reloadThemeProperties
	 */
	public String reloadThemeProperties(String themeKey);
	
	public void updateSiteTemplatesTree(IWContext iwc, boolean sendToAllSessions);
	
	public String createChildTemplateForThisTemplate(String parentTemplateKey);
	
	public Document getUpdatedSiteTree();
	
	public Document getUpdatedSiteTemplatesTree();
	
	public boolean deleteTheme(String themeId);
	
	public boolean deleteAllThemes();
	
	public boolean setBuiltInStyle(String themeId, String builtInStyleId);
	
	public ThemeChanger getThemeChanger();
	
	public ThemesPropertiesExtractor getThemesPropertiesExtractor();
	
	public IWContext getContextAndCheckRights();
	
	public void updateSiteTree(boolean useThreads);
	
	public void updateSiteTree(boolean updateAllSessions, boolean useThreads);
	
	public void setLastUsedTemplate(String pageKey);
	
	public void setLastUsedTemplate(String pageKey, String templateKey);
	
	public boolean addExtraRegionToPage(String pageKey, AdvancedProperty region, BuilderService service);
	
	public boolean clearVariationFromCache(String themeID);
}