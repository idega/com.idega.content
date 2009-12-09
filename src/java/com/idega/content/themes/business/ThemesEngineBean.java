package com.idega.content.themes.business;

import java.rmi.RemoteException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;

import org.apache.myfaces.custom.tree2.TreeNode;
import org.directwebremoting.ScriptBuffer;
import org.directwebremoting.WebContextFactory;
import org.jdom.Document;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.context.annotation.Scope;
import org.springframework.stereotype.Service;

import com.idega.builder.bean.AdvancedProperty;
import com.idega.content.themes.bean.ThemesManagerBean;
import com.idega.content.themes.helpers.bean.SimplifiedTheme;
import com.idega.content.themes.helpers.bean.Theme;
import com.idega.content.themes.helpers.bean.ThemeChange;
import com.idega.content.themes.helpers.business.ThemeChanger;
import com.idega.content.themes.helpers.business.ThemesConstants;
import com.idega.content.themes.helpers.business.ThemesHelper;
import com.idega.content.themes.helpers.business.ThemesPropertiesExtractor;
import com.idega.content.themes.presentation.SiteTreeViewer;
import com.idega.content.themes.presentation.TemplatesTree;
import com.idega.content.themes.presentation.ThemeStyleVariations;
import com.idega.core.accesscontrol.business.StandardRoles;
import com.idega.core.builder.business.BuilderService;
import com.idega.core.builder.business.BuilderServiceFactory;
import com.idega.core.builder.data.ICDomain;
import com.idega.core.builder.data.ICPage;
import com.idega.core.cache.IWCacheManager2;
import com.idega.core.data.ICTreeNode;
import com.idega.core.search.business.SearchResult;
import com.idega.dwr.reverse.ScriptCaller;
import com.idega.idegaweb.IWMainApplication;
import com.idega.presentation.IWContext;
import com.idega.slide.business.IWSlideService;
import com.idega.util.CoreConstants;
import com.idega.util.CoreUtil;
import com.idega.util.ListUtil;
import com.idega.util.StringUtil;
import com.idega.webface.WFUtil;

@Scope(BeanDefinition.SCOPE_SINGLETON)
@Service(ThemesEngine.SPRING_BEAN_IDENTIFIER)
public class ThemesEngineBean implements ThemesEngine {

	private static final long serialVersionUID = 5875353284352953688L;
	
	private static final java.util.logging.Logger LOGGER = java.util.logging.Logger.getLogger(ThemesEngineBean.class.getName());
	
	public static final String ARTICLE_VIEWER_TEMPLATE_KEY = "article_viewer_page_key";
	
	@Autowired
	private ThemesHelper helper;
	@Autowired
	private ThemeChanger themeChanger;
	@Autowired
	private ThemesPropertiesExtractor themesPropertiesExtractor;

	/**
	 * Returns info about themes in Slide
	 */
	public List<SimplifiedTheme> getThemes() {
		List <SimplifiedTheme> simpleThemes = new ArrayList<SimplifiedTheme>();
		
		List<String> pLists = null;
		List<String> configs = null;
		List<String> predefinedThemeStyles = helper.getPredefinedThemeStyles();
		if (!helper.isCheckedFromSlide()) {
			String searchScope = new StringBuffer(CoreConstants.WEBDAV_SERVLET_URI).append(ThemesConstants.THEMES_PATH).toString();
			
			String propSearchKey = new StringBuffer("*").append(ThemesConstants.THEME_PROPERTIES_FILE_END).toString();
			List<SearchResult> propertiesLists = helper.search(propSearchKey, searchScope);
			pLists = helper.loadSearchResults(propertiesLists, null);
			
			String configSearchKey = new StringBuffer("*").append(ThemesConstants.IDEGA_THEME_INFO).toString();
			List<SearchResult> configurationXmls = helper.search(configSearchKey, searchScope);
			configs = helper.loadSearchResults(configurationXmls, null);
			
			String predefinedThemeStyleSearchKey = new StringBuffer("*").append(ThemesConstants.THEME_PREDEFINED_STYLE_CONFIG_FILE).toString();
			List<SearchResult> predefinedStyles = helper.search(predefinedThemeStyleSearchKey, searchScope);
			predefinedThemeStyles.addAll(helper.loadSearchResults(predefinedStyles, null));
		}
		
		helper.searchForThemes();

		//	Checking if exist themes in system
		Collection<Theme> themesCollection = helper.getAllThemes();
		if (themesCollection == null || themesCollection.size() == 0) {
			return null;	// No themes in system
		}
		
		//	Exists some themes, preparing for usage
		try {
			getThemesPropertiesExtractor().prepareThemes(pLists, configs, new ArrayList<String>(predefinedThemeStyles), false);
		} catch (Exception e) {
			e.printStackTrace();
			return null;
		}
		
		List <Theme> themes = helper.getSortedThemes();
		if (themes == null) {
			return null;
		}
		SimplifiedTheme simpleTheme = null;
		for (int i = 0; i < themes.size(); i++) {
			simpleTheme = getSimpleTheme(themes.get(i));
			
			if (simpleTheme != null) {
				simpleThemes.add(simpleTheme);
			}
		}
		return simpleThemes;
	}
	
	public SimplifiedTheme getTheme(String themeId) {
		if (themeId == null) {
			return null;
		}
		
		Theme theme = helper.getTheme(themeId);
		if (theme == null) {
			return null;
		}
		
		return getSimpleTheme(theme);
	}
	
	private SimplifiedTheme getSimpleTheme(Theme theme) {
		if (theme.isPropertiesExtracted()) {
			StringBuffer link = null;
			
			SimplifiedTheme simpleTheme = new SimplifiedTheme();
			
			// Name
			if (theme.getChangedName() == null) {
				simpleTheme.setName(theme.getName());
			}
			else {
				simpleTheme.setName(theme.getChangedName());
			}
			
			if (theme.getLinkToSmallPreview() != null) {
				// Small preview
				link = new StringBuffer(CoreConstants.WEBDAV_SERVLET_URI).append(theme.getLinkToBase());
				link.append(helper.encode(theme.getLinkToSmallPreview(), true));
				simpleTheme.setLinkToSmallPreview(link.toString());
			}
			
			// Big preview
			link = new StringBuffer(CoreConstants.WEBDAV_SERVLET_URI);
			if (StringUtil.isEmpty(theme.getLinkToDraft())) {
				link.append(theme.getLinkToSkeleton());
			}
			else {
				link.append(theme.getLinkToDraft());
			}
			simpleTheme.setLinkToBigPreview(link.toString());
			
			// Id
			simpleTheme.setId(theme.getId());
			
			// Is used?
			simpleTheme.setUsed(isUsedTheme(theme.getIBPageID()));
			
			addTemplatesAsChildrenToTheme(simpleTheme, theme.getIBPageID());
			
			return simpleTheme;
		}
		
		return null;
	}
	
	private void addTemplatesAsChildrenToTheme(SimplifiedTheme theme, int templateId) {
		if (theme == null) {
			return;
		}
		
		List<SimplifiedTheme> templates = new ArrayList<SimplifiedTheme>();
		
		addAllBuilderTypeTemplates(String.valueOf(templateId), templates, helper.getThemesService().getBuilderService());
		
		if (templates.size() > 0) {
			theme.setChildren(templates);
		}
	}
	
	@SuppressWarnings("unchecked")
	private void addAllBuilderTypeTemplates(String key, List<SimplifiedTheme> childrenTemplates, BuilderService builder) {
		if (key == null) {
			return;
		}
		
		ICPage template = helper.getThemesService().getICPage(key);
		if (template == null) {
			return;
		}
		
		Collection children = template.getChildren();
		if (children == null || children.size() == 0) {
			return;
		}
		
		Object o = null;
		ICPage childTemplate = null;
		for (Iterator it = children.iterator(); it.hasNext();) {
			o = it.next();
			
			if (o instanceof ICPage) {
				childTemplate = (ICPage) o;
				
				if (builder.getTemplateKey().equals(childTemplate.getType()) && builder.getIBXMLFormat().equals(childTemplate.getFormat())) {
					String templateId = childTemplate.getId();
					
					childrenTemplates.add(new SimplifiedTheme(templateId, childTemplate.getName()));
					
					addAllBuilderTypeTemplates(templateId, childrenTemplates, builder);
				}
			}
		}
	}
	
	private boolean isUsedTheme(int templateID) {
		if (templateID == -1) {
			return false;
		}
		String id = helper.getDefaultTheme();
		if (id == null) {
			return false;
		}
		if (id.equals(String.valueOf(templateID))) {
			return true;
		}
		return false;
	}
	
	private Map<String, String> getVariationsCache(IWContext iwc) {
		IWCacheManager2 cache = IWCacheManager2.getInstance(iwc.getIWMainApplication());
		if (cache == null) {
			return null;
		}
		
		try {
			return cache.getCache(ThemesConstants.THEME_STYLE_VARIATIONS_CACHE_KEY);
		} catch(Exception e) {
			e.printStackTrace();
		}
		
		return null;
	}
	
	private void putVariationsToCache(String variations, IWContext iwc, String themeID) {
		if (variations == null || iwc == null || themeID == null) {
			return;
		}
		Map<String, String> variationsCache = getVariationsCache(iwc);
		if (variationsCache == null) {
			return;
		}
		
		variationsCache.put(themeID, variations);
	}
	
	public boolean clearVariationFromCache(String themeID, IWContext iwc) {
		if (themeID == null) {
			return false;
		}
		if (iwc == null) {
			iwc = getContextAndCheckRights();
			if (iwc == null) {
				return false;
			}
		}
		Map<String, String> variations = getVariationsCache(iwc);
		if (variations == null) {
			return false;
		}
		String removed = variations.remove(themeID);
		if (removed == null) {
			return false;
		}
		return true;
	}
	
	private String getVariationsFromCache(String themeID, IWContext iwc) {
		Map<String, String> variations = getVariationsCache(iwc);
		if (variations == null) {
			return null;
		}
		return variations.get(themeID);
	}
	
	/**
	 * 
	 */
	public String getThemeStyleVariations(String themeID) {
		if (themeID == null) {
			return null;
		}
		
		IWContext iwc = getContextAndCheckRights();
		if (iwc == null) {
			return null;
		}
		
		String cachedVariations = getVariationsFromCache(themeID, iwc);
		if (cachedVariations != null) {
			return cachedVariations;
		}
		
		BuilderService service = helper.getThemesService().getBuilderService();
		if (service == null) {
			try {
				service = BuilderServiceFactory.getBuilderService(iwc);
			} catch (RemoteException e) {
				e.printStackTrace();
				return null;
			}
		}

		WFUtil.invoke(ThemesManagerBean.THEMES_MANAGER_BEAN_ID, "setThemeId", themeID, String.class);
		String variations = service.getRenderedComponent(new ThemeStyleVariations(), iwc, false);
		putVariationsToCache(variations, iwc, themeID);
		return variations;
	}
	
	/**
	 * 
	 */
	public String changeTheme(String themeKey, String themeName, ThemeChange change) {
		try {
			return getThemeChanger().changeTheme(themeKey, themeName, change, true);
		} catch (Exception e) {
			e.printStackTrace();
		}
		
		return null;
	}
	
	/**
	 * 
	 */
	public boolean saveTheme(String themeKey, String themeName) {
		try {
			return getThemeChanger().saveTheme(themeKey, themeName);
		} catch (Exception e) {
			e.printStackTrace();
		}
		
		return false;
	}
	
	/**
	 * 
	 */
	public boolean setSelectedStyle(String themeKey, String pageKey, Integer type, Integer templateId) {
		if (type == null) {
			return false;
		}
		
		IWContext iwc = getContextAndCheckRights();
		if (iwc == null) {
			return false;
		}
		
		if (themeKey == null) {
			return false;
		}
		Theme theme = helper.getTheme(themeKey);
		if (theme == null) {
			return false;
		}
		
		boolean isContentEditor = iwc.hasRole(StandardRoles.ROLE_KEY_EDITOR);
		boolean result = true;
		boolean applyToPage = true;
		if (pageKey == null) {
			applyToPage = false;
		}
		
		if (templateId == null) {
			templateId = theme.getIBPageID();
		}
		
		if (applyToPage) {
			//	Apply style to selected page
			result = setPageStyle(pageKey, templateId, iwc, null, type == 0 ? false : true, isContentEditor, theme);
		}
		else {
			//	Apply style to all pages
			result = setSiteStyle(templateId, iwc, false, isContentEditor, theme);
		}
		
		if (result) {
			helper.getThemesService().getBuilderService().clearAllCachedPages();
		}
		
		return result;
	}
	
	private boolean setPageStyle(String pageKey, int templateKey, IWContext iwc, ICDomain cachedDomain, boolean setStyleForChildren, boolean isContentEditor,
			Theme theme) {
		boolean result = setStyle(theme, pageKey, templateKey, false, isContentEditor);
		if (!result) {
			return false;
		}
		if (cachedDomain == null) {
			cachedDomain = iwc.getApplicationContext().getDomain();
		}
		if (cachedDomain == null) {
			return result;
		}
		int startPageID = cachedDomain.getStartPageID();
		if (startPageID == Integer.valueOf(pageKey).intValue()) {
			//	Setting the same style as front page has
			String articleViewerID = iwc.getApplicationSettings().getProperty(ARTICLE_VIEWER_TEMPLATE_KEY);
			if (articleViewerID != null) {
				result = setStyle(theme, articleViewerID, templateKey, true, isContentEditor);
			}
		}
		
		if (setStyleForChildren) {
			return setStyleForChildren(pageKey, templateKey, iwc, cachedDomain, isContentEditor, theme);
		}
		
		return result;
	}
	
	@SuppressWarnings("unchecked")
	private boolean setStyleForChildren(String pageKey, int templateKey, IWContext iwc, ICDomain cachedDomain, boolean isContentEditor, Theme theme) {
		Map tree = getTree(iwc);
		if (tree == null) {
			return true;
		}
		
		ICTreeNode parentPage = null;
		boolean foundParent = false;
		Object o = null;
		for (Iterator<ICTreeNode> it = tree.values().iterator(); (it.hasNext() && !foundParent);) {
			o = it.next();
			if (o instanceof ICTreeNode) {
				parentPage = (ICTreeNode) o;
				if (pageKey.equals(parentPage.getId())) {
					foundParent = true;
				}
			}
		}
		if (!foundParent) {
			return true;
		}
		if (parentPage == null) {
			return true;
		}
		
		Collection pageChildren = parentPage.getChildren();
		if (pageChildren == null) {
			return true;
		}
		o = null;
		ICTreeNode childPage = null;
		for (Iterator it = pageChildren.iterator(); it.hasNext();) {
			o = it.next();
			if (o instanceof ICTreeNode) {
				childPage = (ICTreeNode) o;
				setPageStyle(childPage.getId(), templateKey, iwc, cachedDomain, true, isContentEditor, theme);
			}
		}
		
		return true;
	}
	
	private boolean setStyle(Theme theme, String pageKey, int templateId, boolean ignoreTemplate, boolean isContentEditor) {
		ICPage page = null;
		if (templateId < 0) {
			return false;
		}
		page = helper.getThemesService().getICPage(pageKey);
		if (page == null) {
			return false;
		}
		
		if (!isContentEditor) {
			if (page.isPage() && page.isPublished()) {
				//	Insufficient rights
				return false;
			}
		}
		
		String templateKey = String.valueOf(templateId); 
		if (page.isPage() || ignoreTemplate) {
			helper.getThemesService().getBuilderService().setTemplateId(pageKey, templateKey);
			page.setTemplateId(templateId);
			helper.setLastUsedTheme(templateKey);
			
			if (!checkIfNeedExtraRegions(pageKey, theme)) {
				page.store();
			}
		}
		return true;
	}
	
	private boolean checkIfNeedExtraRegions(String pageKey, Theme theme) {
		if (pageKey == null || theme == null) {
			return false;
		}
		
		BuilderService service = null;
		try {
			service = BuilderServiceFactory.getBuilderService(IWMainApplication.getDefaultIWApplicationContext());
		} catch (RemoteException e) {
			e.printStackTrace();
			return false;
		}
		if (service == null) {
			return false;
		}
		
		List<AdvancedProperty> regions = theme.getExtraRegions();
		if (regions == null || regions.size() == 0) {
			return false;
		}
		
		AdvancedProperty region = null;
		for (int i = 0; i < regions.size(); i++) {
			region = regions.get(i);
			if (!service.existsRegion(pageKey, region.getValue(), region.getId())) {
				addExtraRegionToPage(pageKey, region, service);
			}
		}
		
		return false;
	}
	
	public boolean addExtraRegionToPage(String pageKey, AdvancedProperty region, BuilderService service) {
		if (pageKey == null || region == null || service == null) {
			return false;
		}
		
		String newRegionId = region.getValue();
		return service.copyAllModulesFromRegionIntoRegion(pageKey, region.getId(), newRegionId, newRegionId);
	}
	
	@SuppressWarnings("unchecked")
	private Map getTree(IWContext iwc) {
		try {
			return helper.getThemesService().getBuilderService().getTree(iwc);
		} catch(Exception e) {
			e.printStackTrace();
		}
		return null;
	}
	
	@SuppressWarnings("unchecked")
	private boolean setSiteStyle(int templateID, IWContext iwc, boolean setStyleForChildren, boolean isContentEditor, Theme theme) {
		Map tree = getTree(iwc);
		if (tree == null) {
			return false;
		}
		
		ICDomain cachedDomain = iwc.getApplicationContext().getDomain();
		boolean result = true;
		Object o = null;
		for (Iterator it = tree.values().iterator(); it.hasNext();) {
			o = it.next();
			if (o instanceof ICTreeNode) {
				result = setPageStyle(((ICTreeNode) o).getId(), templateID, iwc, cachedDomain, false, isContentEditor, theme);
			}
		}
		return result;
	}
	
	public boolean restoreTheme(String themeID) {
		try {
			return getThemeChanger().restoreTheme(themeID);
		} catch (Exception e) {
			e.printStackTrace();
		}
		
		return false;
	}
	
	public void updateSiteTemplatesTree(IWContext iwc, boolean sendToAllSessions) {
		StringBuffer uri = new StringBuffer(CoreConstants.SLASH).append(CoreConstants.WORKSPACE_VIEW_MANAGER_ID).append(CoreConstants.SLASH);
		uri.append(CoreConstants.CONTENT_VIEW_MANAGER_ID).append(CoreConstants.SLASH).append(CoreConstants.PAGES_VIEW_MANAGER_ID).append(CoreConstants.SLASH);
		Thread scriptCaller = new Thread(new ScriptCaller(WebContextFactory.get(), new ScriptBuffer("getUpdatedSiteTemplatesTreeFromServer();"), uri.toString(),
				sendToAllSessions));
		scriptCaller.start();
	}
	
	public Document getUpdatedSiteTemplatesTree() {
		IWContext iwc = getContextAndCheckRights();
		if (iwc == null) {
			return null;
		}
		
		BuilderService service = helper.getThemesService().getBuilderService();
		if (service == null) {
			return null;
		}
		
		return service.getRenderedComponent(iwc, new TemplatesTree(), false);
	}
	
	public Document getUpdatedSiteTree() {
		IWContext iwc = getContextAndCheckRights();
		if (iwc == null) {
			return null;
		}
		
		SiteTreeViewer tree = (SiteTreeViewer) iwc.getApplication().createComponent(SiteTreeViewer.COMPONENT_TYPE);
		Object o = WFUtil.getValue("pageCreationBean", "pageSelectorTopNode");
		if (o instanceof TreeNode) {
			tree.setRootNode((TreeNode) o);
		}
		else {
			return null;
		}
		
		BuilderService service = helper.getThemesService().getBuilderService();
		if (service == null) {
			return null;
		}
		
		return service.getRenderedComponent(iwc, tree, true);
	}
	
	public void updateSiteTree(boolean updateAllSessions, boolean useThreads) {
		Thread scriptCaller = new Thread(new ScriptCaller(WebContextFactory.get(), new ScriptBuffer("getUpdatedSiteTreeFromServer();"), updateAllSessions));
		if (!useThreads) {
			scriptCaller.run();
			return;
		}
		scriptCaller.start();
	}
	
	public void updateSiteTree(boolean useThreads) {
		updateSiteTree(false, useThreads);
	}
	
	public void setLastUsedTemplate(String pageKey) {
		setLastUsedTemplate(pageKey, helper.getLastUsedTheme());
	}
	
	public void setLastUsedTemplate(String pageKey, String templateKey) {
		if (pageKey == null || templateKey == null) {
			return;
		}
		helper.getThemesService().getBuilderService().setTemplateId(pageKey, templateKey);
	}
	
	public String applyMultipleChangesToTheme(String themeID, List<ThemeChange> changes, String themeName) {
		try {
			return getThemeChanger().applyMultipleChangesToTheme(themeID, changes, themeName);
		} catch (Exception e) {
			e.printStackTrace();
		}
		
		return null;
	}
	
	public String reloadThemeProperties(String themeId) {
		ThemeChanger changer = getThemeChanger();
		if (changer == null) {
			return null;
		}
		
		try {
			if (changer.reloadThemeProperties(themeId, true)) {
				return getThemeStyleVariations(themeId);
			}
		} catch (Exception e) {
			e.printStackTrace();
			return null;
		}
		
		return null;
	}
	
	public String createChildTemplateForThisTemplate(String parentTemplateKey) {
		String newTemplateId = helper.getThemesService().createChildTemplateForThisTemplate(parentTemplateKey);
		if (newTemplateId == null) {
			return null;
		}
		
		updateSiteTemplatesTree(getContextAndCheckRights(), true);
		
		return newTemplateId;
	}

	public boolean setBuiltInStyle(String themeId, String builtInStyleId) {
		try {
			return getThemeChanger().setBuiltInStyle(themeId, builtInStyleId);
		} catch(Exception e) {
			e.printStackTrace();
		}
		return false;
	}
	
	public ThemeChanger getThemeChanger() {
		return themeChanger;
	}

	public void setThemeChanger(ThemeChanger themeChanger) {
		this.themeChanger = themeChanger;
	}

	public ThemesPropertiesExtractor getThemesPropertiesExtractor() {
		return themesPropertiesExtractor;
	}

	public void setThemesPropertiesExtractor(ThemesPropertiesExtractor themesPropertiesExtractor) {
		this.themesPropertiesExtractor = themesPropertiesExtractor;
	}

	public ThemesHelper getHelper() {
		return helper;
	}

	public void setHelper(ThemesHelper helper) {
		this.helper = helper;
	}

	public IWContext getContextAndCheckRights() {
		IWContext iwc = CoreUtil.getIWContext();
		if (iwc == null) {
			LOGGER.warning("Instance of " + IWContext.class + " is not set for the current thread!");
			return null;
		}
		
		if (iwc.isSuperAdmin() || (iwc.hasRole(StandardRoles.ROLE_KEY_AUTHOR) || iwc.hasRole(StandardRoles.ROLE_KEY_EDITOR))) {
			return iwc;
		}
		
		LOGGER.warning("Current user has no rights to work in Lucid!");
		return null;
	}
	
	public boolean deleteAllThemes() {
		IWContext iwc = getContextAndCheckRights();
		if (iwc == null) {
			return false;
		}
		
		List<Theme> themes = helper.getAvailableThemes();
		if (ListUtil.isEmpty(themes)) {
			return false;
		}
		IWSlideService slide = helper.getSlideService(IWMainApplication.getDefaultIWApplicationContext());
		for (Theme theme: themes) {
			if (!deleteTheme(theme, slide)) {
				return false;
			}
		}
		
		return true;
	}
	
	public boolean deleteTheme(String themeId) {
		IWContext iwc = getContextAndCheckRights();
		if (iwc == null) {
			return false;
		}
		
		Theme theme = helper.getTheme(themeId);
		IWSlideService slide = helper.getSlideService(IWMainApplication.getDefaultIWApplicationContext());
		
		return deleteTheme(theme, slide);
	}
	
	private boolean deleteTheme(Theme theme, IWSlideService slide) {
		if (slide == null) {
			return false;
		}
		
		if (theme == null) {
			LOGGER.warning("Theme is unknown!");
			return false;
		}
		
		String path = theme.getLinkToSkeleton();
		try {
			if (!getHelper().getSlideService().deleteAsRootUser(path)) {
				LOGGER.warning("Unable to delete: " + path);
				return false;
			}
		} catch (Exception e) {
			LOGGER.log(Level.WARNING, "Unable to delete: " + path, e);
			return false;
		}
		return true;
	}
	
	public boolean clearVariationFromCache(String themeID) {
		if (themeID == null) {
			return false;
		}
		Theme theme = helper.getTheme(themeID);
		if (theme == null) {
			return false;
		}
		List<String> keys = theme.getStyleVariationsCacheKeys();
		if (keys == null) {
			return false;
		}
		IWContext iwc = getContextAndCheckRights();
		if (iwc == null) {
			return false;
		}
		
		//	Removing Block from cache
		for (int i = 0; i < keys.size(); i++) {
			helper.getThemesService().getBuilderService().removeBlockObjectFromCache(iwc, keys.get(i));
		}
		
		//	Removing cache keys
		theme.clearStyleVariationsCacheKeys();
		
		//	Removing rendered variations from cache
		try {
			clearVariationFromCache(themeID, iwc);
		} catch(Exception e) {
			e.printStackTrace();
			return false;
		}
		
		return true;
	}
}