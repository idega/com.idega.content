package com.idega.content.themes.business;

import java.io.InputStream;
import java.rmi.RemoteException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;

import org.apache.myfaces.custom.tree2.TreeNode;
import org.directwebremoting.ScriptBuffer;
import org.directwebremoting.WebContextFactory;
import org.jdom2.Document;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.context.ApplicationListener;
import org.springframework.context.annotation.Scope;
import org.springframework.stereotype.Service;

import com.idega.builder.bean.AdvancedProperty;
import com.idega.content.business.WebDAVUploadBean;
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
import com.idega.core.builder.data.ICPageHome;
import com.idega.core.cache.IWCacheManager2;
import com.idega.core.data.ICTreeNode;
import com.idega.data.IDOLookup;
import com.idega.dwr.reverse.ScriptCaller;
import com.idega.idegaweb.IWMainApplication;
import com.idega.idegaweb.IWMainApplicationSettings;
import com.idega.idegaweb.RepositoryStartedEvent;
import com.idega.presentation.IWContext;
import com.idega.repository.RepositoryService;
import com.idega.util.CoreConstants;
import com.idega.util.CoreUtil;
import com.idega.util.IOUtil;
import com.idega.util.ListUtil;
import com.idega.util.StringUtil;
import com.idega.webface.WFUtil;

@Scope(BeanDefinition.SCOPE_SINGLETON)
@Service(ThemesEngine.SPRING_BEAN_IDENTIFIER)
public class ThemesEngineBean implements ThemesEngine, ApplicationListener<RepositoryStartedEvent> {

	private static final java.util.logging.Logger LOGGER = java.util.logging.Logger.getLogger(ThemesEngineBean.class.getName());

	public static final String ARTICLE_VIEWER_TEMPLATE_KEY = "article_viewer_page_key";
	private static final String DEFAULT_THEMES_INSTALLED_KEY = "default_themes_installed";

	@Autowired
	private ThemesHelper helper;
	@Autowired
	private ThemeChanger themeChanger;
	@Autowired
	private ThemesPropertiesExtractor themesPropertiesExtractor;
	@Autowired
	private RepositoryService repositoryService;

	private void addPropertiesList(String linkToSkeleton, List<String> pLists) {
		String base = linkToSkeleton.substring(0, linkToSkeleton.lastIndexOf(CoreConstants.SLASH));

		String pList = base.concat("/Theme.plist");
		if (!pList.startsWith(CoreConstants.WEBDAV_SERVLET_URI))
			pList = CoreConstants.WEBDAV_SERVLET_URI.concat(pList);
		try {
			if (!pLists.contains(pList) && repositoryService.getExistence(pList))
				pLists.add(pList);
		} catch(Exception e) {
			e.printStackTrace();
		}
	}

	/**
	 * Returns info about themes in repository
	 */
	@Override
	public List<SimplifiedTheme> getThemes() {
		List<SimplifiedTheme> simpleThemes = new ArrayList<SimplifiedTheme>();

		Collection<ICPage> templates = null;
		try {
			ICPageHome pageHome = (ICPageHome) IDOLookup.getHome(ICPage.class);
			templates = pageHome.findAllTemplatesWithWebDavUri();
		} catch (Exception e) {
			e.printStackTrace();
		}
		if (templates == null)
			templates = Collections.emptyList();
		else
			helper.searchForThemes(templates);

		//	Checking if exist themes in system
		Collection<Theme> themesCollection = helper.getAllThemes();
		if (ListUtil.isEmpty(themesCollection))
			return Collections.emptyList();	// No themes in system

		//	Finding configuration and properties files
		List<String> pLists = new ArrayList<String>();
		List<String> configs = null;
		List<String> predefinedThemeStyles = helper.getPredefinedThemeStyles();
		for (ICPage template: templates)
			addPropertiesList(template.getWebDavUri(), pLists);
		for (Theme theme: themesCollection)
			addPropertiesList(theme.getLinkToSkeleton(), pLists);

		//	Exists some themes, preparing for usage
		try {
			getThemesPropertiesExtractor().prepareThemes(pLists, configs, new ArrayList<String>(predefinedThemeStyles), false);
		} catch (Exception e) {
			LOGGER.log(Level.WARNING, "Error preparing theme(s): plists: " + pLists + ", configs: " + configs + ", predefined styles: " +
					predefinedThemeStyles, e);
			return null;
		}

		List<Theme> themes = helper.getSortedThemes();
		if (themes == null)
			return null;

		SimplifiedTheme simpleTheme = null;
		for (Theme theme: themes) {
			simpleTheme = getSimpleTheme(theme);

			if (simpleTheme != null) {
				simpleThemes.add(simpleTheme);
			}
		}
		return simpleThemes;
	}

	@Override
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

	private void addAllBuilderTypeTemplates(String key, List<SimplifiedTheme> childrenTemplates, BuilderService builder) {
		if (key == null) {
			return;
		}

		ICPage template = helper.getThemesService().getICPage(key);
		if (template == null) {
			return;
		}

		@SuppressWarnings("rawtypes")
		Collection children = template.getChildren();
		if (ListUtil.isEmpty(children)) {
			return;
		}

		Object o = null;
		ICPage childTemplate = null;
		for (@SuppressWarnings("rawtypes")
		Iterator it = children.iterator(); it.hasNext();) {
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
			LOGGER.log(Level.WARNING, "Error getting cache: " + ThemesConstants.THEME_STYLE_VARIATIONS_CACHE_KEY);
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

	@Override
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
	@Override
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
				LOGGER.log(Level.WARNING, "Error getting " + BuilderService.class.getName(), e);
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
	@Override
	public String changeTheme(String themeKey, String themeName, ThemeChange change) {
		try {
			return getThemeChanger().changeTheme(themeKey, themeName, change, true);
		} catch (Exception e) {
			LOGGER.log(Level.WARNING, "Error changing theme: " + themeName + "("+themeKey+"): " + change, e);
		}

		return null;
	}

	/**
	 *
	 */
	@Override
	public boolean saveTheme(String themeKey, String themeName) {
		try {
			return getThemeChanger().saveTheme(themeKey, themeName);
		} catch (Exception e) {
			LOGGER.log(Level.WARNING, "Error saving theme: " + themeKey + ", name: " + themeName, e);
		}

		return false;
	}

	/**
	 *
	 */
	@Override
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
		@SuppressWarnings("rawtypes")
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

		@SuppressWarnings("rawtypes")
		Collection pageChildren = parentPage.getChildren();
		if (pageChildren == null) {
			return true;
		}
		o = null;
		ICTreeNode childPage = null;
		for (@SuppressWarnings("rawtypes")
		Iterator it = pageChildren.iterator(); it.hasNext();) {
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
			LOGGER.log(Level.WARNING, "Error getting " + BuilderService.class.getName(), e);
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

	@Override
	public boolean addExtraRegionToPage(String pageKey, AdvancedProperty region, BuilderService service) {
		if (pageKey == null || region == null || service == null) {
			return false;
		}

		String newRegionId = region.getValue();
		return service.copyAllModulesFromRegionIntoRegion(pageKey, region.getId(), newRegionId, newRegionId);
	}

	@SuppressWarnings("rawtypes")
	private Map getTree(IWContext iwc) {
		try {
			return helper.getThemesService().getBuilderService().getTree(iwc);
		} catch(Exception e) {
			LOGGER.log(Level.WARNING, "Error getting pages tree", e);
		}
		return null;
	}

	private boolean setSiteStyle(int templateID, IWContext iwc, boolean setStyleForChildren, boolean isContentEditor, Theme theme) {
		@SuppressWarnings("rawtypes")
		Map tree = getTree(iwc);
		if (tree == null) {
			return false;
		}

		ICDomain cachedDomain = iwc.getApplicationContext().getDomain();
		boolean result = true;
		Object o = null;
		for (@SuppressWarnings("rawtypes")
		Iterator it = tree.values().iterator(); it.hasNext();) {
			o = it.next();
			if (o instanceof ICTreeNode) {
				result = setPageStyle(((ICTreeNode) o).getId(), templateID, iwc, cachedDomain, false, isContentEditor, theme);
			}
		}
		return result;
	}

	@Override
	public boolean restoreTheme(String themeID) {
		try {
			return getThemeChanger().restoreTheme(themeID);
		} catch (Exception e) {
			LOGGER.log(Level.WARNING, "Error restoring theme: " + themeID, e);
		}

		return false;
	}

	@Override
	public void updateSiteTemplatesTree(boolean sendToAllSessions) {
		StringBuffer uri = new StringBuffer(CoreConstants.SLASH).append(CoreConstants.WORKSPACE_VIEW_MANAGER_ID).append(CoreConstants.SLASH);
		uri.append(CoreConstants.CONTENT_VIEW_MANAGER_ID).append(CoreConstants.SLASH).append(CoreConstants.PAGES_VIEW_MANAGER_ID).append(CoreConstants.SLASH);
		Thread scriptCaller = new Thread(new ScriptCaller(WebContextFactory.get(), new ScriptBuffer("getUpdatedSiteTemplatesTreeFromServer();"), uri.toString(),
				sendToAllSessions));
		scriptCaller.start();
	}

	@Override
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

	@Override
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

	@Override
	public void updateSiteTree(boolean updateAllSessions, boolean useThreads) {
		Thread scriptCaller = new Thread(new ScriptCaller(WebContextFactory.get(), new ScriptBuffer("getUpdatedSiteTreeFromServer();"), updateAllSessions));
		if (!useThreads) {
			scriptCaller.run();
			return;
		}
		scriptCaller.start();
	}

	@Override
	public void updateSiteTree(boolean useThreads) {
		updateSiteTree(false, useThreads);
	}

	@Override
	public void setLastUsedTemplate(String pageKey) {
		setLastUsedTemplate(pageKey, helper.getLastUsedTheme());
	}

	@Override
	public void setLastUsedTemplate(String pageKey, String templateKey) {
		if (pageKey == null || templateKey == null) {
			return;
		}
		helper.getThemesService().getBuilderService().setTemplateId(pageKey, templateKey);
	}

	@Override
	public String applyMultipleChangesToTheme(String themeID, List<ThemeChange> changes, String themeName) {
		try {
			return getThemeChanger().applyMultipleChangesToTheme(themeID, changes, themeName);
		} catch (Exception e) {
			LOGGER.log(Level.WARNING, "Error applying multiple changes for theme: " + themeID, e);
		}

		return null;
	}

	@Override
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
			LOGGER.log(Level.WARNING, "Error reloading theme: " + themeId, e);
			return null;
		}

		return null;
	}

	@Override
	public String createChildTemplateForThisTemplate(String parentTemplateKey) {
		String newTemplateId = helper.getThemesService().createChildTemplateForThisTemplate(parentTemplateKey);
		if (newTemplateId == null) {
			return null;
		}

		updateSiteTemplatesTree(true);

		return newTemplateId;
	}

	@Override
	public boolean setBuiltInStyle(String themeId, String builtInStyleId) {
		try {
			return getThemeChanger().setBuiltInStyle(themeId, builtInStyleId);
		} catch(Exception e) {
			LOGGER.log(Level.WARNING, "Error setting built-in style for theme: " + themeId + ", style id: " + builtInStyleId, e);
		}
		return false;
	}

	@Override
	public ThemeChanger getThemeChanger() {
		return themeChanger;
	}

	public void setThemeChanger(ThemeChanger themeChanger) {
		this.themeChanger = themeChanger;
	}

	@Override
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

	@Override
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

	@Override
	public boolean deleteAllThemes() {
		IWContext iwc = getContextAndCheckRights();
		if (iwc == null) {
			return false;
		}

		List<Theme> themes = helper.getAvailableThemes();
		if (ListUtil.isEmpty(themes)) {
			return false;
		}
		for (Theme theme: themes) {
			if (!deleteTheme(theme)) {
				return false;
			}
		}

		return true;
	}

	@Override
	public boolean deleteTheme(String themeId) {
		IWContext iwc = getContextAndCheckRights();
		if (iwc == null) {
			return false;
		}

		Theme theme = helper.getTheme(themeId);
		return deleteTheme(theme);
	}

	private boolean deleteTheme(Theme theme) {
		if (theme == null) {
			LOGGER.warning("Theme is unknown!");
			return false;
		}

		String path = theme.getLinkToSkeleton();
		try {
			if (!repositoryService.deleteAsRootUser(path)) {
				LOGGER.warning("Unable to delete: " + path);
				return false;
			}
		} catch (Exception e) {
			LOGGER.log(Level.WARNING, "Unable to delete: " + path, e);
			return false;
		}
		return true;
	}

	@Override
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
			LOGGER.log(Level.WARNING, "Error cleaninf cache for theme: " + themeID, e);
			return false;
		}

		return true;
	}

	@Override
	public void onApplicationEvent(RepositoryStartedEvent event) {
			final IWMainApplicationSettings settings = event.getIWMA().getSettings();
			if (!settings.getBoolean(DEFAULT_THEMES_INSTALLED_KEY, Boolean.FALSE) && settings.getBoolean("auto_load_themes", Boolean.TRUE)) {
				if (ListUtil.isEmpty(getThemes())) {
					Thread themesInstaller = new Thread(new Runnable() {
						@Override
						public void run() {
							Boolean result = installDefaultThemes();
							settings.setProperty(DEFAULT_THEMES_INSTALLED_KEY, result.toString());
							if (result) {
								getThemes();
							}
						}
					});
					themesInstaller.start();
				}
			}
	}

	private boolean installDefaultThemes() {
		WebDAVUploadBean wub = new WebDAVUploadBean();
		for (AdvancedProperty theme: ThemesConstants.DEFAULT_THEMES) {
			if (!installTheme(wub, theme.getId(), helper.getInputStream(theme.getValue()))) {
				return false;
			}
		}
		return true;
	}

	private boolean installTheme(WebDAVUploadBean uploadBean, String fileName, InputStream stream) {
		try {
			uploadBean.setUploadFilePath(ThemesConstants.THEMES_PATH);
			return uploadBean.uploadZipFile(true, fileName, stream);
		} catch (Exception e) {
			LOGGER.log(Level.WARNING, "Error installing theme: " + fileName, e);
		} finally {
			IOUtil.close(stream);
		}
		return false;
	}
}