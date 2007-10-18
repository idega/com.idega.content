package com.idega.content.themes.business;

import java.rmi.RemoteException;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.idega.business.IBOServiceBean;
import com.idega.content.business.ContentConstants;
import com.idega.content.business.ContentUtil;
import com.idega.content.themes.bean.ThemesManagerBean;
import com.idega.content.themes.helpers.Setting;
import com.idega.content.themes.helpers.SimplifiedTheme;
import com.idega.content.themes.helpers.Theme;
import com.idega.content.themes.helpers.ThemeChange;
import com.idega.content.themes.helpers.ThemeStyleGroupMember;
import com.idega.content.themes.helpers.ThemesConstants;
import com.idega.content.themes.helpers.ThemesHelper;
import com.idega.content.themes.helpers.TreeNodeStructure;
import com.idega.content.themes.presentation.ThemeStyleVariations;
import com.idega.core.builder.business.BuilderService;
import com.idega.core.builder.business.BuilderServiceFactory;
import com.idega.core.builder.data.CachedDomain;
import com.idega.core.builder.data.ICDomain;
import com.idega.core.builder.data.ICPage;
import com.idega.core.cache.IWCacheManager2;
import com.idega.core.data.ICTreeNode;
import com.idega.core.localisation.business.ICLocaleBusiness;
import com.idega.core.search.business.SearchResult;
import com.idega.data.TreeableEntity;
import com.idega.idegaweb.IWMainApplication;
import com.idega.idegaweb.IWMainApplicationSettings;
import com.idega.idegaweb.IWResourceBundle;
import com.idega.presentation.IWContext;
import com.idega.servlet.filter.IWWelcomeFilter;
import com.idega.util.CoreConstants;
import com.idega.util.CoreUtil;
import com.idega.webface.WFUtil;

public class ThemesEngineBean extends IBOServiceBean implements ThemesEngine {

	private static final long serialVersionUID = 5875353284352953688L;
	private static final Log log = LogFactory.getLog(ThemesEngineBean.class);
	
	private static final String PAGE_URI = "pageUri";
	private static final String PAGE_TITLE = "pageTitle";
	private static final String PATH_TO_IMAGE_FOLDER = ContentUtil.getBundle().getResourcesPath() + "/images/";
	
	private static final String ARTICLE_VIEWER_NAME = "Article Viewer";
	private static final String ARTICLE_VIEWER_SUBTYPE = "viewer";
	
	private static final String ARTICLE_VIEWER_TEMPLATE_KEY = "article_viewer_page_key";
	
	private ThemesHelper helper = ThemesHelper.getInstance(false);

	/**
	 * Returns info about themes in slide
	 */
	public List<SimplifiedTheme> getThemes() {
		List <SimplifiedTheme> simpleThemes = new ArrayList<SimplifiedTheme>();
		
		List<String> pLists = null;
		List<String> configs = null;
		if (!helper.isCheckedFromSlide()) {
			String searchScope = new StringBuffer(CoreConstants.WEBDAV_SERVLET_URI).append(ThemesConstants.THEMES_PATH).toString();
			
			String propSearchKey = new StringBuffer("*").append(ThemesConstants.THEME_PROPERTIES_FILE_END).toString();
			List<SearchResult> propertiesLists = helper.search(propSearchKey, searchScope);
			pLists = helper.loadSearchResults(propertiesLists, null);
			
			String configSearchKey = new StringBuffer("*").append(ThemesConstants.IDEGA_THEME_INFO).toString();
			List<SearchResult> configurationXmls = helper.search(configSearchKey, searchScope);
			configs = helper.loadSearchResults(configurationXmls, null);
		}
		
		helper.searchForThemes();

		//	Checking if exist themes in system
		Collection<Theme> themesCollection = helper.getThemesCollection();
		if (themesCollection == null) {
			return simpleThemes;	// No themes in system
		}
		int themesCount = themesCollection.size();
		if (themesCount == 0) {
			return simpleThemes;	// No themes in system
		}
		
		//	Preparing themes
		helper.getThemesPropertiesExtractor().prepareThemes(pLists, configs, false);
		
		List <Theme> themes = helper.getSortedThemes();
		Theme theme = null;
		SimplifiedTheme simpleTheme = null;
		StringBuffer link = null;
		for (int i = 0; i < themes.size(); i++) {
			theme = themes.get(i);
			if (theme.isPropertiesExtracted()) {
				simpleTheme = new SimplifiedTheme();
				
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
				link = new StringBuffer(CoreConstants.WEBDAV_SERVLET_URI).append(theme.getLinkToBase());
				if (theme.getLinkToDraftPreview() == null) {
					link.append(helper.encode(theme.getLinkToThemePreview(), true));
				}
				else {
					link.append(helper.encode(theme.getLinkToDraftPreview(), true));
				}
				simpleTheme.setLinkToBigPreview(link.toString());
				
				// Id
				simpleTheme.setId(theme.getId());
				
				// Is used?
				simpleTheme.setUsed(isUsedTheme(theme.getIBPageID()));
				
				simpleThemes.add(simpleTheme);
			}
		}
		return simpleThemes;
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
	
	@SuppressWarnings("unchecked")
	private Map getVariationsCache(IWContext iwc) {
		IWCacheManager2 cache = IWCacheManager2.getInstance(iwc.getIWMainApplication());
		if (cache == null) {
			return null;
		}
		return cache.getCache(ThemesConstants.THEME_STYLE_VARIATIONS_CACHE_KEY);
	}
	
	@SuppressWarnings("unchecked")
	private void putVariationsToCache(String variations, IWContext iwc, String themeID) {
		if (variations == null || iwc == null || themeID == null) {
			return;
		}
		Map variationsCache = getVariationsCache(iwc);
		variationsCache.put(themeID, variations);
	}
	
	@SuppressWarnings("unchecked")
	public boolean clearVariationFromCache(String themeID, IWContext iwc) {
		if (themeID == null) {
			return false;
		}
		if (iwc == null) {
			iwc = CoreUtil.getIWContext();
			if (iwc == null) {
				return false;
			}
		}
		Map variations = getVariationsCache(iwc);
		Object removed = variations.remove(themeID);
		if (removed == null) {
			return false;
		}
		return true;
	}
	
	@SuppressWarnings("unchecked")
	private String getVariationsFromCache(String themeID, IWContext iwc) {
		Map variations = getVariationsCache(iwc);
		Object o = variations.get(themeID);
		if (o instanceof String) {
			return (String) o;
		}
		return null;
	}
	
	/**
	 * 
	 */
	public String getThemeStyleVariations(String themeID) {
		if (themeID == null) {
			return null;
		}
		
		IWContext iwc = CoreUtil.getIWContext();
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
	public String changeTheme(String themeID, String styleGroupName, String styleMember, String themeName, boolean isRadio, boolean isChecked) {
		return helper.getThemeChanger().changeTheme(themeID, styleGroupName, styleMember, themeName, isRadio, isChecked);
	}
	
	/**
	 * 
	 */
	public boolean saveTheme(String themeID, String themeName) {
		return helper.getThemeChanger().saveTheme(themeID, themeName);
	}
	
	/**
	 * 
	 */
	public boolean setSelectedStyle(String themeID, String pageID, boolean applyToPage) {
		IWContext iwc = CoreUtil.getIWContext();
		if (iwc == null) {
			return false;
		}
		
		if (themeID == null) {
			return false;
		}
		Theme theme = helper.getTheme(themeID);
		if (theme == null) {
			return false;
		}
		boolean result = true;
		
		helper.setLastUsedTheme(theme.getIBPageID());
		
		if (applyToPage) {
			//	Apply style to selected page
			result = setPageStyle(pageID, theme.getIBPageID(), iwc, null);
		}
		else {
			//	Apply style to all pages
			result = setSiteStyle(theme.getIBPageID(), iwc);
		}
		helper.getThemesService().getBuilderService().clearAllCachedPages();
		return result;
	}
	
	private boolean setPageStyle(String pageID, int templateID, IWContext iwc, ICDomain cachedDomain) {
		boolean result = setStyle(pageID, templateID, false);
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
		if (startPageID == Integer.valueOf(pageID).intValue()) {
			//	Setting the same style as front page has
			String articleViewerID = iwc.getApplicationSettings().getProperty(ARTICLE_VIEWER_TEMPLATE_KEY);
			if (articleViewerID != null) {
				result = setStyle(articleViewerID, templateID, true);
			}
		}
		return result;
	}
	
	private boolean setStyle(String pageID, int templateID, boolean ignoreTemplate) {
		ICPage page = null;
		if (templateID <= 0) {
			return false;
		}
		page = helper.getThemesService().getICPage(pageID);
		if (page == null) {
			return false;
		}
		if (page.isPage() || ignoreTemplate) {
			helper.getThemesService().getBuilderService().setTemplateId(pageID, String.valueOf(templateID));
			page.setTemplateId(templateID);
			page.store();
		}
		return true;
	}
	
	@SuppressWarnings("unchecked")
	private boolean setSiteStyle(int templateID, IWContext iwc) {
		Map tree = helper.getThemesService().getBuilderService().getTree(iwc);
		if (tree == null) {
			return false;
		}
		ICDomain cachedDomain = iwc.getApplicationContext().getDomain();
		Object o = null;
		boolean result = true;
		for (Iterator it = tree.values().iterator(); it.hasNext();) {
			o = it.next();
			if (o instanceof ICTreeNode) {
				result = setPageStyle(((ICTreeNode) o).getId(), templateID, iwc, cachedDomain);
			}
		}
		return result;
	}
	
	private boolean setPageTitle(String pageID, String title) {
		if (pageID == null || title == null) {
			return false;
		}
		if (ThemesConstants.MINUS_ONE.equals(pageID)) {
			return false;
		}
		
		IWContext iwc = CoreUtil.getIWContext();
		if (iwc == null) {
			return false;
		}
		
		IWMainApplication appl = iwc.getIWMainApplication();
		if (appl == null) {
			return false;
		}
		
		String method = ":method:1:implied:void:setTitle:java.lang.String:";
		return helper.getThemesService().getBuilderService().setProperty(pageID, ThemesConstants.MINUS_ONE, method, new String[]{title}, appl);
	}
	
	public String changePageUri(String pageKey, String pageUri, boolean needSetPageTitle) {
		if (pageKey == null || pageUri == null) {
			return null;
		}
		if (ThemesConstants.MINUS_ONE.equals(pageKey)) {
			return null;
		}
		
		ICPage page = helper.getThemesService().getICPage(pageKey);
		if (page == null) {
			return null;
		}
		if (pageUri.equals(page.getDefaultPageURI())) {
			return null;
		}
		
		if (needSetPageTitle) {
			setPageTitle(pageKey, pageUri);
		}
		
		ICDomain domain = null;
		IWContext iwc = CoreUtil.getIWContext();
		if (iwc == null) {
			return null;
		}
		else {
			domain = iwc.getDomain();
		}
		if (domain == null) {
			return null;
		}
		
		if (domain.getStartPageID() == Integer.valueOf(page.getId())) { // Is page a root page?
			return page.getDefaultPageURI();
		}
		
		ICTreeNode parentNode = page.getParentNode();
		String parentId = null;
		if (parentNode != null) {
			parentId = parentNode.getId();
		}
		
		if (helper.getThemesService().getBuilderService().changePageUriByTitle(parentId, page, pageUri, domain.getID())) {
			setNewLinkInArticleFile(page.getId(), CoreConstants.getArticleItemViewerClass().getName(), page.getDefaultPageURI());
			return page.getDefaultPageURI();
		}
		return null;
	}
	
	private String setPageUri(String pageKey, String uri) {
		if (pageKey == null || uri == null) {
			return null;
		}
		if (ThemesConstants.MINUS_ONE.equals(pageKey)) {
			return null;
		}
		
		ICPage page = helper.getThemesService().getICPage(pageKey);
		if (page == null) {
			return null;
		}
		if (uri.equals(page.getDefaultPageURI())) {
			return null;
		}
		
		ICDomain domain = null;
		IWContext iwc = CoreUtil.getIWContext();
		if (iwc == null) {
			return null;
		}
		else {
			domain = iwc.getDomain();
		}
		if (domain == null) {
			return null;
		}
		
		if (domain.getStartPageID() == Integer.valueOf(page.getId())) { // Is page a root page?
			return page.getDefaultPageURI();
		}
		
		if (helper.getThemesService().getBuilderService().setPageUri(page, uri, domain.getID())) {
			setNewLinkInArticleFile(page.getId(), CoreConstants.getArticleItemViewerClass().getName(), page.getDefaultPageURI());
			return page.getDefaultPageURI();
		}
		return null;
	}
	
	public boolean setNewLinkInArticleFile(String pageKey, String moduleClass, String pageUri) {
		if (pageKey == null || moduleClass == null || pageUri == null) {
			return false;
		}
		
		BuilderService builder = helper.getThemesService().getBuilderService();
		if (builder == null) {
			return false;
		}
		IWContext iwc = CoreUtil.getIWContext();
		if (iwc == null) {
			return false;
		}
		List<String> moduleIds = builder.getModuleId(pageKey, moduleClass);
		if (moduleIds == null) {
			return false;
		}
		String propertyName = "resourcePath";
		String[] linkToArticle = null;
		for (int i = 0; i < moduleIds.size(); i++) {
			linkToArticle = builder.getPropertyValues(iwc.getIWMainApplication(), pageKey, moduleIds.get(i), propertyName, null, false);
		}
		if (linkToArticle == null) {
			return false;
		}
		if (linkToArticle.length == 0) {
			return false;
		}
		
		List<Locale> locales = ICLocaleBusiness.getListOfLocalesJAVA();
		if (locales == null) {
			return false;
		}
		StringBuffer link = new StringBuffer(helper.getFullWebRoot()).append(linkToArticle[0]);
		if (!link.toString().endsWith(ContentConstants.SLASH)) {
			link.append(ContentConstants.SLASH);
		}
		Locale l = null;
		boolean result = true;
		for (int i = 0; i < locales.size(); i++) {
			l = locales.get(i);
			if (l.getLanguage() != null) {
				result = helper.setNewLinkInArticleFile(iwc, link.toString(), l.getLanguage(), linkToArticle[0], pageUri);
			}
		}
		
		return result;
	}
	
	/**
	 * 
	 */
	public String savePageInfo(String pageID, String[] keywords, String[] values) {
		if (pageID == null || keywords == null || values == null) {
			return null;
		}
		if (keywords.length != values.length) {
			return null;
		}
		IWContext iwc = CoreUtil.getIWContext();
		if (iwc == null) {
			return null;
		}
		IWMainApplication appl = iwc.getIWMainApplication();
		if (appl == null) {
			return null;
		}
		String changedPageUri = null;
		Setting s = null;
		Map <String, Setting> map = helper.getPageSettings();
		String[] currentValues = null;
		String[] newValues = null;
		boolean changedPageTitle = false;
		boolean needSetValue = true;
		for (int i = 0; i < keywords.length; i++) {
			needSetValue = true;
			s = map.get(keywords[i]);
			if (s != null) {
				currentValues = helper.getThemesService().getBuilderService().getPropertyValues(appl, pageID, ThemesConstants.MINUS_ONE, s.getMethod(), null, true);
				if (ThemesConstants.EMPTY.equals(values[i]) || values[i] == null) {
					if (currentValues != null) {
						helper.getThemesService().getBuilderService().removeProperty(appl, pageID, ThemesConstants.MINUS_ONE, s.getMethod(), currentValues);
					}
				}
				else {
					if (s.getCode().equals(PAGE_URI)) {
						if (!changedPageTitle) {
							changedPageUri = setPageUri(pageID, values[i]);
						}
					}
					else {
						newValues = helper.getPageValues(s, values[i]);
						if (newValues == null) {
							needSetValue = false;
						}
						if (Arrays.deepEquals(newValues, currentValues)) {
							needSetValue = false;
						}
						if (needSetValue) {
							helper.getThemesService().getBuilderService().setProperty(pageID, ThemesConstants.MINUS_ONE, s.getMethod(), newValues, appl);
							if (s.getCode().equals(PAGE_TITLE)) {
								changedPageUri = setPageUri(pageID, values[i]);
								helper.getThemesService().getBuilderService().changePageName(Integer.valueOf(pageID).intValue(), values[i]);
								changedPageTitle = true;
							}
						}
					}
				}
			}
		}
		return changedPageUri;
	}
	
	public String[] getPageInfoValues(String pageID, String[] keywords) {
		if (pageID == null || keywords == null) {
			return null;
		}
		if (ThemesConstants.MINUS_ONE.equals(pageID)) {
			return null;
		}
		IWContext iwc = CoreUtil.getIWContext();
		if (iwc == null) {
			return null;
		}
		IWMainApplication appl = iwc.getIWMainApplication();
		if (appl == null) {
			return null;
		}
		Setting s = null;
		Map <String, Setting> map = helper.getPageSettings();
		String[] values = new String[keywords.length];
		String[] propValues = null;
		StringBuffer value = null;
		for (int i = 0; i < keywords.length; i++) {
			s = map.get(keywords[i]);
			value = new StringBuffer();
			if (s != null) {
				propValues = helper.getThemesService().getBuilderService().getPropertyValues(appl, pageID, ThemesConstants.MINUS_ONE, s.getMethod(), null, true);
				if (propValues != null) {
					for (int j = 0; j < propValues.length; j++) {
						if (!s.getDefaultValue().equals(propValues[j])) {
							value.append(propValues[j]);
							if (j + 1 < propValues.length) {
								value.append(ThemesConstants.COMMA);
							}
						}
					}
				}
				else {
					if (s.getCode().equals(PAGE_URI)) {
						ICPage page = helper.getThemesService().getICPage(pageID);
						if (page != null) {
							value.append(page.getDefaultPageURI());
						}
					}
				}
			}
			values[i] = value.toString();
		}
		return values;
	}
	
	/**
	 * 
	 */
	public String[] getPageInfoElements() {
		Collection <Setting> c = helper.getPageSettings().values();
		if (c == null) {
			return null;
		}
		return getElements(c);
	}
	
	/**
	 * 
	 */
	public boolean restoreTheme(String themeID) {
		return helper.getThemeChanger().restoreTheme(themeID);
	}
	
	public String[] getSiteInfoElements() {
		Collection <Setting> c = helper.getThemeSettings().values();
		if (c == null) {
			return null;
		}
		return getElements(c);
	}
	
	public String[] getSiteInfoValues(String[] keywords, String language) {
		if (keywords == null || language == null) {
			return null;
		}
		Collection <Setting> c = helper.getThemeSettings().values();
		if (c == null) {
			return null;
		}
		String[] values = new String[keywords.length];
		IWMainApplication application = ContentUtil.getBundle().getApplication();
		IWMainApplicationSettings settings  = application.getSettings();
		ICDomain domain = application.getIWApplicationContext().getDomain(); // Cached domain
		for (int i = 0; i < keywords.length; i++) {
			values[i] = getSiteInfoValue(keywords[i], language, settings, domain);
		}
		return values;
	}
	
	public String getSiteInfoValue(String keyword, String language, IWMainApplicationSettings settings, ICDomain domain) {
		if (keyword == null || language == null || settings == null) {
			return ThemesConstants.EMPTY;
		}
		keyword = ThemesConstants.THEMES_PROPERTY_START + keyword + ThemesConstants.DOT + language;
		if (keyword.indexOf(ThemesConstants.SYSTEM_SETTINGS) == -1) {
			return settings.getProperty(keyword);
		}
		else {
			//	System Settings
			if (domain == null) {
				IWContext iwc = CoreUtil.getIWContext();
				if (iwc != null) {
					domain = iwc.getDomain();
				}
			}
			if (domain == null) {
				return ThemesConstants.EMPTY;
			}
			else {
				if (keyword.indexOf(ThemesConstants.DOMAIN_NAME) != -1) {
					return domain.getDomainName();
				}
				if (keyword.indexOf(ThemesConstants.DOMAIN_SERVER_NAME) != -1) {
					return domain.getServerName();
				}
			}
		}
		return ThemesConstants.EMPTY;
	}
	
	private String[] getElements(Collection <Setting> c) {
		if (c == null) {
			return null;
		}
		try {
			String[] elements = null;
			List <Setting> settings = new ArrayList<Setting>(c);
			elements = new String[settings.size()];
			for (int i = 0; i < settings.size(); i++) {
				elements[i] = settings.get(i).getCode();
			}
			return elements;
		} catch (Exception e) {
			e.printStackTrace();
		}
		return null;
	}
	
	private boolean saveSiteInfoValue(String language, String keyword, String value, IWMainApplicationSettings settings,
			ICDomain domain, ICDomain cachedDomain) {
		if (language == null || keyword == null) {
			return false;
		}

		if (keyword.indexOf(ThemesConstants.SYSTEM_SETTINGS) == -1) {
			if (settings == null) {
				return false;
			}
			String key = new StringBuffer(ThemesConstants.THEMES_PROPERTY_START).append(keyword).append(language).toString();
			if (value == null) {
				settings.removeProperty(key);
			}
			else {
				if (value.equals("               ") || value.equals(ContentConstants.SPACE)) {
					value = ContentConstants.EMPTY;
				}
				settings.setProperty(ThemesConstants.THEMES_PROPERTY_START + keyword + language, value);
			}
		}
		else {
			//	Saving System Settings
			if (cachedDomain == null) {
				return false;
			}
			if (domain == null) {
				domain = helper.getThemesService().getDomain();
			}
			if (value != null && !ThemesConstants.EMPTY.equals(value)) {
				if (domain != null) {
					if (keyword.indexOf(ThemesConstants.DOMAIN_NAME) != -1) {
						domain.setDomainName(value);
						cachedDomain.setDomainName(value);
					}
					if (keyword.indexOf(ThemesConstants.DOMAIN_SERVER_NAME) != -1) {
						domain.setServerName(value);
						cachedDomain.setServerName(value);
					}
					domain.store();
				}
			}
		}
		
		return true;
	}
	
	public boolean saveSiteInfoValue(String keyword, String value) {
		if (keyword == null || value == null) {
			return false;
		}
		
		IWContext iwc = CoreUtil.getIWContext();
		if (iwc == null) {
			return false;
		}
		String language = helper.getCurrentLanguage(iwc);
		if (language == null) {
			return false;
		}
		language = ThemesConstants.DOT + language;
		keyword = helper.extractValueFromString(keyword, 0, keyword.lastIndexOf(ThemesConstants.UNDER));
		
		IWMainApplication appl = ContentUtil.getBundle().getApplication();
		ICDomain cachedDomain = null;
		if (appl != null) {
			cachedDomain = appl.getIWApplicationContext().getDomain();
		}
		
		return saveSiteInfoValue(language, keyword, value, appl.getSettings(), null, cachedDomain);
	}
	
	public boolean saveSiteInfo(String language, String[] keywords, String[] values) {
		if (language == null || keywords == null || values == null) {
			return false;
		}
		if (keywords.length != values.length) {
			return false;
		}
		IWMainApplication application = ContentUtil.getBundle().getApplication();
		IWMainApplicationSettings settings  = application.getSettings();
		language = ThemesConstants.DOT + language;
		ICDomain domain = null;
		ICDomain cachedDomain = application.getIWApplicationContext().getDomain();
		for (int i = 0; i < keywords.length; i++) {
			saveSiteInfoValue(language, keywords[i], values[i], settings, domain, cachedDomain);
		}
		return true;
	}
	
	public List <String> createPage(List <TreeNodeStructure> struct, Boolean isTopLevelPage, String numberInLevel, List<String> followingNodes){

		List <String> newIds = new ArrayList<String>();
		
		struct.get(0).setTreeOrder(numberInLevel);
		struct = getOrderInLevel(struct);
		
		BuilderService builder = helper.getThemesService().getBuilderService();
		ICDomain domain = helper.getThemesService().getDomain();
		
		int pageID = -1;
		int domainID = -1;
		
		boolean isRootPage = false;
		
		if (domain != null) {
			domainID = domain.getID();
		}
		
		String uri = null;
		String pageType = builder.getPageKey();
		String format = builder.getIBXMLFormat();
		String pageKey = null;
		
		List<String> createdPages = new ArrayList<String>();
		
		TreeNodeStructure node = null;
		for (int i = 0; i < struct.size(); i++) {
			node = struct.get(i);
			if (domain != null && node.getParentId() == null) {
				if (domain.getStartPage() == null) {
					uri = ContentConstants.SLASH;
					isRootPage = true;
				}
			}
			
			pageID = createPage(node.getParentId(), node.getNodeName(), pageType, null, uri, node.getPageType(), domainID, format, null, node.getTreeOrder());
			if (pageID < 0) {
				//	Error
				break;
			}
			pageKey = String.valueOf(pageID);
			createdPages.add(pageKey);
			
			if (domain != null) {
				if ((domain.getStartPage() == null) && (isTopLevelPage)) {
					//	Marking page as top level page
					domain.setIBPage(helper.getThemesService().getICPage(pageID));
					domain.store();
				}					
			}
			
			if (isRootPage) {
				//	Creating root page and root template
				createRootPage(pageID, domain, builder, domainID, format);
				createRootTemplate(domain, builder, domainID, format);
				createArticlePreviewTemplate(domainID, builder, format);
				initializeCachedDomain(ThemesConstants.DEFAULT_DOMAIN_NAME, domain);
				IWWelcomeFilter.unload();
			}

			uri = null;
			isRootPage = false;

			preparePage(node.getTemplateFile(), node.getPageType(), pageID, pageKey, false);

			for (int j = i; j < struct.size(); j++) {
				if (struct.get(j).getParentId() != null) {
					if ((struct.get(j).getParentId()).equals(node.getNodeId())) {						
						struct.get(j).setParentId(pageKey);		
					}
				}
			}

			newIds.add(pageKey);
		}
		
		//	Clearing cache
		builder.clearAllCachedPages();
		
		//	Setting template id for new page(s)
		String lastUsedTemplate = helper.getLastUsedTheme();
		if (lastUsedTemplate != null && !CoreConstants.EMPTY.equals(lastUsedTemplate)) {
			for (int i = 0; i < createdPages.size(); i++) {
				setLastUsedTemplate(createdPages.get(i), lastUsedTemplate);
			}
		}

		//	Creating new tree order
		increaseNodesNumbersInLevel(followingNodes, -1, null);
		
		return newIds;
	}
	
	private int createPage(String parentId, String name, String type, String templateId, String pageUri, String subType, int domainId, String format, String sourceMarkup) {
		return createPage(parentId, name, type, templateId, pageUri, subType, domainId, format, sourceMarkup, null);
	}
	
	private boolean preparePage(String templateFile, String pageType, int pageID, String realID, boolean clearCache) {
		if (templateFile == null || pageType == null || pageID < 0 || realID == null) {
			return false;
		}
		if (ThemesConstants.EMPTY.equals(templateFile)) {
			return false;
		}
		List<String> articlesPaths = helper.createArticle(templateFile, pageID);
		String uriToPage = helper.loadPageToSlide(pageType, templateFile, articlesPaths, pageID);
		if (uriToPage != null) {
			helper.getThemesService().updatePageWebDav(pageID, uriToPage, clearCache);
		}
		return true;
	}
	
	private void setLastUsedTemplate(String pageKey) {
		setLastUsedTemplate(pageKey, helper.getLastUsedTheme());
	}
	
	private void setLastUsedTemplate(String pageKey, String templateKey) {
		if (pageKey == null || templateKey == null) {
			return;
		}
		helper.getThemesService().getBuilderService().setTemplateId(pageKey, templateKey);
	}
	
	private int createPage(String parentId, String name, String type, String templateId, String pageUri, String subType, int domainId, String format, String sourceMarkup, String treeOrder) {
		int id = -1;
		if (pageUri != null) {
			if (pageUri.equals(ThemesConstants.EMPTY)) {
				pageUri = null;
			}
		}
		try {
			id = helper.getThemesService().createIBPage(parentId, name, type, templateId, pageUri, subType, domainId, format, sourceMarkup, treeOrder);
		} catch (RemoteException e) {
			log.error(e);
			return -1;
		}
		return id;
	}
	
	public boolean deletePage(String pageId, boolean deleteChildren) {
		return deletePageAndDecrease(pageId, deleteChildren, null);
	}
	
	public boolean deletePageAndDecrease(String pageId, boolean deleteChildren, ArrayList<String> followingNodes) {
		if (pageId == null) {
			return false;
		}
		try {
			decreaseNodesNumbersInLevel(followingNodes, -1, null);			
			helper.getThemesService().deleteIBPage(pageId, deleteChildren);
		} catch (RemoteException e) {
			log.error(e);
			return false;
		}
		return true;
	}
	
	private boolean isPageDeleted(String pageID) {
		if (pageID == null) {
			return true;
		}
		ICPage page = helper.getThemesService().getICPage(pageID);
		if (page == null) {
			return true;
		}
		return page.getDeleted();
	}
	
	public String getPageId() {
		String id = helper.getLastVisitedPage();
		if (id != null) {
			if (!ThemesConstants.MINUS_ONE.equals(id)) {
				if (isPageDeleted(id)) {
					return String.valueOf(getRootPageId());
				}
				return id;
			}
		}
		
		id = String.valueOf(getRootPageId());
		if (ThemesConstants.MINUS_ONE.equals(id)) {
			return ThemesConstants.MINUS_ONE;
		}
		if (isPageDeleted(id)) {
			return ThemesConstants.MINUS_ONE;
		}
		helper.setLastVisitedPage(id);
		return id;
	}
	
	public boolean setPageId(String id) {
		if (id == null) {
			return false;
		}
		helper.setLastVisitedPage(id);
		return true;
	}
	
	public boolean movePage(int newParentId, int nodeId, int numberInLevel, ArrayList<String> nodesToIncrease, ArrayList<String> nodesToDecrease) {
		IWContext iwc = CoreUtil.getIWContext();
		boolean result = false;
		if (iwc == null) {
			return false;
		}
		
		BuilderService service = helper.getThemesService().getBuilderService(); 
		
		ICPage page = helper.getThemesService().getICPage(nodeId);
		page.setTreeOrder(numberInLevel);
		page.store();
		service.setTreeOrder(nodeId, numberInLevel);

		if (nodesToIncrease != null) {
			increaseNodesNumbersInLevel(nodesToIncrease, numberInLevel, service);
		}
		
		if (nodesToDecrease != null) {
			decreaseNodesNumbersInLevel(nodesToDecrease, numberInLevel, service);
		}
		
		if (newParentId < 0) {
			result = service.movePageToTopLevel(nodeId, iwc);
			return result;
		}
		
		result = service.movePage(newParentId, nodeId, iwc.getDomain());
		return result;
	}
	
	public String getPathToImageFolder(){
		return PATH_TO_IMAGE_FOLDER;
	}
	
	public boolean isStartPage(String pageID) {
		if (pageID == null) {
			pageID = helper.getLastVisitedPage();
		}
		int id = -1;
		try {
			id = Integer.valueOf(pageID).intValue();
		} catch (NumberFormatException e) {
			log.error(e);
			return true; // Returning true to disable a button
		}
		if (id <= 0) {
			return true; // Returning true to disable a button
		}
		if (id == getRootPageId()) {
			return true;
		}
		return false;
	}
	
	public String setAsStartPage(String pageID) {
		if (pageID == null) {
			return null;
		}
		int newRoot = -1;
		try {
			newRoot = Integer.valueOf(pageID).intValue();
		} catch (NumberFormatException e) {
			log.error(e);
			return null;
		}
		if (newRoot <= 0) {
			return null;
		}
		
		int currentRoot = getRootPageId();
		if (currentRoot == newRoot) {
			return null;
		}
		
		BuilderService builder = helper.getThemesService().getBuilderService();
		
		IWContext iwc = CoreUtil.getIWContext();
		if (iwc == null) {
			return null;
		}
		
		ICPage newRootPage = helper.getThemesService().getICPage(newRoot);
		if (newRootPage == null) {
			return null;
		}
		
		//	Setting new tree order
		manageNewSiteTreeOrder(iwc, builder, newRootPage, newRoot);

		//	Setting new page root for ICDomain
		ICDomain domain = iwc.getDomain();
		if (domain != null) {
			//	Setting new start page in ICDomain
			domain.setIBPage(newRootPage);
			domain.store();
		}
		
		//	Changing uri to new start page
		newRootPage.setDefaultPageURI(ContentConstants.SLASH);
		//	New root page now is also top level page
		builder.createTopLevelPageFromExistingPage(newRoot, domain, iwc);

		//	Setting new tree order
		newRootPage.setTreeOrder(1);			
		newRootPage.store();
		
		//	Changing old root page's properties
		ICPage rootPage = helper.getThemesService().getICPage(currentRoot);
		if (rootPage == null) {
			return null;
		}
		//	Changing page uri from "/" to some other
		changePageUri(rootPage.getPageKey(), rootPage.getName().toLowerCase(), false);
		//	Old root page now is a simple top level page
		builder.createTopLevelPageFromExistingPage(currentRoot, domain, iwc);
		
		TreeableEntity parent = newRootPage.getParentEntity();
		if (parent instanceof ICPage) {
			ICPage parentPage = (ICPage) parent;
			try {
				parentPage.removeChild(newRootPage);	//	Removing new root as child from his old parent node
			} catch (SQLException e) {
				log.error(e);
			}
		}
		
		builder.clearAllCaches();
		
		return newRootPage.getDefaultPageURI();
	}
	
	private int getRootPageId() {
		int id = 1;
		try {
			id = helper.getThemesService().getBuilderService().getRootPageId();
		} catch (Exception e) {
			log.error(e);
			return -1;
		}
		return id;
	}
	
	private boolean createRootPage(int pageID, ICDomain domain, BuilderService builder, int domainID, String format) {	
		if (domain.getStartPage() != null) {
			return true;
		}
		
		if (domain.getDomainName() == null) {
			domain.setDomainName(ThemesConstants.DEFAULT_DOMAIN_NAME);
			domain.store();
		}
		
		builder.unlockRegion(String.valueOf(pageID), ThemesConstants.MINUS_ONE, null);

		domain.setIBPage(helper.getThemesService().getICPage(pageID));
		domain.store();
		return true;
	}
	
	public int createRootTemplate(ICDomain domain, BuilderService builder, int domainID, String format) {
		if (domain.getStartTemplate() != null) {
			return domain.getStartTemplateID();
		}
		
		int templateId = createPage(null, "Template", builder.getTemplateKey(), null, null, null, domainID, format, null);
		
		builder.unlockRegion(String.valueOf(templateId), ThemesConstants.MINUS_ONE, null);
		
		domain.setStartTemplate(helper.getThemesService().getICPage(templateId));
		domain.store();
		return templateId;
	}
	
	private boolean createArticlePreviewTemplate(int domainID, BuilderService builder, String format) {
		if (builder == null) {
			return false;
		}
		IWContext iwc = CoreUtil.getIWContext();
		
		String articleTemplateFile = "/idegaweb/bundles/com.idega.block.article.bundle/resources/pages/article_viewer_template.xml";
		boolean existInSlide = false;
		int id = getArticleViewerTemplateId(builder, iwc);
		existInSlide = helper.existFileInSlide(ThemesConstants.PAGES_PATH_SLIDE + articleTemplateFile);
		if (existInSlide && id != -1) {
			return true;
		}
		
		if (id == -1) {
			id = createPage(null, ARTICLE_VIEWER_NAME, builder.getTemplateKey(), null, ContentConstants.ARTICLE_VIEWER_URI, ARTICLE_VIEWER_SUBTYPE, domainID, format, null);
		}
		else {
			if (existInSlide) {
				return true;
			}
		}
		
		String pageKey = String.valueOf(id);
		boolean result = preparePage(articleTemplateFile, ARTICLE_VIEWER_SUBTYPE, id, pageKey, true);
		setLastUsedTemplate(pageKey);
		if (iwc == null) {
			return result;
		}
		iwc.getApplicationSettings().setProperty(ARTICLE_VIEWER_TEMPLATE_KEY, pageKey);
		return result;
	}
	
	@SuppressWarnings("unchecked")
	private int getArticleViewerTemplateId(BuilderService builder, IWContext iwc) {
		if (builder == null || iwc == null) {
			return -1;
		}
		Collection templates = builder.getTopLevelTemplates(iwc);
		if (templates == null) {
			return -1;
		}
		if (templates.size() == 0) {
			return -1;
		}
		Object o = null;
		ICTreeNode treeNode = null;
		ICPage page = null;
		int id = -1;
		for (Iterator it = templates.iterator(); it.hasNext();) {
			o = it.next();
			if (o instanceof ICTreeNode) {
				treeNode = (ICTreeNode) o;
				if (ARTICLE_VIEWER_NAME.equals(treeNode.getNodeName())) {
					try {
						id = Integer.valueOf(treeNode.getId());
					} catch (NumberFormatException e) {
						log.error(e);
						return -1;
					}
					page = helper.getThemesService().getICPage(id);
					if (page != null) {
						if (ARTICLE_VIEWER_SUBTYPE.equals(page.getSubType())) {
							return id;
						}
					}
				}
			}
		}
		
		return -1;
	}
	
	public boolean initializeCachedDomain(String domainName, ICDomain domain) {
		ICDomain cachedDomain = IWMainApplication.getDefaultIWMainApplication().getIWApplicationContext().getDomain();
		if (cachedDomain.getDomainName() == null) {
			cachedDomain.setDomainName(domainName);
		}
		cachedDomain.setIBPage(domain.getStartPage());
		cachedDomain.setStartTemplate(domain.getStartTemplate());
		if(cachedDomain instanceof CachedDomain){
			CachedDomain ccachedDomain = (CachedDomain)cachedDomain;
			ccachedDomain.setStartTemplateID(domain.getStartTemplateID());
			ccachedDomain.setStartPage(domain.getStartPage());
			ccachedDomain.setStartPageID(domain.getStartPageID());
		}
		return true;
	}
	
	public String applyMultipleChangesToTheme(String themeID, List<ThemeChange> changes, String themeName) {
		return helper.getThemeChanger().applyMultipleChangesToTheme(themeID, changes, themeName);
	}

	private boolean decreaseNodesNumbersInLevel(List<String> nodes, int numberInLevel, BuilderService service) {
		if (nodes == null) {
			return false;
		}		
		if (service == null) {
			service = helper.getThemesService().getBuilderService();
		}
		
		int id = -1;
		ICPage page = null;
		for (int i = 0; i < nodes.size(); i++) {
			try {
				id = Integer.valueOf(nodes.get(i)).intValue();
			} catch (NumberFormatException e) {
				e.printStackTrace();
			} catch (NullPointerException e) {
				System.out.println("List element nr. " + i + " is null");
				e.printStackTrace();
			}
			if (id != -1)
				page = helper.getThemesService().getICPage(id);
			if (page != null) {
				page.setTreeOrder(page.getTreeOrder()-1);
				service.decreaseTreeOrder(id);
				page.store();
			}
		}
		return true;
	}
	
	private boolean increaseNodesNumbersInLevel(List<String> nodes, int numberInLevel, BuilderService service) {
		if (nodes == null) {
			return false;
		}
		if (service == null) {
			service = helper.getThemesService().getBuilderService();
		}
		int id = -1;
		ICPage page = null;
		for (int i = 0; i < nodes.size(); i++){
			try {
				id = Integer.valueOf(nodes.get(i)).intValue();
			} catch (NumberFormatException e) {
				System.out.println("Can't convert "+nodes.get(i)+"to integer");
				e.printStackTrace();
			} catch (NullPointerException e) {
				System.out.println("List element nr. " + i + " is null");
				e.printStackTrace();
			}
			page = null;	// Reseting
			if (id != -1) {
				page = helper.getThemesService().getICPage(id);
			}
			if (page != null) {
				page.setTreeOrder(page.getTreeOrder()+1);
				service.increaseTreeOrder(id);
				page.store();
			}
		}
		
		return true;
	}
	
	private List <TreeNodeStructure> getOrderInLevel(List <TreeNodeStructure> struct){
		Map<String, Integer> children = new HashMap<String, Integer>();
		String ONE = "1";
		String parentId = null;
		Integer number = null;
		for (int i = 0; i < struct.size(); i++) {
			if (struct.get(i).getTreeOrder() == null) {
				parentId = struct.get(i).getParentId();
				if (children.containsKey(parentId)) {
					number = (Integer)(children.get(parentId)) + 1;
					children.put(parentId, number);
					struct.get(i).setTreeOrder(String.valueOf(number));
				}
				else {
					children.put(parentId, 1);
					struct.get(i).setTreeOrder(ONE);
				}
			}
		}
		
		return struct; 
	}
	
	@SuppressWarnings("unchecked")
	private boolean manageNewSiteTreeOrder(IWContext iwc, BuilderService builder, ICPage newRootPage, int newRoot) {
		if (iwc == null || builder == null || newRootPage == null || newRoot == -1) {
			return false;
		}
		Collection topLevelPages = builder.getTopLevelPages(iwc);
		if (topLevelPages == null) {
			return false;
		}
		int nodeOrder = 0;
		int newRootOrder = 0;
		ICTreeNode element = null;
		ICPage page = null;
		ICPage newPage = null;
		if (newRootPage.getParentNode() == null) {
			//	Top level page
			List<String> increaseLevelOnTop = new ArrayList<String>();
			for (Iterator iter = topLevelPages.iterator(); iter.hasNext();) {
				element = (ICTreeNode) iter.next();
				page = helper.getThemesService().getICPage(element.getId());
				newPage = helper.getThemesService().getICPage(newRoot);
				if (page != null && newPage != null) {
					nodeOrder = page.getTreeOrder();
					newRootOrder = newPage.getTreeOrder();
					if (nodeOrder < newRootOrder) {
						increaseLevelOnTop.add(element.getId());
					}
				}
			}
			increaseNodesNumbersInLevel(increaseLevelOnTop, -1, builder);
		}
		else {
			//	Not top level page
			for (Iterator iter = topLevelPages.iterator(); iter.hasNext();) {
				element = (ICTreeNode) iter.next();
				page = helper.getThemesService().getICPage(element.getId());
				if (page != null) {
					page.setTreeOrder(page.getTreeOrder()+1);
					page.store();
				}
			}
			
			List<String> decreaseLevelOnTop = new ArrayList<String>();
			Collection siblings = newRootPage.getParentNode().getChildren();
			if (siblings == null) {
				return false;
			}
			for (Iterator iter = siblings.iterator(); iter.hasNext();) {
				element = (ICTreeNode) iter.next();
				page = helper.getThemesService().getICPage(element.getId());
				newPage = helper.getThemesService().getICPage(newRoot);
				if (page != null && newPage != null) {
					nodeOrder = page.getTreeOrder();
					newRootOrder = newPage.getTreeOrder();
					if (nodeOrder > newRootOrder) {
						decreaseLevelOnTop.add(element.getId());
					}
				}
			}
			decreaseNodesNumbersInLevel(decreaseLevelOnTop, -1, builder);
		}
		return true;
	}
	
	public List<String> getLocalizedText() {
		List <String>localizedText = new ArrayList<String>();
		IWResourceBundle resourceBundle = null;
		try {
			resourceBundle = ContentUtil.getBundle().getResourceBundle(CoreUtil.getIWContext());
		} catch (Exception e) {
			log.error(e);
		}
		if (resourceBundle == null) {
			return localizedText;
		}
		
		try {
			localizedText.add(resourceBundle.getLocalizedString("uploading_theme", "Uploading..."));							// 0
			localizedText.add(resourceBundle.getLocalizedString("changing_theme", "Changing..."));								// 1
			localizedText.add(resourceBundle.getLocalizedString("saving", "Saving..."));										// 2
			localizedText.add(resourceBundle.getLocalizedString("generating_preview", "Generating preview..."));				// 3
			localizedText.add(resourceBundle.getLocalizedString("restoring_theme", "Restoring..."));							// 4
			localizedText.add(resourceBundle.getLocalizedString("hide_themes", "Hide Themes"));									// 5
			localizedText.add(resourceBundle.getLocalizedString("show_themes", "Show Themes"));									// 6
			localizedText.add(resourceBundle.getLocalizedString("style_for_current_page", "Select style for current page"));	// 7
			localizedText.add(resourceBundle.getLocalizedString("style_for_site", "Select style for all pages"));				// 8
			localizedText.add(resourceBundle.getLocalizedString("applying_style", "Applying style..."));						// 9
			localizedText.add(resourceBundle.getLocalizedString("close", "Close"));												// 10
			localizedText.add(resourceBundle.getLocalizedString("start_page_text", "Start Page"));								// 11
			localizedText.add(resourceBundle.getLocalizedString("make_start_page", "Start Page"));								// 12
			localizedText.add(resourceBundle.getLocalizedString("changing_structure", "Changing structure..."));				// 13
			localizedText.add(resourceBundle.getLocalizedString("new_page", "New Page"));										// 14
			localizedText.add(resourceBundle.getLocalizedString("moving", "Moving..."));										// 15
			localizedText.add(resourceBundle.getLocalizedString("are_you_sure", "Are you sure?"));								// 16
			localizedText.add(resourceBundle.getLocalizedString("deleting", "Deleting..."));									// 17
			localizedText.add(resourceBundle.getLocalizedString("page", "Page"));												// 18
			localizedText.add(resourceBundle.getLocalizedString("site", "Site"));												// 19
			localizedText.add(resourceBundle.getLocalizedString("drop_templates_here", "Drop templates here"));					// 20
			localizedText.add(resourceBundle.getLocalizedString("no_page_exist", "No page exist"));								// 21
			localizedText.add(resourceBundle.getLocalizedString("loading", "Loading..."));										// 22
			localizedText.add(resourceBundle.getLocalizedString("make_this_page_start_page", "Make This Page As Start Page"));	// 23
			localizedText.add(resourceBundle.getLocalizedString("reloading", "Reloading..."));									//	24
			localizedText.add(resourceBundle.getLocalizedString("show_modules", "Show Modules"));								//	25
			localizedText.add(resourceBundle.getLocalizedString("hide_modules", "Hide Modules"));								//	26
			localizedText.add(resourceBundle.getLocalizedString("redirecting", "Redirecting..."));								//	27
			localizedText.add(resourceBundle.getLocalizedString("creating", "Creating..."));									//	28
			
		} catch (Exception e) {
			log.error(e);
		}
		return localizedText;
	}
	
	public boolean startBuilderApplication() {
		IWContext iwc = CoreUtil.getIWContext();
		if (iwc == null) {
			return false;
		}
		BuilderService builder = helper.getThemesService().getBuilderService();
		if (builder == null) {
			return false;
		}
		builder.startBuilderSession(iwc);
		return true;
	}
	
	public String reloadThemeProperties(String themeId) {
		if (themeId == null) {
			return null;
		}
		Theme theme = helper.getTheme(themeId);
		if (theme == null) {
			return null;
		}
		
		List<ThemeChange> enabledStyles = getEnabledStyles(theme);				//	Getting current state
		helper.clearVariationFromCache(themeId);								//	Clearing cache
		theme.reloadProperties();												//	Clearing properties
		helper.getThemesPropertiesExtractor().prepareTheme(theme, null, null);	//	Extracting new properties (also setting default state)
		helper.getThemeChanger().setSelectedStyles(theme, enabledStyles);		//	Restoring current state
		
		return getThemeStyleVariations(themeId);
	}
	
	private List<ThemeChange> getEnabledStyles(Theme theme) {
		List<ThemeChange> enabled = new ArrayList<ThemeChange>();
		
		if (theme == null) {
			return enabled;
		}
		
		ThemeChange change = null;
		ThemeStyleGroupMember member = null;
		for (Iterator<ThemeStyleGroupMember> it = theme.getStyleGroupsMembers().values().iterator(); it.hasNext(); ) {
			member = it.next();
			change = new ThemeChange();
			change.setStyleGroupName(member.getGroupName());
			change.setVariation(member.getName());
			change.setEnabled(member.isEnabled());
			change.setLimitedSelection(member.isLimitedSelection());
			enabled.add(change);
		}
		
		return enabled;
	}
	
	public boolean isUserAdmin() {
		IWContext iwc = CoreUtil.getIWContext();
		if (iwc == null) {
			return false;
		}
		
		return iwc.isSuperAdmin();
	}

	public String getPageUri(String pageKey) {
		if (pageKey == null) {
			return null;
		}
		
		IWContext iwc = CoreUtil.getIWContext();
		if (iwc == null) {
			return null;
		}
		
		String uri = null;
		BuilderService builderService = null;
		try {
			builderService = BuilderServiceFactory.getBuilderService(iwc);
		} catch (RemoteException e) {
			e.printStackTrace();
			return null;
		}
		if (builderService == null) {
			return null;
		}
		
		try {
			uri = builderService.getPageURI(pageKey);
			builderService.setCurrentPageId(iwc, pageKey);
		} catch (Exception e) {
			e.printStackTrace();
			return null;
		}

		return uri;
	}

}