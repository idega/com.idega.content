package com.idega.content.themes.business;

import java.rmi.RemoteException;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.idega.business.IBOServiceBean;
import com.idega.content.business.ContentUtil;
import com.idega.content.themes.helpers.Setting;
import com.idega.content.themes.helpers.Theme;
import com.idega.content.themes.helpers.ThemeChange;
import com.idega.content.themes.helpers.ThemesConstants;
import com.idega.content.themes.helpers.ThemesHelper;
import com.idega.content.themes.helpers.TreeNodeStructure;
import com.idega.core.builder.business.BuilderService;
import com.idega.core.builder.data.CachedDomain;
import com.idega.core.builder.data.ICDomain;
import com.idega.core.builder.data.ICPage;
import com.idega.core.data.ICTreeNode;
import com.idega.data.TreeableEntity;
import com.idega.idegaweb.IWMainApplication;
import com.idega.idegaweb.IWMainApplicationSettings;
import com.idega.presentation.IWContext;
import com.idega.servlet.filter.IWWelcomeFilter;

public class ThemesEngineBean extends IBOServiceBean implements ThemesEngine {

	private static final long serialVersionUID = 5875353284352953688L;
	private static final Log log = LogFactory.getLog(ThemesEngineBean.class);
	
	private static final String PAGE_URI = "pageUri";
	private static final String PAGE_TITLE = "pageTitle";
	private static final String PATH_TO_IMAGE_FOLDER = ContentUtil.getBundle().getResourcesPath() + "/images/pageIcons/";
	
	private ThemesHelper helper = ThemesHelper.getInstance();

	/**
	 * Returns info about themes in slide
	 */
	public String getThemesPreviewsInfo() {
		helper.searchForThemes(); // It is done in ThemesHelper's constructor, but it's possible to pass a paremeter not to search

		if (!helper.getThemesPropertiesExtractor().prepareThemes()) {
			log.info("Error extracting theme's properties");
		}
		
		List <Theme> themes = new ArrayList<Theme>(helper.getThemesCollection());
		StringBuffer info = new StringBuffer();
		
		Theme theme = null;
		String webRoot = helper.getFullWebRoot();
		for (int i = 0; i < themes.size(); i++) {
			theme = themes.get(i);			
			if (theme.isPropertiesExtracted()) {
				if (theme.getChangedName() != null) {
					info.append(theme.getChangedName());
				}
				else {
					info.append(theme.getName());
				}
				info.append(ThemesConstants.AT);
				info.append(webRoot);
				info.append(theme.getLinkToBase());
				info.append(helper.encode(theme.getLinkToSmallPreview(), true));
				info.append(ThemesConstants.AT);
				info.append(webRoot);
				info.append(theme.getLinkToBase());
				if (theme.getLinkToDraftPreview() != null) {
					info.append(helper.encode(theme.getLinkToDraftPreview(), true));
				}
				else {
					info.append(helper.encode(theme.getLinkToThemePreview(), true));
				}
				info.append(ThemesConstants.AT);
				info.append(theme.getId());
				if (i + 1 < themes.size()) {
					info.append(ThemesConstants.SEMICOLON);
				}
			}
		}
		return info.toString();
	}
	
	/**
	 * 
	 */
	public String getThemeStyleVariations(String themeID) {
		return helper.getThemeStyleVariations().getThemeStyleVariations(themeID);
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
		IWContext iwc = IWContext.getInstance();
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
		
		if (applyToPage) { // Apply style to selected page
			result = setPageStyle(pageID, theme.getIBPageID(), iwc);
		}
		else { // Apply style to all pages
			result = setSiteStyle(theme.getIBPageID(), iwc);
		}
		helper.getThemesService().getBuilderService().clearAllCachedPages();
		return result;
	}
	
	private boolean setPageStyle(String pageID, int templateID, IWContext iwc) {
		ICPage page = null;
		if (templateID <= 0) {
			return false;
		}
		page = helper.getThemesService().getICPage(Integer.valueOf(pageID).intValue());
		if (page == null) {
			return false;
		}
		if (page.isPage()) {
			helper.getThemesService().getBuilderService().setTemplateId(pageID, String.valueOf(templateID));
			page.setTemplateId(templateID);
			page.store();
		}
		return true;
	}
	
	private boolean setSiteStyle(int templateID, IWContext iwc) {
		Map tree = helper.getThemesService().getBuilderService().getTree(iwc);
		if (tree == null) {
			return false;
		}
		Iterator it = tree.values().iterator();
		Object o = null;
		boolean result = true;
		while (it.hasNext()) {
			o = it.next();
			if (o instanceof ICTreeNode) {
				result = setPageStyle(((ICTreeNode) o).getId(), templateID, iwc);
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
		
		IWContext iwc = IWContext.getInstance();
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
	
	public String changePageUri(String pageID, String pageTitle, boolean needSetPageTitle) {
		if (pageID == null || pageTitle == null) {
			return null;
		}
		if (ThemesConstants.MINUS_ONE.equals(pageID)) {
			return null;
		}
		
		ICPage page = helper.getThemesService().getICPage(Integer.valueOf(pageID).intValue());
		if (pageTitle.equals(page.getDefaultPageURI())) {
			return null;
		}
		
		ICTreeNode parentNode = page.getParentNode();
		String parentId = null;
		if (parentNode != null) {
			parentId = parentNode.getId();
		}
		
		if (needSetPageTitle) {
			setPageTitle(pageID, pageTitle);
		}
		
		ICDomain domain = null;
		try {
			domain = helper.getThemesService().getBuilderService().getCurrentDomain();
		} catch (RemoteException e) {
			log.error(e);
			return null;
		}
		
		if (helper.getThemesService().getBuilderService().changePageUriByTitle(parentId, page, pageTitle, domain.getID())) {
			return page.getDefaultPageURI();
		}
		return null;
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
		IWContext iwc = IWContext.getInstance();
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
							changedPageUri = changePageUri(pageID, values[i], false);
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
								changedPageUri = changePageUri(pageID, values[i], false);
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
		IWContext iwc = IWContext.getInstance();
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
						ICPage page = helper.getThemesService().getICPage(Integer.valueOf(pageID).intValue());
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
		else { // System Settings
			if (domain == null) {
				try {
					domain = helper.getThemesService().getBuilderService().getCurrentDomain();
				} catch (RemoteException e) {
					log.error(e);
					return ThemesConstants.EMPTY;
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
		String[] elements = null;
		List <Setting> settings = new ArrayList<Setting>(c);
		elements = new String[settings.size()];
		for (int i = 0; i < settings.size(); i++) {
			elements[i] = settings.get(i).getCode();
		}
		return elements;
	}
	
	private boolean saveSiteInfoValue(String language, String keyword, String value, IWMainApplicationSettings settings,
			ICDomain domain, ICDomain cachedDomain) {
		if (language == null || keyword == null || value == null) {
			return false;
		}
		
		if (keyword.indexOf(ThemesConstants.SYSTEM_SETTINGS) == -1) {
			if (settings == null) {
				return false;
			}
			if (value == null || value.equals(ThemesConstants.EMPTY)) {
				settings.removeProperty(ThemesConstants.THEMES_PROPERTY_START + keyword + language);
			}
			else {
				settings.setProperty(ThemesConstants.THEMES_PROPERTY_START + keyword + language, value);
			}
		}
		else { // Saving System Settings
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
		
		IWContext iwc = IWContext.getInstance();
		if (iwc == null) {
			return false;
		}
		Locale l = iwc.getCurrentLocale();
		if (l == null) {
			return false;
		}
		String language = l.getLanguage();
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
	
//	public List <String> beforeCreatePage(List <String> struct, boolean isFirst){
	public List <String> beforeCreatePage(List <TreeNodeStructure> struct, Boolean isFirst){
		List <String> newIds = new ArrayList<String>();
		BuilderService builder = helper.getThemesService().getBuilderService();
		ICDomain domain = helper.getThemesService().getDomain();
		
		int pageID = -1;
		int domainID = -1;
		
		boolean isRootPage = false;
		boolean canCreate;
		
		if (domain != null) {
			domainID = domain.getID();
		}
		
		String uri = null;
		String lastTheme = null;
		String prevID = null;
		String pageType = builder.getPageKey();
		String format = builder.getIBXMLFormat();
		String realID = null;
		String webDAVUri = null;
		String subType = null;
		String parentID = null;
		String name = null;				
		for (int i = 0; i < struct.size(); i++) {			
			canCreate = true;
			try {
				prevID = struct.get(i).getNodeId();
				parentID = struct.get(i).getParentId();
				name = struct.get(i).getNodeName();
				subType = struct.get(i).getPageType();
				webDAVUri = struct.get(i).getTemplateFile();
			} catch (IndexOutOfBoundsException e) {
				log.error(e);
				canCreate = false;
			}
			if (!canCreate) {
				return newIds;
			}
			if (domain != null && parentID == null) {
				if (domain.getStartPage() == null) {
					uri = ThemesConstants.SLASH;
					isRootPage = true;
				}
			}
			
			pageID = createPage(parentID, name, pageType, null, uri, subType, domainID, format, null);
			realID = String.valueOf(pageID);
			
			if (domain != null){
				if((domain.getStartPage() == null) && (isFirst)){
					domain.setIBPage(helper.getThemesService().getICPage(pageID));
					domain.store();
				}					
			}
			
			if (isRootPage) { // Creating root page and root template
				createRootPage(pageID, domain, builder, domainID, format);
				createRootTemplate(domain, builder, domainID, format);
				initializeCachedDomain(ThemesConstants.DEFAULT_DOMAIN_NAME, domain);
				IWWelcomeFilter.unload();
			}

			uri = null;
			isRootPage = false;
			
			if (webDAVUri != null) {
				if (!webDAVUri.equals(ThemesConstants.EMPTY)) {
					String uriToPage = helper.loadPageToSlide(subType, pageID, webDAVUri);
					if (uriToPage != null) {
						helper.getThemesService().updatePageWebDav(pageID, uriToPage);
					}
					helper.createArticle(subType, pageID);
					lastTheme = helper.getLastUsedTheme();
					if (lastTheme != null) {
						helper.getThemesService().getBuilderService().setTemplateId(realID, lastTheme);
					}
				}
			}

			for (int j = i; j < struct.size(); j++) {
				try {
					if (struct.get(j).getParentId() != null) {
						if ((struct.get(j).getParentId()).equals(prevID)) {
System.out.println("before "+struct.get(j).getParentId());							
							struct.get(j).setParentId(realID);
System.out.println("after "+struct.get(j).getParentId());							
						}
					}
				} catch (IndexOutOfBoundsException e) {
					log.error(e);
				}
			}

			newIds.add(realID);
		}

		return newIds;
	}
	
	public int createPage(String parentId, String name, String type, String templateId, String pageUri, String subType, int domainId, String format, String sourceMarkup) {
		int id = -1;
		if (pageUri != null) {
			if (pageUri.equals(ThemesConstants.EMPTY)) {
				pageUri = null;
			}
		}
		try {
			id = helper.getThemesService().createIBPage(parentId, name, type, templateId, pageUri, subType, domainId, format, sourceMarkup);
		} catch (RemoteException e) {
			log.error(e);
			return -1;
		}
		return id;
	}
	
	public boolean deletePage(String pageId, boolean deleteChildren) {
		if (pageId == null) {
			return false;
		}
		try {
			helper.getThemesService().deleteIBPage(pageId, deleteChildren);
		} catch (RemoteException e) {
			log.error(e);
			return false;
		}
		return true;
	}
	
	public String getPageId() {
		String id = null;
		id = helper.getLastVisitedPage();
		if (id != null) {
			if (!ThemesConstants.MINUS_ONE.equals(id)) {
				return id;
			}
		}
		
		id = String.valueOf(getRootPageId());
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
	
	public boolean movePage(int newParentId, int nodeId) {
		IWContext iwc = IWContext.getInstance();
		if (iwc == null) {
			return false;
		}
		if (newParentId <= 0) {
			return helper.getThemesService().getBuilderService().movePageToTopLevel(nodeId, iwc);
		}
		return helper.getThemesService().getBuilderService().movePage(newParentId, nodeId, iwc.getDomain());
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
		
		ICDomain domain = null;
		try {
			domain = builder.getCurrentDomain();
		} catch (RemoteException e) {
			log.error(e);
			return null;
		}
		
		IWContext iwc = IWContext.getInstance();
		if (iwc == null) {
			return null;
		}
		
		ICPage newRootPage = helper.getThemesService().getICPage(newRoot);
		if (newRootPage == null) {
			return null;
		}
		domain.setIBPage(newRootPage); // Setting new start page in ICDomain
		domain.store();
		newRootPage.setDefaultPageURI(ThemesConstants.SLASH); // Changing uri to new start page
		newRootPage.store();
		
		ICPage rootPage = helper.getThemesService().getICPage(currentRoot);
		if (rootPage == null) {
			return null;
		}
		changePageUri(rootPage.getPageKey(), rootPage.getName().toLowerCase(), false);
		builder.createTopLevelPageFromExistingPage(currentRoot, domain, iwc);
		
		TreeableEntity parent = newRootPage.getParentEntity();
		if (parent instanceof ICPage) {
			ICPage parentPage = (ICPage) parent;
			try {
				parentPage.removeChild(newRootPage); // Removing new root as child from his old parent node
			} catch (SQLException e) {
				log.error(e);
			}
		}
		
		builder.clearAllCachedPages();
		
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

}