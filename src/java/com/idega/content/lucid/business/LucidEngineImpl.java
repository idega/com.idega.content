package com.idega.content.lucid.business;

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
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.faces.model.SelectItem;

import org.jdom.Document;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.context.annotation.Scope;
import org.springframework.stereotype.Service;

import com.idega.block.web2.business.JQuery;
import com.idega.block.web2.business.Web2Business;
import com.idega.block.web2.business.Web2BusinessBean;
import com.idega.builder.bean.AdvancedProperty;
import com.idega.builder.business.BuilderLogicWrapper;
import com.idega.content.business.ContentConstants;
import com.idega.content.business.ContentItemChecker;
import com.idega.content.business.ContentUtil;
import com.idega.content.lucid.bean.LucidApplicationInfo;
import com.idega.content.themes.business.ThemesEngine;
import com.idega.content.themes.business.ThemesEngineBean;
import com.idega.content.themes.helpers.bean.PageAccessibilityProperty;
import com.idega.content.themes.helpers.bean.Setting;
import com.idega.content.themes.helpers.bean.Theme;
import com.idega.content.themes.helpers.bean.TreeNodeStructure;
import com.idega.content.themes.helpers.business.ThemesConstants;
import com.idega.content.themes.helpers.business.ThemesHelper;
import com.idega.content.themes.presentation.PageInfo;
import com.idega.content.themes.presentation.SiteInfo;
import com.idega.core.accesscontrol.business.AccessControl;
import com.idega.core.accesscontrol.business.AccessController;
import com.idega.core.accesscontrol.business.StandardRoles;
import com.idega.core.builder.business.BuilderService;
import com.idega.core.builder.business.ICBuilderConstants;
import com.idega.core.builder.data.CachedDomain;
import com.idega.core.builder.data.ICDomain;
import com.idega.core.builder.data.ICPage;
import com.idega.core.builder.data.ICPageBMPBean;
import com.idega.core.data.ICTreeNode;
import com.idega.core.localisation.business.ICLocaleBusiness;
import com.idega.data.TreeableEntity;
import com.idega.idegaweb.IWBundle;
import com.idega.idegaweb.IWMainApplication;
import com.idega.idegaweb.IWMainApplicationSettings;
import com.idega.idegaweb.IWResourceBundle;
import com.idega.presentation.IWContext;
import com.idega.servlet.filter.IWWelcomeFilter;
import com.idega.util.ArrayUtil;
import com.idega.util.CoreConstants;
import com.idega.util.CoreUtil;
import com.idega.util.ListUtil;
import com.idega.util.LocaleUtil;
import com.idega.util.StringUtil;
import com.idega.util.expression.ELUtil;

@Scope(BeanDefinition.SCOPE_SINGLETON)
@Service(LucidEngine.SPRING_BEAN_IDENTIFIER)
public class LucidEngineImpl implements LucidEngine {

	private static final long serialVersionUID = 201381337142886542L;
	private static final Logger LOGGER = Logger.getLogger(LucidEngineImpl.class.getName());

	private static final String PAGE_URI = "pageUri";
	private static final String PAGE_TITLE = "pageTitle";
	private static final String ARTICLE_VIEWER_NAME = "Article Viewer";
	private static final String ARTICLE_VIEWER_SUBTYPE = "viewer";

	private String pathToImagesFolder = null;

	@Autowired
	private Web2Business web2;

	@Autowired
	private JQuery jQuery;

	@Autowired
	private BuilderLogicWrapper builderLogic;

	@Autowired
	private ThemesEngine themesEngine;

	@Autowired
	private ThemesHelper themesHelper;

	@Override
	public String getJavaScriptResources() {
		//	DWR
		StringBuilder js = new StringBuilder(CoreConstants.DWR_UTIL_SCRIPT).append(CoreConstants.COMMA);
		js.append(CoreConstants.DWR_ENGINE_SCRIPT).append(CoreConstants.COMMA);
		js.append("/dwr/interface/ThemesEngine.js,/dwr/interface/BuilderService.js,/dwr/interface/LucidEngine.js,");

		//	MooTools
		try {
			js.append(web2.getBundleURIToMootoolsLib()).append(CoreConstants.COMMA);
		} catch (RemoteException e) {
			LOGGER.log(Level.WARNING, "Error getting URI to MooTools script", e);
		}

		//	jQuery
		js.append(jQuery.getBundleURIToJQueryLib()).append(CoreConstants.COMMA);
		js.append(web2.getBundleUriToContextMenuScript(false)).append(CoreConstants.COMMA);

		//	Helpers
		IWBundle bundle = ContentUtil.getBundle();
		js.append(bundle.getVirtualPathWithFileNameString("javascript/drag-drop-folder-tree.js")).append(CoreConstants.COMMA);
		js.append(bundle.getVirtualPathWithFileNameString("javascript/ThemesHelper.js")).append(CoreConstants.COMMA);
		js.append(bundle.getVirtualPathWithFileNameString("javascript/LucidHelper.js")).append(CoreConstants.COMMA);
		js.append(bundle.getVirtualPathWithFileNameString("javascript/tree.js")).append(CoreConstants.COMMA);
		js.append(bundle.getVirtualPathWithFileNameString("javascript/SiteManagerHelper.js"));

		return js.toString();
	}

	@Override
	public String getJavaScriptResourcesForThemes() {
		StringBuilder js = new StringBuilder();

		//	MooTools
		try {
			js.append(web2.getBundleURIToMootoolsLib()).append(CoreConstants.COMMA);
		} catch (RemoteException e) {
			LOGGER.log(Level.WARNING, "Error getting URI to MooTools script", e);
		}
		js.append(web2.getReflectionForMootoolsScriptFilePath()).append(CoreConstants.COMMA);
		js.append(web2.getBundleUriToMooRainbowScript()).append(CoreConstants.COMMA);

		//	jQuery
		js.append(jQuery.getBundleURIToJQueryLib()).append(CoreConstants.COMMA);
		js.append(web2.getBundleUriToContextMenuScript(false)).append(CoreConstants.COMMA);

		//	DWR
		js.append(CoreConstants.DWR_ENGINE_SCRIPT).append(CoreConstants.COMMA).append("/dwr/interface/ThemesEngine.js,/dwr/interface/LucidEngine.js,");

		//	Helpers
		IWBundle bundle = ContentUtil.getBundle();
		js.append(bundle.getVirtualPathWithFileNameString("javascript/ThemesManagerHelper.js")).append(CoreConstants.COMMA);
		js.append(bundle.getVirtualPathWithFileNameString("javascript/ThemesSliderHelper.js")).append(CoreConstants.COMMA);
		js.append(bundle.getVirtualPathWithFileNameString("javascript/ThemesHelper.js"));

		return js.toString();
	}

	@Override
	public String getStyleSheetResources() {
		return ContentUtil.getBundle().getVirtualPathWithFileNameString("style/content.css");
	}

	@Override
	public String getStyleSheetResourcesForThemes() {
		return new StringBuilder(ContentUtil.getBundle().getVirtualPathWithFileNameString("style/content.css")).append(CoreConstants.COMMA)
								.append(web2.getBundleUriToMooRainbowStyle()).append(CoreConstants.COMMA)
								.append(ContentUtil.getBundle().getVirtualPathWithFileNameString("style/themes_manager.css"))
				.toString();
	}

	public Web2Business getWeb2() {
		return web2;
	}

	public void setWeb2(Web2Business web2) {
		this.web2 = web2;
	}

	public BuilderLogicWrapper getBuilderLogic() {
		return builderLogic;
	}

	public void setBuilderLogic(BuilderLogicWrapper builderLogic) {
		this.builderLogic = builderLogic;
	}

	private List<String> getThickBoxResources() {
		List<String> resources = new ArrayList<String>();
		try {
			resources.add(web2.getThickboxStyleFilePath());
		} catch (RemoteException e) {
			LOGGER.log(Level.WARNING, "Error getting URI to Thickbox style", e);
		}
		resources.add(jQuery.getBundleURIToJQueryLib());
		try {
			resources.add(web2.getThickboxScriptFilePath());
		} catch (RemoteException e) {
			LOGGER.log(Level.WARNING, "Error getting URI to Thickbox script", e);
		}
		return resources;
	}

	@Override
	public List<String> getPermissionWindowResources() {
		List<String> resources = getThickBoxResources();

		List<AdvancedProperty> parameters = Arrays.asList(new AdvancedProperty(ICBuilderConstants.UI_COMPONENT_IS_IN_LIGHTBOX, Boolean.TRUE.toString()));
		resources.add(0, getBuilderLogic().getBuilderService(IWMainApplication.getDefaultIWApplicationContext()).getUriToPagePermissionsWindow(parameters));

		return resources;
	}

	@Override
	public List<String> getPropertiesWindowResources() {
		List<String> resources = getThickBoxResources();

		List<AdvancedProperty> parameters = Arrays.asList(
				new AdvancedProperty(ICBuilderConstants.IB_CONTROL_PARAMETER, ICBuilderConstants.ACTION_EDIT),
				new AdvancedProperty(ICBuilderConstants.IC_OBJECT_INSTANCE_ID_PARAMETER, String.valueOf(-1)),
				new AdvancedProperty(ICBuilderConstants.UI_COMPONENT_IS_IN_LIGHTBOX, Boolean.TRUE.toString())
		);
		resources.add(0, getBuilderLogic().getBuilderService(IWMainApplication.getDefaultIWApplicationContext()).getUriToPagePropertiesWindow(parameters));

		return resources;
	}

	@Override
	public boolean isContentEditor() {
		if (isSuperAdmin()) {
			return true;
		}

		try {
			return CoreUtil.getIWContext().hasRole(StandardRoles.ROLE_KEY_EDITOR);
		} catch(Exception e) {
			LOGGER.log(Level.WARNING, "Error while determining if user is content editor", e);
		}

		return false;
	}

	@Override
	public boolean isSuperAdmin() {
		IWContext iwc = CoreUtil.getIWContext();
		if (iwc == null) {
			return false;
		}

		try {
			if (!iwc.isLoggedOn()) {
				return false;
			}
		} catch(Exception e) {
			LOGGER.log(Level.WARNING, "Error while determining if user is logged", e);
			return false;
		}

		return iwc.isSuperAdmin();
	}

	@Override
	public Collection<SelectItem> getAvailableLocales() {
		List<SelectItem> availableLocales = new ArrayList<SelectItem>();

		List<Locale> locales = ICLocaleBusiness.getListOfLocalesJAVA();
		if (ListUtil.isEmpty(locales)) {
			return availableLocales;
		}

		Locale currentLocale = null;
		IWContext iwc = getThemesEngine().getContextAndCheckRights();
		if (iwc == null) {
			return availableLocales;
		}

		currentLocale = iwc.getCurrentLocale();
		if (currentLocale == null) {
			currentLocale = Locale.ENGLISH;
		}

		for (Locale locale: locales) {
			availableLocales.add(new SelectItem(locale.toString(), locale.getDisplayName(currentLocale)));
		}

		availableLocales.add(new SelectItem(String.valueOf(-1),
				ContentUtil.getBundle().getResourceBundle(currentLocale).getLocalizedString("lucid.change_locale", "Change locale")));

		return availableLocales;
	}

	@Override
	public boolean setLocale(String locale) {
		if (StringUtil.isEmpty(locale)) {
			return false;
		}

		IWContext iwc = getThemesEngine().getContextAndCheckRights();
		if (iwc == null) {
			return false;
		}

		Locale newLocale = LocaleUtil.getLocale(locale);
		if (newLocale != null && !newLocale.equals(locale)) {
			iwc.setCurrentLocale(newLocale);
			return true;
		}

		return false;
	}

	@Override
	public String getCurrentLocaleValue() {
		IWContext iwc = getThemesEngine().getContextAndCheckRights();
		if (iwc != null) {
			Locale locale = iwc.getCurrentLocale();
			if (locale != null) {
				return locale.toString();
			}
		}
		return String.valueOf(-1);
	}

	@Override
	public String changePageUri(String pageKey, String pageUri, boolean needSetPageTitle) {
		if (pageKey == null || pageUri == null) {
			return null;
		}
		if (ThemesConstants.MINUS_ONE.equals(pageKey)) {
			return null;
		}

		ICPage page = getThemesHelper().getThemesService().getICPage(pageKey);
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
		IWContext iwc = getThemesEngine().getContextAndCheckRights();
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

		if (getThemesHelper().getThemesService().getBuilderService().changePageUriByTitle(parentId, page, pageUri, domain.getID())) {
			setNewLinkInArticleFile(page.getId(), CoreConstants.getArticleItemViewerClass().getName(), page.getDefaultPageURI());
			return page.getDefaultPageURI();
		}
		return null;
	}

	@Override
	public boolean setNewLinkInArticleFile(String pageKey, String moduleClass, String pageUri) {
		if (pageKey == null || moduleClass == null || pageUri == null) {
			return false;
		}

		BuilderService builder = getBuilderService();
		IWContext iwc = getThemesEngine().getContextAndCheckRights();
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
		StringBuffer link = new StringBuffer(getThemesHelper().getFullWebRoot()).append(linkToArticle[0]);
		if (!link.toString().endsWith(ContentConstants.SLASH)) {
			link.append(ContentConstants.SLASH);
		}
		Locale l = null;
		boolean result = true;
		for (int i = 0; i < locales.size(); i++) {
			l = locales.get(i);
			if (l.getLanguage() != null) {
				result = getThemesHelper().setNewLinkInArticleFile(iwc, link.toString(), l.getLanguage(), linkToArticle[0], pageUri);
			}
		}

		return result;
	}

	@Override
	public String savePageInfo(String pageKey, String[] keywords, String[] values) {
		if (pageKey == null || keywords == null || values == null) {
			return null;
		}
		if (ThemesConstants.MINUS_ONE.equals(pageKey)) {
			return null;
		}
		if (keywords.length != values.length) {
			return null;
		}
		IWContext iwc = getThemesEngine().getContextAndCheckRights();
		if (iwc == null) {
			return null;
		}
		IWMainApplication appl = iwc.getIWMainApplication();
		if (appl == null) {
			return null;
		}
		String changedPageUri = null;

		List<Setting> settings = getThemesHelper().getPageSettings();
		if (ListUtil.isEmpty(settings)) {
			return null;
		}

		int index = 0;
		String currentValue = null;
		String[] currentValues = null;
		String[] newValues = null;
		boolean changedPageTitle = false;
		boolean needSetValue = true;
		for (Setting s: settings) {
			needSetValue = true;
			currentValue = values[index];

			currentValues = getThemesHelper().getThemesService().getBuilderService().getPropertyValues(appl, pageKey, ThemesConstants.MINUS_ONE,
					s.getMethod(), null, true);
			if (StringUtil.isEmpty(currentValue)) {
				if (currentValues != null) {
					getThemesHelper().getThemesService().getBuilderService().removeProperty(appl, pageKey, ThemesConstants.MINUS_ONE,
							s.getMethod(), currentValues);
				}
			}
			else {
				if (s.getCode().equals(PAGE_URI)) {
					if (!changedPageTitle) {
						changedPageUri = setPageUri(pageKey, currentValue);	//	Setting user's uri
					}
				}
				else if (ContentConstants.HIDE_MENU_IN_PAGE.equals(s.getCode())) {
					setValueForPage(pageKey, currentValue, ICPageBMPBean.HIDE_PAGE_IN_MENU);
				}
				else if (ContentConstants.PUBLISH_PAGE_IN_LUCID_CODE.equals(s.getCode())) {
					setValueForPage(pageKey, currentValue, ICPageBMPBean.PAGE_IS_PUBLISHED);
				}
				else if (ContentConstants.SET_PAGE_LOCKED_IN_LUCID_CODE.equals(s.getCode())) {
					setPageAvailability(iwc, pageKey, currentValue);
					setValueForPage(pageKey, currentValue, ICPageBMPBean.PAGE_IS_LOCKED);
				}
				else if (ContentConstants.PAGE_IS_CATEGORY_TYPE.equals(s.getCode())) {
					setValueForPage(pageKey, currentValue, ICPageBMPBean.IS_CATEGORY, false);
					getBuilderService().clearAllCaches();
				}
				else {
					newValues = getThemesHelper().getPageValues(s, currentValue);
					if (newValues == null) {
						needSetValue = false;
					}
					if (Arrays.deepEquals(newValues, currentValues)) {
						needSetValue = false;
					}
					if (needSetValue) {
						getThemesHelper().getThemesService().getBuilderService().setProperty(pageKey, ThemesConstants.MINUS_ONE, s.getMethod(),
								newValues, appl);
						if (s.getCode().equals(PAGE_TITLE)) {
							changedPageTitle = changePageName(Integer.valueOf(pageKey).intValue(), currentValue, iwc);
							changedPageUri = changePageUri(pageKey, currentValue, true);	//	Changing uri by new name
						}
					}
				}
			}

			index++;
		}

		if (changedPageUri != null) {
			getThemesEngine().updateSiteTree(false);
		}

		return changedPageUri;
	}

	@Override
	public String[] getPageInfoValues(String pageKey, String[] keywords) {
		if (pageKey == null || keywords == null) {
			return null;
		}
		if (ThemesConstants.MINUS_ONE.equals(pageKey)) {
			return null;
		}
		IWContext iwc = getThemesEngine().getContextAndCheckRights();
		if (iwc == null) {
			return null;
		}
		IWMainApplication appl = iwc.getIWMainApplication();
		if (appl == null) {
			return null;
		}
		List<Setting> settings = getThemesHelper().getPageSettings();
		if (ListUtil.isEmpty(settings)) {
			return null;
		}

		List<String> values = new ArrayList<String>(keywords.length);
		String[] propValues = null;
		StringBuffer value = null;
		ICPage page = getThemesHelper().getThemesService().getICPage(pageKey);
		for (Setting s: settings) {
			value = new StringBuffer();

			propValues = getThemesHelper().getThemesService().getBuilderService().getPropertyValues(appl, pageKey, ThemesConstants.MINUS_ONE,
					s.getMethod(), null, true);
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
					if (page != null) {
						value.append(page.getDefaultPageURI());
					}
				}
				if (ContentConstants.HIDE_MENU_IN_PAGE.equals(s.getCode())) {
					value.append(page.isHidePageInMenu());
				}
				if (ContentConstants.PUBLISH_PAGE_IN_LUCID_CODE.equals(s.getCode())) {
					value.append(page.isPublished());
				}
				if (ContentConstants.SET_PAGE_LOCKED_IN_LUCID_CODE.equals(s.getCode())) {
					value.append(page.isLocked());
				}
				if (ContentConstants.PAGE_IS_CATEGORY_TYPE.equals(s.getCode())) {
					value.append(page.isCategory());
				}
			}

			values.add(value.toString());
		}

		return ArrayUtil.convertListToArray(values);
	}

	@Override
	public String[] getPageInfoElements() {
		Collection<Setting> c = getThemesHelper().getPageSettings();
		if (c == null) {
			return null;
		}
		return getElements(c);
	}

	@Override
	public String[] getSiteInfoElements() {
		Collection <Setting> c = getThemesHelper().getThemeSettings();
		if (c == null) {
			return null;
		}
		return getElements(c);
	}

	@Override
	public String[] getSiteInfoValues(String[] keywords, String language) {
		if (keywords == null || language == null) {
			return null;
		}
		Collection <Setting> c = getThemesHelper().getThemeSettings();
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

	@Override
	public String getSiteInfoValue(String keyword, String language, IWMainApplicationSettings settings, ICDomain domain) {
		if (keyword == null || language == null || settings == null) {
			return ThemesConstants.EMPTY;
		}
		keyword = keyword.indexOf("_PAGE_URI") == -1 ? ThemesConstants.THEMES_PROPERTY_START + keyword + CoreConstants.DOT + language : keyword;
		if (keyword.indexOf(ThemesConstants.SYSTEM_SETTINGS) == -1 && keyword.indexOf("_PAGE_URI") == -1) {
			return settings.getProperty(keyword);
		}
		else if (keyword.indexOf(ThemesConstants.SYSTEM_SETTINGS) != -1) {
			//	System Settings
			if (domain == null) {
				IWContext iwc = getThemesEngine().getContextAndCheckRights();
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
		} else if (keyword.indexOf("_PAGE_URI") != -1) {
			String applicationProperty = settings.getProperty(keyword);
			return StringUtil.isEmpty(applicationProperty) ? CoreConstants.EMPTY : applicationProperty;
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

		if (keyword.indexOf(ThemesConstants.SYSTEM_SETTINGS) == -1 && keyword.indexOf("_PAGE_URI") == -1) {
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
		else if (keyword.indexOf(ThemesConstants.SYSTEM_SETTINGS) != -1) {
			//	Saving System Settings
			if (cachedDomain == null) {
				return false;
			}
			if (domain == null) {
				domain = getThemesHelper().getThemesService().getDomain();
			}
			if (value != null && !ThemesConstants.EMPTY.equals(value)) {
				if (domain != null) {
					if (keyword.indexOf(ThemesConstants.DOMAIN_NAME) != -1) {
						domain.setDomainName(value);
						domain.setName(value);
						cachedDomain.setDomainName(value);
						cachedDomain.setName(value);
					}
					if (keyword.indexOf(ThemesConstants.DOMAIN_SERVER_NAME) != -1) {
						domain.setServerName(value);
						cachedDomain.setServerName(value);
					}
					domain.store();
				}
			}
		} else if (keyword.indexOf("_PAGE_URI") != -1) {
			if (settings == null) {
				return false;
			}

			if (StringUtil.isEmpty(value)) {
				settings.removeProperty(keyword);
			}
			else {
				settings.setProperty(keyword, value);
			}
		}

		return true;
	}

	@Override
	public boolean saveSiteInfoValue(String keyword, String value) {
		if (keyword == null || value == null) {
			return false;
		}

		IWContext iwc = getThemesEngine().getContextAndCheckRights();
		if (iwc == null) {
			return false;
		}
		String language = getThemesHelper().getCurrentLanguage(iwc);
		if (language == null) {
			return false;
		}
		language = CoreConstants.DOT + language;
		keyword = getThemesHelper().extractValueFromString(keyword, 0, keyword.lastIndexOf(CoreConstants.UNDER));

		IWMainApplication appl = ContentUtil.getBundle().getApplication();
		ICDomain cachedDomain = null;
		if (appl != null) {
			cachedDomain = appl.getIWApplicationContext().getDomain();
		}

		return saveSiteInfoValue(language, keyword, value, appl.getSettings(), null, cachedDomain);
	}

	@Override
	public boolean saveSiteInfo(String language, String[] keywords, String[] values) {
		if (language == null || keywords == null || values == null) {
			return false;
		}
		if (keywords.length != values.length) {
			return false;
		}
		IWMainApplication application = ContentUtil.getBundle().getApplication();
		IWMainApplicationSettings settings  = application.getSettings();
		language = CoreConstants.DOT + language;
		ICDomain domain = null;
		ICDomain cachedDomain = application.getIWApplicationContext().getDomain();
		for (int i = 0; i < keywords.length; i++) {
			saveSiteInfoValue(language, keywords[i], values[i], settings, domain, cachedDomain);
		}

		getThemesEngine().updateSiteTree(false);

		return true;
	}

	@Override
	public List <String> createPage(List<TreeNodeStructure> struct, Boolean isTopLevelPage, String numberInLevel, List<String> followingNodes) {
		List <String> newIds = new ArrayList<String>();

		if (struct == null || numberInLevel == null) {
			return newIds;
		}
		if (struct.size() == 0) {
			return newIds;
		}

		struct.get(0).setTreeOrder(numberInLevel);
		struct = getOrderInLevel(struct);

		IWContext iwc = getThemesEngine().getContextAndCheckRights();
		if (iwc == null) {
			return newIds;
		}

		BuilderService builder = getBuilderService();

		ICDomain domain = getThemesHelper().getThemesService().getDomain();

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
					domain.setIBPage(getThemesHelper().getThemesService().getICPage(pageID));
					domain.store();
				}
			}

			if (isRootPage) {
				//	Creating root page and root template
				createRootPage(pageID, domain, builder, domainID, format);
				createRootTemplate(domain, builder, domainID, format);
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
		String lastUsedTemplate = getThemesHelper().getLastUsedTheme();
		if (lastUsedTemplate != null && !CoreConstants.EMPTY.equals(lastUsedTemplate)) {
			String createdPageKey = null;
			Theme theme = getThemesHelper().getThemeByTemplateKey(lastUsedTemplate);
			for (int i = 0; i < createdPages.size(); i++) {
				createdPageKey = createdPages.get(i);

				getThemesEngine().setLastUsedTemplate(createdPageKey, lastUsedTemplate);

				addExtraRegionsToPage(createdPageKey, theme);
			}
		}

		//	Creating new tree order
		changeNodesOrderInLevel(followingNodes, -1, null);

		getThemesEngine().updateSiteTree(false);

		return newIds;
	}

	private boolean addExtraRegionsToPage(String pageKey, Theme theme) {
		if (pageKey == null || theme == null) {
			return false;
		}

		List<AdvancedProperty> extraRegions = theme.getExtraRegions();
		if (extraRegions == null || extraRegions.size() == 0) {
			return true;
		}

		BuilderService service = getBuilderService();

		for (int i = 0; i < extraRegions.size(); i++) {
			if (!getThemesEngine().addExtraRegionToPage(pageKey, extraRegions.get(i), service)) {
				return false;
			}
		}

		return true;
	}

	private int createPage(String parentId, String name, String type, String templateId, String pageUri, String subType, int domainId, String format,
			String sourceMarkup) {
		return createPage(parentId, name, type, templateId, pageUri, subType, domainId, format, sourceMarkup, null);
	}

	private boolean preparePage(String templateFile, String pageType, int pageID, String realID, boolean clearCache) {
		if (templateFile == null || pageType == null || pageID < 0 || realID == null) {
			return false;
		}
		if (ThemesConstants.EMPTY.equals(templateFile)) {
			return false;
		}
		List<String> articlesPaths = getThemesHelper().createArticle(templateFile, pageID);
		String uriToPage = getThemesHelper().loadPageToSlide(pageType, templateFile, articlesPaths, pageID);
		if (uriToPage == null) {
			return false;
		}

		return getThemesHelper().getThemesService().updatePageWebDav(pageID, uriToPage, clearCache);
	}

	private int createPage(String parentId, String name, String type, String templateId, String pageUri, String subType, int domainId, String format,
			String sourceMarkup, String treeOrder) {
		int id = -1;
		if (pageUri != null) {
			if (pageUri.equals(ThemesConstants.EMPTY)) {
				pageUri = null;
			}
		}
		try {
			id = getThemesHelper().getThemesService().createIBPage(parentId, name, type, templateId, pageUri, subType, domainId, format,
					sourceMarkup, treeOrder);
		} catch (RemoteException e) {
			e.printStackTrace();
			return -1;
		}
		return id;
	}

	@Override
	public boolean deletePage(String pageId, boolean deleteChildren) {
		return deletePageAndDecrease(pageId, deleteChildren, null);
	}

	@Override
	public boolean deletePageAndDecrease(String pageId, boolean deleteChildren, List<String> followingNodes) {
		if (pageId == null) {
			return false;
		}

		try {
			changeNodesOrderInLevel(followingNodes, -1, null);
			getThemesHelper().getThemesService().deleteIBPage(pageId, deleteChildren, true);
		} catch (RemoteException e) {
			e.printStackTrace();
			return false;
		}

		getThemesEngine().updateSiteTree(false);

		return true;
	}

	private boolean isPageDeleted(String pageID) {
		if (pageID == null) {
			return true;
		}
		ICPage page = getThemesHelper().getThemesService().getICPage(pageID);
		if (page == null) {
			return true;
		}
		return page.getDeleted();
	}

	@Override
	public String getPageIdByUri(String uri) {
		if (uri == null) {
			return null;
		}

		try {
			return getBuilderService().getPageKeyByURI(uri);
		} catch(Exception e) {
		}

		return null;
	}

	@Override
	public String getPageId() {
		String id = getThemesHelper().getLastVisitedPage();
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
		getThemesHelper().setLastVisitedPage(id);
		return id;
	}

	@Override
	public boolean setPageId(String id) {
		if (id == null) {
			return false;
		}
		getThemesHelper().setLastVisitedPage(id);
		return true;
	}

	@Override
	public boolean movePage(int newParentId, int nodeId, int numberInLevel, List<String> nodesToIncrease, List<String> nodesToDecrease) {
		IWContext iwc = getThemesEngine().getContextAndCheckRights();
		boolean result = false;
		if (iwc == null) {
			return false;
		}

		BuilderService service = getBuilderService();

		ICPage page = getThemesHelper().getThemesService().getICPage(nodeId);
		page.setTreeOrder(numberInLevel);
		page.store();
		service.setTreeOrder(nodeId, numberInLevel);

		if (nodesToIncrease != null) {
			changeNodesOrderInLevel(nodesToIncrease, 1, service);
		}

		if (nodesToDecrease != null) {
			changeNodesOrderInLevel(nodesToDecrease, -1, service);
		}

		if (newParentId < 0) {
			result = service.movePageToTopLevel(nodeId, iwc);
		}
		else {
			result = service.movePage(newParentId, nodeId, iwc.getDomain());
		}

		if (result) {
			getThemesEngine().updateSiteTree(false);
		}

		return result;
	}

	@Override
	public String getPathToImageFolder(){
		return getPathToImagesFolder();
	}

	@Override
	public boolean isStartPage(String pageKey) {
		if (pageKey == null) {
			pageKey = getThemesHelper().getLastVisitedPage();
		}
		if (pageKey == null) {
			return true;	//	Returning true to disable a button
		}
		int id = -1;
		try {
			id = Integer.valueOf(pageKey);
		} catch (NumberFormatException e) {
			e.printStackTrace();
			return true;	//	Returning true to disable a button
		}
		if (id < 0) {
			return true;	//	Returning true to disable a button
		}
		if (id == getRootPageId()) {
			return true;
		}
		return false;
	}

	@Override
	public boolean setAsStartPage(String pageKey) {
		if (pageKey == null) {
			return false;
		}
		int newRoot = -1;
		try {
			newRoot = Integer.valueOf(pageKey).intValue();
		} catch (NumberFormatException e) {
			e.printStackTrace();
			return false;
		}
		if (newRoot < 0) {
			return false;
		}

		int currentRoot = getRootPageId();
		if (currentRoot == newRoot) {
			return false;
		}

		BuilderService builder = getBuilderService();

		IWContext iwc = getThemesEngine().getContextAndCheckRights();
		if (iwc == null) {
			return false;
		}

		ICPage newRootPage = getThemesHelper().getThemesService().getICPage(newRoot);
		if (newRootPage == null) {
			return false;
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
		ICPage rootPage = getThemesHelper().getThemesService().getICPage(currentRoot);
		if (rootPage == null) {
			return false;
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
				e.printStackTrace();
			}
		}

		builder.clearAllCaches();

		getThemesEngine().updateSiteTree(true, false);

		return true;
	}

	private int getRootPageId() {
		int id = 1;
		try {
			id = getThemesHelper().getThemesService().getBuilderService().getRootPageId();
		} catch (Exception e) {
			e.printStackTrace();
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

		domain.setIBPage(getThemesHelper().getThemesService().getICPage(pageID));
		domain.store();
		return true;
	}

	@Override
	public String createRootTemplate(ICDomain domain, BuilderService builder, int domainID, String format) {
		ICPage startTemplate = domain.getStartTemplate();
		if (startTemplate != null && !startTemplate.getDeleted()) {
			return startTemplate.getId();
		}

		int templateId = createPage(null, "Template", builder.getTemplateKey(), null, null, null, domainID, format, null);
		String templateKey = String.valueOf(templateId);

		builder.unlockRegion(templateKey, ThemesConstants.MINUS_ONE, null);

		domain.setStartTemplate(getThemesHelper().getThemesService().getICPage(templateId));
		domain.store();

		createArticlePreviewTemplate(domainID, builder, format, templateId);

		return templateKey;
	}

	private boolean createArticlePreviewTemplate(int domainID, BuilderService builder, String format, int id) {
		if (builder == null) {
			return false;
		}
		IWContext iwc = getThemesEngine().getContextAndCheckRights();

		String articleTemplateFile = "/idegaweb/bundles/com.idega.block.article.bundle/resources/pages/article_viewer_template.xml";
		if (id < 0) {
			id = getArticleViewerTemplateId(builder, iwc);
		}

		if (id == -1) {
			id = createPage(null, ARTICLE_VIEWER_NAME, builder.getTemplateKey(), null, ContentConstants.ARTICLE_VIEWER_URI, ARTICLE_VIEWER_SUBTYPE, domainID, format,
					null);
		}

		if (id < 0) {
			return false;
		}

		String pageKey = String.valueOf(id);
		boolean result = preparePage(articleTemplateFile, ARTICLE_VIEWER_SUBTYPE, id, pageKey, true);
		getThemesEngine().setLastUsedTemplate(pageKey);
		if (iwc == null) {
			return result;
		}
		iwc.getApplicationSettings().setProperty(ThemesEngineBean.ARTICLE_VIEWER_TEMPLATE_KEY, pageKey);
		return result;
	}

	private int getArticleViewerTemplateId(BuilderService builder, IWContext iwc) {
		if (builder == null || iwc == null) {
			return -1;
		}
		@SuppressWarnings("rawtypes")
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
		for (@SuppressWarnings("rawtypes")
		Iterator it = templates.iterator(); it.hasNext();) {
			o = it.next();
			if (o instanceof ICTreeNode) {
				treeNode = (ICTreeNode) o;
				if (ARTICLE_VIEWER_NAME.equals(treeNode.getNodeName())) {
					try {
						id = Integer.valueOf(treeNode.getId());
					} catch (NumberFormatException e) {
						e.printStackTrace();
						return -1;
					}
					page = getThemesHelper().getThemesService().getICPage(id);
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

	@Override
	public boolean initializeCachedDomain(String domainName, ICDomain domain) {
		ICDomain cachedDomain = IWMainApplication.getDefaultIWMainApplication().getIWApplicationContext().getDomain();
		if (cachedDomain.getDomainName() == null) {
			cachedDomain.setDomainName(domainName);
		}
		cachedDomain.setIBPage(domain.getStartPage());
		cachedDomain.setStartTemplate(domain.getStartTemplate());
		if (cachedDomain instanceof CachedDomain) {
			CachedDomain ccachedDomain = (CachedDomain)cachedDomain;
			ccachedDomain.setStartTemplateID(domain.getStartTemplateID());
			ccachedDomain.setStartPage(domain.getStartPage());
			ccachedDomain.setStartPageID(domain.getStartPageID());
		}
		return true;
	}

	private boolean changeNodesOrderInLevel(List<String> nodes, int orderChange, BuilderService service) {
		if (nodes == null || nodes.isEmpty()) {
			return false;
		}
		if (service == null) {
			service = getThemesHelper().getThemesService().getBuilderService();
		}
		if (service == null) {
			return false;
		}

		int id = -1;
		ICPage page = null;
		for (String nodeId: nodes) {
			id = -1;
			page = null;

			try {
				id = Integer.valueOf(nodeId);
			} catch (NumberFormatException e) {
				e.printStackTrace();
			} catch (NullPointerException e) {
				e.printStackTrace();
			}

			if (id != -1) {
				page = getThemesHelper().getThemesService().getICPage(nodeId);
				if (page != null) {
					page.setTreeOrder(page.getTreeOrder() + orderChange);
					service.changeTreeOrder(id, orderChange);
					page.store();
				}
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
					number = (children.get(parentId)) + 1;
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

	private boolean manageNewSiteTreeOrder(IWContext iwc, BuilderService builder, ICPage newRootPage, int newRoot) {
		if (iwc == null || builder == null || newRootPage == null || newRoot == -1) {
			return false;
		}
		@SuppressWarnings("unchecked")
		Collection<ICTreeNode> topLevelPages = builder.getTopLevelPages(iwc);
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
			for (Iterator<ICTreeNode> iter = topLevelPages.iterator(); iter.hasNext();) {
				element = iter.next();
				page = getThemesHelper().getThemesService().getICPage(element.getId());
				newPage = getThemesHelper().getThemesService().getICPage(newRoot);
				if (page != null && newPage != null) {
					nodeOrder = page.getTreeOrder();
					newRootOrder = newPage.getTreeOrder();
					if (nodeOrder < newRootOrder) {
						increaseLevelOnTop.add(element.getId());
					}
				}
			}
			changeNodesOrderInLevel(increaseLevelOnTop, -1, builder);
		}
		else {
			//	Not top level page
			for (Iterator<ICTreeNode> iter = topLevelPages.iterator(); iter.hasNext();) {
				element = iter.next();
				page = getThemesHelper().getThemesService().getICPage(element.getId());
				if (page != null) {
					page.setTreeOrder(page.getTreeOrder()+1);
					page.store();
				}
			}

			List<String> decreaseLevelOnTop = new ArrayList<String>();
			@SuppressWarnings("unchecked")
			Collection<ICTreeNode> siblings = newRootPage.getParentNode().getChildren();
			if (siblings == null) {
				return false;
			}
			for (Iterator<ICTreeNode> iter = siblings.iterator(); iter.hasNext();) {
				element = iter.next();
				page = getThemesHelper().getThemesService().getICPage(element.getId());
				newPage = getThemesHelper().getThemesService().getICPage(newRoot);
				if (page != null && newPage != null) {
					nodeOrder = page.getTreeOrder();
					newRootOrder = newPage.getTreeOrder();
					if (nodeOrder > newRootOrder) {
						decreaseLevelOnTop.add(element.getId());
					}
				}
			}
			changeNodesOrderInLevel(decreaseLevelOnTop, -1, builder);
		}
		return true;
	}

	private List<String> getLocalizedText(IWContext iwc) {
		List<String> texts = new ArrayList<String>();
		IWResourceBundle iwrb = null;
		try {
			iwrb = ContentUtil.getBundle().getResourceBundle(iwc);
		} catch (Exception e) {
			e.printStackTrace();
		}
		if (iwrb == null) {
			return texts;
		}

		try {
			texts.add(iwrb.getLocalizedString("uploading_theme", "Uploading..."));							//	0
			texts.add(iwrb.getLocalizedString("changing_theme", "Changing..."));							//	1
			texts.add(iwrb.getLocalizedString("saving", "Saving..."));										//	2
			texts.add(iwrb.getLocalizedString("generating_preview", "Generating preview..."));				//	3
			texts.add(iwrb.getLocalizedString("restoring_theme", "Restoring..."));							//	4
			texts.add(iwrb.getLocalizedString("hide_themes", "Hide Themes"));								//	5
			texts.add(iwrb.getLocalizedString("show_themes", "Show Themes"));								//	6
			texts.add(iwrb.getLocalizedString("style_for_current_page", "Select style for current page"));	//	7
			texts.add(iwrb.getLocalizedString("style_for_site", "Select style for all pages"));				//	8
			texts.add(iwrb.getLocalizedString("applying_style", "Applying style..."));						//	9
			texts.add(iwrb.getLocalizedString("close", "Close"));											//	10
			texts.add(iwrb.getLocalizedString("start_page_text", "Start Page"));							//	11
			texts.add(iwrb.getLocalizedString("make_start_page", "Start Page"));							//	12
			texts.add(iwrb.getLocalizedString("changing_structure", "Changing structure..."));				//	13
			texts.add(iwrb.getLocalizedString("new_page", "New Page"));										//	14
			texts.add(iwrb.getLocalizedString("moving", "Moving..."));										//	15
			texts.add(iwrb.getLocalizedString("are_you_sure", "Are you sure?"));							//	16
			texts.add(iwrb.getLocalizedString("deleting", "Deleting..."));									//	17
			texts.add(iwrb.getLocalizedString("page", "Page"));												//	18
			texts.add(iwrb.getLocalizedString("site", "Site"));												//	19
			texts.add(iwrb.getLocalizedString("drop_templates_here", "Drop templates here"));				//	20
			texts.add(iwrb.getLocalizedString("no_page_exist", "No page exist"));							//	21
			texts.add(iwrb.getLocalizedString("loading", "Loading..."));									//	22
			texts.add(iwrb.getLocalizedString("make_this_page_start_page", "Make This Page As Start Page"));//	23
			texts.add(iwrb.getLocalizedString("reloading", "Reloading..."));								//	24
			texts.add(iwrb.getLocalizedString("show_modules", "Show Modules"));								//	25
			texts.add(iwrb.getLocalizedString("hide_modules", "Hide Modules"));								//	26
			texts.add(iwrb.getLocalizedString("redirecting", "Redirecting..."));							//	27
			texts.add(iwrb.getLocalizedString("creating", "Creating..."));									//	28
			texts.add(iwrb.getLocalizedString("new_pages", "New Pages"));									//	29
			texts.add(iwrb.getLocalizedString("preparing", "Preparing..."));								//	30
			texts.add(iwrb.getLocalizedString("style_for_page_and_children", "Select style for current page and all children"));//	31
			texts.add(iwrb.getLocalizedString("choose_style_for_page_and_children", "Page*"));				//	32
			texts.add(iwrb.getLocalizedString("select_template_first", "Select template first!"));			//	33
			texts.add(iwrb.getLocalizedString("are_you_sure_you_want_apply_this_template", "Are you sure you want to apply this template?"));	 //	34
			texts.add(iwrb.getLocalizedString("insufficient_rights_for_this_action", "Sorry, you have insufficient rights for this action!"));	//	35
			texts.add(iwrb.getLocalizedString("theme_can_not_be_deleted", "Sorry, selected theme can not be deleted."));		//	36
			texts.add(iwrb.getLocalizedString("error_in_lucid", "Sorry, error occurred... Reloading page might help to avoid it. Do you want to reload page?"));	//	37

		} catch (Exception e) {
			e.printStackTrace();
		}
		return texts;
	}

	private boolean startBuilderApplication(IWContext iwc) {
//		IWContext iwc = getThemesEngine().getContextAndCheckRights();
//		if (iwc == null) {
//			return false;
//		}

		BuilderService builder = getBuilderService();
		builder.startBuilderSession(iwc);
		return true;
	}

	public boolean canUserActAsBuilderUser() {
		IWContext iwc = getThemesEngine().getContextAndCheckRights();
		if (iwc == null) {
			return false;
		}

		if (iwc.isSuperAdmin()) {
			return true;
		}

		ICPage page = getThemesHelper().getThemesService().getICPage(iwc.getCurrentIBPageID());
		if (page == null) {
			return false;
		}
		if (page.isPublished() && !iwc.hasRole(StandardRoles.ROLE_KEY_EDITOR)) {
			return false;
		}

		return iwc.hasRole(StandardRoles.ROLE_KEY_EDITOR) || iwc.hasRole(StandardRoles.ROLE_KEY_AUTHOR);
	}

	public String getPageUri(String pageKey) {
		if (pageKey == null || ThemesConstants.MINUS_ONE.equals(pageKey)) {
			return null;
		}

		IWContext iwc = getThemesEngine().getContextAndCheckRights();
		if (iwc == null) {
			return null;
		}

		String uri = null;
		BuilderService builderService = getBuilderService();

		try {
			uri = builderService.getPageURI(iwc, pageKey, true);
			if (uri != null) {
				builderService.setCurrentPageId(iwc, String.valueOf(pageKey));
			}
		} catch (Exception e) {
			e.printStackTrace();
			return null;
		}

		return uri;
	}

	public boolean changePageName(int id, String newName) {
		IWContext iwc = getThemesEngine().getContextAndCheckRights();
		if (iwc == null) {
			return false;
		}

		if (changePageName(id, newName, iwc)) {
			getThemesEngine().updateSiteTree(false);
			return true;
		}

		return false;
	}

	private boolean changePageName(int id, String newName, IWContext iwc) {
		if (id < 0 || newName == null) {
			return false;
		}

		BuilderService builder = getBuilderService();

		if (builder.changePageName(id, newName, iwc)) {
			builder.clearAllCaches();
			return true;
		}

		return false;
	}

	@SuppressWarnings("unchecked")
	public boolean deleteArticlesFromDeletedPages(String pageKey) {
		if (pageKey == null) {
			return false;
		}

		IWContext iwc = getThemesEngine().getContextAndCheckRights();
		if (iwc == null) {
			return false;
		}

		Object o = iwc.getSessionAttribute(ContentConstants.DELETED_PAGE_IN_LUCID_PROPERTIES_FOR_ARTICLE);
		if (o instanceof List) {
			@SuppressWarnings("rawtypes")
			List ids = (List) o;

			iwc.removeSessionAttribute(ContentConstants.DELETED_PAGE_IN_LUCID_PROPERTIES_FOR_ARTICLE);

			return deleteArticlesInPagesBeingDeleted(ids, getBuilderService());
		}

		return false;
	}

	private boolean deleteArticlesInPagesBeingDeleted(List<String> ids, BuilderService builder) {
		if (ids == null || builder == null) {
			return false;
		}

		for (int i = 0; i < ids.size(); i++) {
			deleteArticlesInThisPage(ids.get(i), builder);
		}

		return true;
	}

	private boolean deleteArticlesInThisPage(String pageKey, BuilderService builder) {
		if (pageKey == null) {
			return false;
		}

		if (builder == null) {
			return false;
		}

		Class<?> articleClass = CoreConstants.getArticleItemViewerClass();
		if (articleClass == null) {
			return false;
		}

		List<String> ids = builder.getModuleId(pageKey, articleClass.getName());
		if (ids == null) {
			return true;
		}

		List<String> paths = new ArrayList<String>();
		String path = null;
		for (int i = 0; i < ids.size(); i++) {
			path = builder.getProperty(pageKey, ids.get(i), CoreConstants.ARTICLE_RESOURCE_PATH_PROPERTY_NAME);
			if (path != null) {
				paths.add(path);
			}
		}

		if (paths.size() == 0) {
			return true;
		}

		ContentItemChecker checker = ELUtil.getInstance().getBean(ContentItemChecker.class);
		if (checker == null) {
			return false;
		}
		return checker.deleteDummyArticles(paths);
	}

	public boolean deleteArticle(String resourcePath) {
		if (resourcePath == null) {
			return false;
		}

		IWContext iwc = getThemesEngine().getContextAndCheckRights();
		if (iwc == null) {
			return false;
		}

		ContentItemChecker checker = ELUtil.getInstance().getBean(ContentItemChecker.class);
		if (checker == null) {
			return false;
		}
		return checker.deleteContentItem(resourcePath, iwc.getCurrentLocale());
	}

	public Document getRenderedPageInfo(String pageKey, String id, String styleClass) {
		PageInfo pageInfo = new PageInfo();
		pageInfo.setPageKey(pageKey);
		pageInfo.setStyleClass(styleClass);
		pageInfo.setId(id);

		return getThemesHelper().getThemesService().getBuilderService().getRenderedComponent(getThemesEngine().getContextAndCheckRights(),
				pageInfo, false);
	}

	public Document getReRenderedSiteInfo(String id, String styleClass) {
		SiteInfo siteInfo = new SiteInfo();
		siteInfo.setId(id);
		siteInfo.setStyleClass(styleClass);

		return getThemesHelper().getThemesService().getBuilderService().getRenderedComponent(getThemesEngine().getContextAndCheckRights(),
				siteInfo, false);
	}

	public List<PageAccessibilityProperty> getPageAccessibilityProperties(String pageKey) {
		if (pageKey == null) {
			return null;
		}

		ICPage page = null;
		try {
			page = getThemesHelper().getThemesService().getICPage(pageKey);
		} catch(Exception e) {
			e.printStackTrace();
		}
		if (page == null) {
			return null;
		}

		IWContext iwc = getThemesEngine().getContextAndCheckRights();
		if (iwc == null) {
			return null;
		}
		IWResourceBundle iwrb = iwc.getIWMainApplication().getBundle(ContentConstants.IW_BUNDLE_IDENTIFIER).getResourceBundle(iwc);

		List<PageAccessibilityProperty> properties = new ArrayList<PageAccessibilityProperty>();

		boolean published = page.isPublished();
		String localization = published ? iwrb.getLocalizedString("unpublish_page", "Unpublish page") : iwrb.getLocalizedString("publish_page", "Publish page");
		PageAccessibilityProperty property = new PageAccessibilityProperty(String.valueOf(!published), localization);
		property.setCode(ContentConstants.PUBLISH_PAGE_IN_LUCID_CODE);
		property.setColumnName(ICPageBMPBean.PAGE_IS_PUBLISHED);
		property.setElementId("publishPageButtonCtxMn");
		properties.add(property);

		boolean locked = page.isLocked();
		localization = locked ? iwrb.getLocalizedString("unlock_page", "Unclock page") : iwrb.getLocalizedString("lock_page", "Lock page");
		property = new PageAccessibilityProperty(String.valueOf(!locked), localization);
		property.setCode(ContentConstants.SET_PAGE_LOCKED_IN_LUCID_CODE);
		property.setColumnName(ICPageBMPBean.PAGE_IS_LOCKED);
		property.setElementId("lockPageButtonCtxMn");
		properties.add(property);

		boolean hidden = page.isHidePageInMenu();
		localization = hidden ? iwrb.getLocalizedString("show_page_in_menu", "Show page in menu") : iwrb.getLocalizedString("hide_page_in_menu", "Hide page in menu");
		property = new PageAccessibilityProperty(String.valueOf(!hidden), localization);
		property.setCode(ContentConstants.HIDE_MENU_IN_PAGE);
		property.setColumnName(ICPageBMPBean.HIDE_PAGE_IN_MENU);
		property.setElementId("hidePageButtonCtxMn");
		properties.add(property);

		return properties;
	}

	public boolean setPageAccessibilityProperty(String pageKey, String code, String value, String columnName) {
		if (pageKey == null || code == null || columnName == null) {
			return false;
		}

		IWContext iwc = getThemesEngine().getContextAndCheckRights();
		if (iwc == null) {
			return false;
		}

		if (ContentConstants.SET_PAGE_LOCKED_IN_LUCID_CODE.equals(code)) {
			if (!setPageAvailability(iwc, pageKey, value)) {
				return false;
			}
		}

		return setValueForPage(pageKey, value, columnName);
	}

	private String getPathToImagesFolder() {
		if (pathToImagesFolder == null) {
			pathToImagesFolder = ContentUtil.getBundle().getResourcesPath() + "/images/";
		}
		return pathToImagesFolder;
	}

	private boolean setPageTitle(String pageID, String title) {
		if (pageID == null || title == null) {
			return false;
		}
		if (ThemesConstants.MINUS_ONE.equals(pageID)) {
			return false;
		}

		IWContext iwc = getThemesEngine().getContextAndCheckRights();
		if (iwc == null) {
			return false;
		}

		IWMainApplication appl = iwc.getIWMainApplication();
		if (appl == null) {
			return false;
		}

		String method = ":method:1:implied:void:setTitle:java.lang.String:";
		return getThemesHelper().getThemesService().getBuilderService().setProperty(pageID, ThemesConstants.MINUS_ONE, method, new String[]{title},
				appl);
	}

	@SuppressWarnings("unchecked")
	public String changePageUriAfterPageWasMoved(String pageKey) {
		if (pageKey == null) {
			return null;
		}
		if (ThemesConstants.MINUS_ONE.equals(pageKey)) {
			return null;
		}

		ICPage page = getThemesHelper().getThemesService().getICPage(pageKey);
		if (page == null) {
			return null;
		}

		String newUri = changePageUri(pageKey, page.getName(), false);
		if (newUri == null) {
			return null;
		}

		Collection<ICPage> children = page.getChildren();
		if (children != null) {
			for (Iterator<ICPage> it = children.iterator(); it.hasNext();) {
				return changePageUriAfterPageWasMoved(it.next().getId());
			}
		}

		return newUri;
	}

	private String setPageUri(String pageKey, String uri) {
		if (pageKey == null || uri == null) {
			return null;
		}
		if (ThemesConstants.MINUS_ONE.equals(pageKey)) {
			return null;
		}

		ICPage page = getThemesHelper().getThemesService().getICPage(pageKey);
		if (page == null) {
			return null;
		}
		if (uri.equals(page.getDefaultPageURI())) {
			return null;
		}

		ICDomain domain = null;
		IWContext iwc = getThemesEngine().getContextAndCheckRights();
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

		BuilderService builder = getBuilderService();
		if (builder.setPageUri(page, uri, domain.getID())) {
			setNewLinkInArticleFile(page.getId(), CoreConstants.getArticleItemViewerClass().getName(), page.getDefaultPageURI());
			return page.getDefaultPageURI();
		}
		return null;
	}

	private boolean setPageAvailability(IWContext iwc, String pageKey, String availability) {
		if (pageKey == null || availability == null) {
			return false;
		}

		if (availability.equals(CoreConstants.EMPTY) || availability.equalsIgnoreCase("null") || availability.equals(ThemesConstants.MINUS_ONE)) {
			return false;
		}

		boolean restrictedAccess = Boolean.TRUE.toString().equalsIgnoreCase(availability);
		String usersGroupId = String.valueOf(AccessControl._GROUP_ID_USERS);
		String everyOneGroupId = String.valueOf(AccessControl._GROUP_ID_EVERYONE);
		try {
			iwc.getAccessController().setPermission(AccessController.CATEGORY_PAGE_INSTANCE, iwc, usersGroupId, pageKey, AccessController.PERMISSION_KEY_VIEW,
					Boolean.TRUE);
			iwc.getAccessController().setPermission(AccessController.CATEGORY_PAGE_INSTANCE, iwc, everyOneGroupId, pageKey, AccessController.PERMISSION_KEY_VIEW,
					!restrictedAccess);
		} catch (Exception e) {
			e.printStackTrace();
			return false;
		}

		ICPage page = getThemesHelper().getThemesService().getICPage(pageKey);
		if (page == null) {
			return false;
		}
		@SuppressWarnings("rawtypes")
		Collection children = page.getChildren();
		if (children != null) {
			Object o = null;
			for (@SuppressWarnings("rawtypes")
			Iterator it = children.iterator(); it.hasNext();) {
				o = it.next();
				if (o instanceof ICTreeNode) {
					setPageAvailability(iwc, ((ICTreeNode) o).getId(), availability);
				}
			}
		}

		return true;
	}

	private boolean setValueForPage(String pageKey, String value, String columnName) {
		return setValueForPage(pageKey, value, columnName, true);
	}

	@SuppressWarnings("deprecation")
	private boolean setValueForPage(String pageKey, String value, String columnName, boolean setSameValueForChildren) {
		if (pageKey == null || value == null) {
			return false;
		}

		if (value.equals(CoreConstants.EMPTY) || value.equalsIgnoreCase("null") || value.equals(ThemesConstants.MINUS_ONE)) {
			return false;
		}

		ICPage page = getThemesHelper().getThemesService().getICPage(pageKey);
		if (page == null) {
			return false;
		}

		boolean pageValue = false;
		if (value != null) {
			pageValue = Boolean.TRUE.toString().equalsIgnoreCase(value);
		}

		page.setColumn(columnName, pageValue);
		page.store();

		if (setSameValueForChildren) {
			@SuppressWarnings("rawtypes")
			Collection children = page.getChildren();
			if (children != null) {
				Object o = null;
				for (@SuppressWarnings("rawtypes")
				Iterator it = children.iterator(); it.hasNext();) {
					o = it.next();
					if (o instanceof ICTreeNode) {
						setValueForPage(((ICTreeNode) o).getId(), value, columnName, setSameValueForChildren);
					}
				}
			}
		}

		return true;
	}

	private BuilderService getBuilderService() {
		return getBuilderLogic().getBuilderService(IWMainApplication.getDefaultIWApplicationContext());
	}

	public LucidApplicationInfo getStartInfo(Boolean fullInfo) {
		LucidApplicationInfo info = new LucidApplicationInfo();
		IWContext iwc = getThemesEngine().getContextAndCheckRights();
		if (iwc == null) {
			LOGGER.warning("Current user has no right for Lucid application!");
			return info;
		}

		info.setLocalizedTexts(getLocalizedText(iwc));

		if (fullInfo != null && fullInfo) {
			info.setPageId(getPageId());
			info.setClassNameForSourceView(getBuilderService().getClassNameForSourceView());
			info.setPathToImageFolder(getPathToImageFolder());
			info.setPageUri(getPageUri(info.getPageId()));

			info.setContentEditor(isContentEditor());
			info.setStartPage(isStartPage(info.getPageId()));
			info.setCanActAsBuilderUser(canUserActAsBuilderUser());

			info.setPageInfoElements(getPageInfoElements());

			startBuilderApplication(iwc);
		}
		else {
			info.setMooRainbowImage(new StringBuilder(iwc.getIWMainApplication().getBundle(Web2BusinessBean.WEB2_BUNDLE_IDENTIFIER).getResourcesPath())
										.append("/javascript/moorainbow/1.1/images/").toString());
		}

		return info;
	}

	public JQuery getJQuery() {
		return jQuery;
	}

	public void setJQuery(JQuery query) {
		jQuery = query;
	}

	public ThemesHelper getThemesHelper() {
		return themesHelper;
	}

	public void setThemesHelper(ThemesHelper themesHelper) {
		this.themesHelper = themesHelper;
	}

	public ThemesEngine getThemesEngine() {
		return themesEngine;
	}

	public void setThemesEngine(ThemesEngine themesEngine) {
		this.themesEngine = themesEngine;
	}

}