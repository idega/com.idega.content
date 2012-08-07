package com.idega.content.themes.business;

import java.rmi.RemoteException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.ejb.FinderException;
import javax.jcr.observation.Event;
import javax.jcr.observation.EventIterator;

import org.springframework.beans.factory.annotation.Autowired;

import com.idega.builder.business.BuilderLogicWrapper;
import com.idega.business.IBOServiceBean;
import com.idega.content.business.ContentConstants;
import com.idega.content.lucid.business.LucidEngine;
import com.idega.content.themes.helpers.bean.Theme;
import com.idega.content.themes.helpers.business.ThemesConstants;
import com.idega.content.themes.helpers.business.ThemesHelper;
import com.idega.content.themes.helpers.business.ThemesLoader;
import com.idega.core.accesscontrol.business.StandardRoles;
import com.idega.core.builder.business.BuilderService;
import com.idega.core.builder.data.ICDomain;
import com.idega.core.builder.data.ICPage;
import com.idega.core.builder.data.ICPageHome;
import com.idega.idegaweb.IWApplicationContext;
import com.idega.idegaweb.IWApplicationContextFactory;
import com.idega.idegaweb.IWMainApplication;
import com.idega.presentation.IWContext;
import com.idega.servlet.filter.BaseFilter;
import com.idega.util.CoreConstants;
import com.idega.util.CoreUtil;
import com.idega.util.StringUtil;
import com.idega.util.expression.ELUtil;

public class ThemesServiceBean extends IBOServiceBean implements ThemesService {

	private static final long serialVersionUID = -1765120426660957585L;

	private static final Logger LOGGER = Logger.getLogger(ThemesServiceBean.class.getName());

	@Autowired
	private BuilderLogicWrapper builderLogicWrapper;

	@Autowired
	private ThemesHelper themesHelper;

	@Autowired
	private ThemesEngine themesEngine;

	private boolean deleteTemplate(int pageId) {
		if (pageId < 0) {
			return false;
		}

		return deletePage(String.valueOf(pageId), true, true, false, false);
	}

	@Override
	public boolean deleteIBPage(String pageID, boolean deleteChildren, boolean markPagesForDeletingArticles) {
		return deletePage(pageID, deleteChildren, false, true, markPagesForDeletingArticles);
	}

	private boolean deletePage(String pageKey, boolean deleteChildren, boolean canUseDefaultUser, boolean clearCache, boolean markPagesForDeletingArticles) {
		if (pageKey == null) {
			return false;
		}

		IWContext iwc = CoreUtil.getIWContext();

		@SuppressWarnings("rawtypes")
		Map tree = null;
		ICDomain domain = null;
		int userId = 1;

		if (iwc == null && canUseDefaultUser) {
			try {
				userId = Integer.valueOf(getAccessController().getAdministratorUser().getId());	//	Using default user
			} catch(Exception e) {
				LOGGER.log(Level.WARNING, "Error getting user id", e);
			}
			try {
				tree = getBuilderService().getTree(getIWApplicationContext());
			} catch(Exception e) {
				LOGGER.log(Level.WARNING, "Error getting tree", e);
			}
		} else {
			userId = iwc.getCurrentUserId();
			tree = getBuilderService().getTree(iwc);
			domain = iwc.getDomain();

			if (pageKey.equals(getThemesHelper().getLastVisitedPage())) {
				getThemesHelper().setLastVisitedPage(null);
			}
		}

		if (markPagesForDeletingArticles) {
			markPagesForDeletingArticles(pageKey, iwc);
		}
		boolean result = getBuilderService().deletePage(pageKey, deleteChildren, tree, userId, domain);
		LOGGER.info("IBPage (id=" + pageKey + ") was deleted successfully: " + result);

		if (domain != null) {
			try {
				if (Integer.valueOf(pageKey).intValue() == domain.getStartPageID()) {
					domain.setIBPage(null);
					domain.store();
				}
			} catch (NumberFormatException e) {
				LOGGER.log(Level.WARNING, "Error converting to number: " + pageKey, e);
			}

			if (clearCache) {
				getBuilderService().clearAllCachedPages();
			}
		}

		BaseFilter.reInitializeCachedDomainOnNextRequest();

		return result;
	}

	private void markPagesForDeletingArticles(String pageKey, IWContext iwc) {
		if (pageKey == null || iwc == null) {
			return;
		}

		List<String> ids = new ArrayList<String>();
		putAllIdsOfPageAndChildren(pageKey, ids);

		iwc.setSessionAttribute(ContentConstants.DELETED_PAGE_IN_LUCID_PROPERTIES_FOR_ARTICLE, ids);
	}

	private void putAllIdsOfPageAndChildren(String pageKey, List<String> ids) {
		ICPage page = getICPage(pageKey);

		if (page != null) {
			String id = page.getId();
			if (!ids.contains(id)) {
				ids.add(id);
			}

			@SuppressWarnings("rawtypes")
			Collection children = page.getChildren();
			if (children != null) {
				Object o = null;
				for (@SuppressWarnings("rawtypes")
				Iterator it = children.iterator(); it.hasNext();) {
					o = it.next();
					if (o instanceof ICPage) {
						putAllIdsOfPageAndChildren(((ICPage) o).getId(), ids);
					}
				}
			}
		}
	}

	@Override
	public boolean createBuilderTemplate(Theme theme) {
		if (theme == null) {
			return false;
		}

		if (theme.getIBPageID() != -1) {
			return true;	//	IBPage (template) for this theme already exists
		}

		int id = -1;
		IWContext iwc = CoreUtil.getIWContext();
		IWApplicationContext iwac = iwc == null ? IWMainApplication.getDefaultIWApplicationContext() : iwc;

		LucidEngine lucidEngine = ELUtil.getInstance().getBean(LucidEngine.SPRING_BEAN_IDENTIFIER);
		if (lucidEngine == null) {
			return false;
		}

		int domainID = -1;
		ICDomain domain = iwac.getDomain();
		if (domain == null) {
			return false;
		}
		domainID = domain.getID();

		String webDavUri = CoreConstants.WEBDAV_SERVLET_URI.concat(theme.getLinkToSkeleton());
		if (existsTheme(theme, webDavUri, domainID)) {
			return true;
		}

		//	Creating IBPage (template) for theme
		String parentId = getBuilderService().getTopLevelTemplateId(getBuilderService().getTopLevelTemplates(iwac));
		if (parentId == null || ThemesConstants.MINUS_ONE.equals(parentId)) {
			//	No Top Level Template
			parentId = lucidEngine.createRootTemplate(domain, getBuilderService(), domainID, getBuilderService().getIBXMLFormat());
			lucidEngine.initializeCachedDomain(ThemesConstants.DEFAULT_DOMAIN_NAME, domain);
		}

		String name = getThemesHelper().getPreparedThemeNameToUseInRepository(theme);
		String suffix = getSuffixForTemplate(theme.getName());
		String templateName = suffix == null ? theme.getName() : new StringBuilder(theme.getName()).append(suffix).toString();
		String uri = getUriForTemplate(new StringBuilder(ThemesConstants.THEMES).append(name).append(suffix == null ? CoreConstants.EMPTY : suffix).toString(),
				domainID);
		if (uri == null) {
			return false;
		}
		if (!uri.endsWith(CoreConstants.SLASH)) {
			uri = uri.concat(CoreConstants.SLASH);
		}

		id = createIBPage(parentId, templateName, getBuilderService().getTemplateKey(), null, uri, null, domainID, getBuilderService().getHTMLTemplateKey(), null);
		if (id == -1) {
			return false;
		}
		theme.setIBPageID(id);

		if (updatePageWebDav(theme.getIBPageID(), webDavUri)) {
			ThemesEngine themesEngine = getThemesEngine();
			if (themesEngine != null) {
				themesEngine.updateSiteTemplatesTree(true);
			}

			return true;
		}

		return false;
	}

	private String getUriForTemplate(String initialValue, int domainId) {
		try {
			ICPageHome pageHome = getICPageHome();
			ICPage template = pageHome.findByUri(initialValue.concat(CoreConstants.SLASH), domainId);
			if (template == null) {
				return initialValue;
			}

			LOGGER.warning("Trying to create template with already existing uri: " + initialValue + ", will change it!");
			initialValue = initialValue.concat(CoreConstants.UNDER).concat(String.valueOf(System.currentTimeMillis()));
			return getUriForTemplate(initialValue, domainId);
		} catch (FinderException e) {
			return initialValue;
		} catch (Exception e) {
			LOGGER.log(Level.WARNING, "Error getting template by the uri: " + initialValue.concat(CoreConstants.SLASH), e);
		}
		return null;
	}

	private boolean existsTheme(Theme theme, String webDavUri, int domainId) {
		if (StringUtil.isEmpty(webDavUri)) {
			return false;
		}

		try {
			ICPage icpage = getICPageHome().findByWebDavUri(webDavUri);
			if (icpage == null) {
				return false;
			}

			boolean exists = !icpage.getDeleted();
			if (exists) {
				LOGGER.info("Template with WebDavUri '' already exists! Not creating new one.");
				theme.setIBPageID(Integer.valueOf(icpage.getPrimaryKey().toString()));
			}
			return exists;
		} catch (FinderException e) {
		} catch (Exception e) {
			LOGGER.log(Level.WARNING, "Error while checking if template exists by uri: " + webDavUri, e);
		}
		return false;
	}
	private String getSuffixForTemplate(String name) {
		int counter = 0;

		Collection<ICPage> templates = null;
		try {
			templates = getICPageHome().findAllByName(name, true);
		} catch (Exception e) {
			LOGGER.log(Level.WARNING, "Error getting templates", e);
			return null;
		}

		String queryName = null;
		while (templates != null && !templates.isEmpty()) {
			counter++;
			queryName = new StringBuilder(name).append(CoreConstants.UNDER).append(counter).toString();

			try {
				templates = getICPageHome().findAllByName(queryName, true);
			} catch (Exception e) {
				LOGGER.log(Level.WARNING, "Error getting template", e);
				return null;
			}
		}

		return counter == 0 ? null : new StringBuilder(CoreConstants.UNDER).append(counter).toString();
	}

	@Override
	public boolean updatePageWebDav(int id, String uri) {
		return updatePageWebDav(id, uri, true);
	}

	@Override
	public boolean updatePageWebDav(int id, String uri, boolean clearCache) {
		ICPage page = getICPage(id);
		if (page == null) {
			return false;
		}

		page.setWebDavUri(uri);
		page.store();

		if (clearCache) {
			getBuilderService().clearAllCachedPages();
		}
		return true;
	}

	@Override
	public int createIBPage(String parentId, String name, String type, String templateId, String pageUri, String subType, int domainId, String format,
			String sourceMarkup) {
		return createIBPage(parentId, name, type, templateId, pageUri, subType, domainId, format, sourceMarkup, null);
	}

	@Override
	public int createIBPage(String parentId, String name, String type, String templateId, String pageUri, String subType, int domainId, String format,
			String sourceMarkup, String treeOrder) {

		IWContext iwc = CoreUtil.getIWContext();
		IWApplicationContext iwac = iwc == null ? IWMainApplication.getDefaultIWApplicationContext() : iwc;

		@SuppressWarnings("rawtypes")
		Map tree = getBuilderService().getTree(iwac);
		if (tree == null) {
			return -1;
		}

		if (getBuilderService().getPageKey().equals(type)) {
			if (templateId == null) {
				templateId = getThemesHelper().getLastUsedTheme();
			}
		}

		if (parentId == null && domainId == -1) { // Creating top level page
			ICDomain domain = null;
			domain = iwac.getDomain();
			if (domain != null) {
				domainId = domain.getID();
			}
		}

		int pageId = getBuilderService().createNewPage(parentId, name, type, templateId, pageUri, tree, iwc, subType, domainId, format, sourceMarkup, treeOrder);
		if (pageId < 0) {
			LOGGER.warning("Probably page was not created using params:\n" +
					"parent ID: " + parentId + "\n" +
					"name: " + name + "\n" +
					"type: " + type + "\n" +
					"template ID: " + templateId + "\n" +
					"page URI: " + pageUri + "\n" +
					"tree: " + tree + "\n" +
					"sub type: " + subType + "\n" +
					"domain ID" + domainId + "\n" +
					"format: " + format + "\n" +
					"source markup: " + sourceMarkup + "\n" +
					"tree order: "+ treeOrder + "\n" +
				"\nbecause returned ID is: " + pageId);
		}

		if (iwc != null && iwc.hasRole(StandardRoles.ROLE_KEY_EDITOR)) {
			ICPage createdPage = getICPage(pageId);
			if (createdPage != null) {
				createdPage.setPublished(true);
				createdPage.store();
			}
		}

		return pageId;
	}

	private boolean isNewTheme(String uri) {
		if (getThemesHelper().existTheme(uri)) {
			return false;
		}
		return true;
	}

	@Override
	public ICPageHome getICPageHome() throws RemoteException {
		ICPageHome sHome = (ICPageHome) getIDOHome(ICPage.class);
		return sHome;
	}

	@Override
	public BuilderService getBuilderService() {
		if (builderLogicWrapper == null) {
			ELUtil.getInstance().autowire(this);
		}
		return builderLogicWrapper.getBuilderService(getIWApplicationContext());
	}

	@Override
	public ICPage getICPage(String pageKey) {
		if (pageKey == null) {
			return null;
		}
		int id = -1;
		try {
			id = Integer.valueOf(pageKey);
		} catch (NumberFormatException e) {
			LOGGER.log(Level.WARNING, "Error converting number", e);
			return null;
		}
		return getICPage(id);
	}

	@Override
	public ICPage getICPage(int id) {
		ICPage page = null;
		try {
			page = getICPageHome().findByPrimaryKey(id);
		} catch (FinderException e) {
			LOGGER.warning("Page by ID=" + id + " does not exist!");
		} catch (Exception e) {
			LOGGER.log(Level.WARNING, "Error getting page by id: " + id, e);
		}
		return page;
	}

	@Override
	public ICDomain getDomain() {
		return IWApplicationContextFactory.getCurrentIWApplicationContext().getDomain();
	}

	@Override
	public String createChildTemplateForThisTemplate(String parentTemplateKey) {
		if (parentTemplateKey == null) {
			return null;
		}

		ICPage currentTemplate = getICPage(parentTemplateKey);
		if (currentTemplate == null) {
			return null;
		}
		ICDomain domain = getDomain();
		if (domain == null) {
			return null;
		}

		String childTemplate = "Child Template";
		String name = currentTemplate.getName();
		if (name == null) {
			name = childTemplate;
		}
		else {
			name = new StringBuffer(name).append(CoreConstants.SPACE).append(childTemplate).toString();
		}

		int templateId = -1;
		try {
			templateId = createIBPage(parentTemplateKey, name, getBuilderService().getTemplateKey(), parentTemplateKey, null, null, domain.getID(),
					getBuilderService().getIBXMLFormat(), null, null);
		} catch (Exception e) {
			LOGGER.log(Level.WARNING, "Error creating template: " + name, e);
		}

		if (templateId == -1) {
			return null;
		}

		return String.valueOf(templateId);
	}

	public ThemesHelper getThemesHelper() {
		if (themesHelper == null) {
			ELUtil.getInstance().autowire(this);
		}
		return themesHelper;
	}

	public void setThemesHelper(ThemesHelper themesHelper) {
		this.themesHelper = themesHelper;
	}

	public ThemesEngine getThemesEngine() {
		if (themesEngine == null) {
			ELUtil.getInstance().autowire(this);
		}
		return themesEngine;
	}

	public void setThemesEngine(ThemesEngine themesEngine) {
		this.themesEngine = themesEngine;
	}

	@Override
	public void onEvent(EventIterator events) {
		for (; events.hasNext();) {
			onSlideChange(events.nextEvent());
		}
	}

	@Override
	public void onSlideChange(Event event) {
		String uri = null;
		try {
			uri = event.getPath();
		} catch (Exception e) {
			LOGGER.log(Level.WARNING, "Error resolving JCR event path", e);
		}
		if (StringUtil.isEmpty(uri))
			return;

		if (uri.indexOf(ThemesConstants.THEMES_PATH) == -1)
			return;	// If not processing theme

		if (Event.NODE_REMOVED == event.getType()) {
			if (getThemesHelper().isCorrectThemeTemplateFile(uri, ThemesConstants.THEME_SKELETONS_FILTER)) {
				Collection<Theme> themes = getThemesHelper().getAllThemes();
				if (themes == null) {
					return;
				}
				boolean foundTheme = false;
				Theme theme = null;
				for (Iterator<Theme> it = themes.iterator(); (it.hasNext() && !foundTheme);) {
					theme = it.next();
					if (uri.equals(getThemesHelper().decodeUrl(theme.getLinkToSkeleton()))) {
						foundTheme = true;
					}
				}
				if (foundTheme && !theme.isLocked()) {
					getThemesHelper().removeLastUsedTheme(String.valueOf(theme.getIBPageID()));
					int pageId = theme.getIBPageID();

					String themeID = theme.getId();
					getThemesHelper().removeTheme(uri, themeID);

					deleteTemplate(pageId);
				}
			}
		} else if (Event.NODE_ADDED == event.getType()) {
			if (!getThemesHelper().isCreatedManually(uri) && getThemesHelper().isCorrectThemeTemplateFile(uri,
					ThemesConstants.THEME_SKELETONS_FILTER) && isNewTheme(uri)) {
				try {
					ThemesLoader loader = new ThemesLoader();
					loader.loadTheme(uri, getThemesHelper().urlEncode(uri), true, false);
				} catch (Exception e) {
					LOGGER.log(Level.WARNING, "Error loading theme: " + uri, e);
				}
			}
			if (uri.endsWith(ThemesConstants.THEME_PREDEFINED_STYLE_CONFIG_FILE)) {
				getThemesHelper().addPredefinedThemeStyle(uri);
			}
		}
	}

	@Override
	public String getPath() {
		return ThemesConstants.THEMES_PATH;
	}

	@Override
	public int getEventTypes() {
		return Event.NODE_ADDED | Event.NODE_REMOVED | Event.NODE_MOVED;
	}
}