package com.idega.content.themes.business;

import java.rmi.RemoteException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import javax.ejb.FinderException;

import org.apache.slide.event.ContentEvent;

import com.idega.business.IBOServiceBean;
import com.idega.content.business.ContentConstants;
import com.idega.content.themes.helpers.bean.Theme;
import com.idega.content.themes.helpers.business.ThemesConstants;
import com.idega.content.themes.helpers.business.ThemesHelper;
import com.idega.core.accesscontrol.business.StandardRoles;
import com.idega.core.builder.business.BuilderService;
import com.idega.core.builder.business.BuilderServiceFactory;
import com.idega.core.builder.data.ICDomain;
import com.idega.core.builder.data.ICPage;
import com.idega.core.builder.data.ICPageHome;
import com.idega.idegaweb.IWApplicationContextFactory;
import com.idega.presentation.IWContext;
import com.idega.servlet.filter.IWWelcomeFilter;
import com.idega.slide.business.IWContentEvent;
import com.idega.slide.business.IWSlideChangeListener;
import com.idega.util.CoreConstants;
import com.idega.util.CoreUtil;
import com.idega.util.StringHandler;

public class ThemesServiceBean extends IBOServiceBean implements ThemesService, IWSlideChangeListener{

	private static final long serialVersionUID = -1765120426660957585L;
	
	private volatile BuilderService builder = null;

	public void onSlideChange(IWContentEvent idegaWebContentEvent) {
		String uri = idegaWebContentEvent.getContentEvent().getUri();
		if (uri.indexOf(ThemesConstants.THEMES_PATH) == -1) {	// If not processing theme
			return;
		}
		if (ContentEvent.REMOVE.equals(idegaWebContentEvent.getMethod())) {
			if (ThemesHelper.getInstance(false).isCorrectThemeTemplateFile(uri, ThemesConstants.THEME_SKELETONS_FILTER)) {
				Collection<Theme> themes = ThemesHelper.getInstance(false).getAllThemes();
				if (themes == null) {
					return;
				}
				boolean foundTheme = false;
				Theme theme = null;
				for (Iterator<Theme> it = themes.iterator(); (it.hasNext() && !foundTheme);) {
					theme = it.next();
					if (uri.equals(ThemesHelper.getInstance(false).decodeUrl(theme.getLinkToSkeleton()))) {
						foundTheme = true;
					}
				}
				if (foundTheme && !theme.isLocked()) {
					ThemesHelper.getInstance(false).removeLastUsedTheme(String.valueOf(theme.getIBPageID()));
					int pageId = theme.getIBPageID();
					
					String themeID = theme.getId();
					ThemesHelper.getInstance(false).removeTheme(uri, themeID);
					
					deleteTemplate(pageId);
				}
			}
		}
		else {
			if (!ThemesHelper.getInstance().isCreatedManually(uri) && ThemesHelper.getInstance().isCorrectThemeTemplateFile(uri, ThemesConstants.THEME_SKELETONS_FILTER) && isNewTheme(uri)) {
				try {
					ThemesHelper.getInstance().getThemesLoader().loadTheme(uri, ThemesHelper.getInstance().urlEncode(uri), true, false);
				} catch (Exception e) {
					e.printStackTrace();
				}
			}
		}
	}
	
	private boolean deleteTemplate(int pageId) {
		if (pageId < 0) {
			return false;
		}
		
		return deletePage(String.valueOf(pageId), true, true, false, false);
	}
	
	public boolean deleteIBPage(String pageID, boolean deleteChildren, boolean markPagesForDeletingArticles) {
		return deletePage(pageID, deleteChildren, false, true, markPagesForDeletingArticles);
	}
	
	@SuppressWarnings("unchecked")
	private boolean deletePage(String pageKey, boolean deleteChildren, boolean canUseDefaultUser, boolean clearCache, boolean markPagesForDeletingArticles) {
		if (pageKey == null) {
			return false;
		}

		IWContext iwc = CoreUtil.getIWContext();
		
		getBuilderService();
		
		Map tree = null;
		ICDomain domain = null;
		int userId = 1;
		
		if (iwc == null && canUseDefaultUser) {
			try {
				userId = Integer.valueOf(getAccessController().getAdministratorUser().getId());	//	Using default user
			} catch(Exception e) {
				e.printStackTrace();
			}
			try {
				tree = builder.getTree(getIWApplicationContext());
			} catch(Exception e) {
				e.printStackTrace();
			}
		}
		else {
			userId = iwc.getCurrentUserId();
			tree = builder.getTree(iwc);
			domain = iwc.getDomain();
			
			if (pageKey.equals(ThemesHelper.getInstance().getLastVisitedPage())) {
				ThemesHelper.getInstance().setLastVisitedPage(null);
			}
		}
		
		if (markPagesForDeletingArticles) {
			markPagesForDeletingArticles(pageKey, iwc);
		}
		boolean result = builder.deletePage(pageKey, deleteChildren, tree, userId, domain);
		log("IBPage (id=" + pageKey + ") was deleted successfully: " + result);
		
		if (domain != null) {
			try {
				if (Integer.valueOf(pageKey).intValue() == domain.getStartPageID()) {
					domain.setIBPage(null);
					domain.store();
				}
			} catch (NumberFormatException e) {
				e.printStackTrace();
			}
			
			if (clearCache) {
				builder.clearAllCachedPages();
			}
		}
		
		IWWelcomeFilter.reInitializeCachedDomainOnNextRequest();
		
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
	
	@SuppressWarnings("unchecked")
	private void putAllIdsOfPageAndChildren(String pageKey, List<String> ids) {
		ICPage page = getICPage(pageKey);
		
		if (page != null) {
			String id = page.getId();
			if (!ids.contains(id)) {
				ids.add(id);
			}
			
			Collection children = page.getChildren();
			if (children != null) {
				Object o = null;
				for (Iterator it = children.iterator(); it.hasNext();) {
					o = it.next();
					if (o instanceof ICPage) {
						putAllIdsOfPageAndChildren(((ICPage) o).getId(), ids);
					}
				}
			}
		}
	}
	
	public boolean createBuilderTemplate(Theme theme) {
		if (theme == null) {
			return false;
		}
		
		if (theme.getIBPageID() != -1) {
			return true;	//	IBPage (template) for this theme already exists
		}
		
		int id = -1;
		IWContext iwc = CoreUtil.getIWContext();
		if (iwc == null) {
			return false;
		}
		int domainID = -1;
		ICDomain domain = iwc.getDomain();
		if (domain == null) {
			return false;
		}
		domainID = domain.getID();
		
		getBuilderService();
		
		//	Creating IBPage (template) for theme
		String parentId = builder.getTopLevelTemplateId(builder.getTopLevelTemplates(iwc));
		if (parentId == null || ThemesConstants.MINUS_ONE.equals(parentId)) {
			//	No Top Level Template
			parentId = ThemesHelper.getInstance().getThemesEngine().createRootTemplate(domain, builder, domainID, builder.getIBXMLFormat());
			ThemesHelper.getInstance().getThemesEngine().initializeCachedDomain(ThemesConstants.DEFAULT_DOMAIN_NAME, domain);
		}
		String name = StringHandler.removeCharacters(theme.getName(), ContentConstants.SPACE, ContentConstants.UNDER);
		id = createIBPage(parentId, theme.getName(), builder.getTemplateKey(), null, ThemesConstants.THEMES + name +
				ContentConstants.SLASH, null, domainID, builder.getHTMLTemplateKey(), null);
		if (id == -1) {
			return false;
		}
		theme.setIBPageID(id);
		
		if (updatePageWebDav(theme.getIBPageID(), CoreConstants.WEBDAV_SERVLET_URI + theme.getLinkToSkeleton())) {
			ThemesHelper.getInstance().getThemesEngine().updateSiteTemplatesTree(iwc, true);
			return true;
		}
		
		return false;
	}
	
	public boolean updatePageWebDav(int id, String uri) {
		return updatePageWebDav(id, uri, true);
	}
	
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
	
	public int createIBPage(String parentId, String name, String type, String templateId, String pageUri, String subType, int domainId, String format, String sourceMarkup){
		return createIBPage(parentId, name, type, templateId, pageUri, subType, domainId, format, sourceMarkup, null);
	}
	
	@SuppressWarnings("unchecked")
	public int createIBPage(String parentId, String name, String type, String templateId, String pageUri, String subType, int domainId, String format, String sourceMarkup, String treeOrder) {
		IWContext iwc = CoreUtil.getIWContext();
		if (iwc == null) {
			return -1;
		}
		
		getBuilderService();
		
		Map tree = builder.getTree(iwc);
		if (tree == null) {
			return -1;
		}
		
		if (builder.getPageKey().equals(type)) {
			if (templateId == null) {
				templateId = ThemesHelper.getInstance().getLastUsedTheme();
			}
		}
		
		if (parentId == null && domainId == -1) { // Creating top level page
			ICDomain domain = null;
			domain = iwc.getDomain();
			if (domain != null) {
				domainId = domain.getID();
			}
		}
		
		int pageId = builder.createNewPage(parentId, name, type, templateId, pageUri, tree, iwc, subType, domainId, format, sourceMarkup, treeOrder);
		
		if (iwc.hasRole(StandardRoles.ROLE_KEY_EDITOR)) {
			ICPage createdPage = getICPage(pageId);
			if (createdPage != null) {
				createdPage.setPublished(true);
				createdPage.store();
			}
		}
		
		return pageId;
	}
	
	private boolean isNewTheme(String uri) {
		if (ThemesHelper.getInstance().existTheme(uri)) {
			return false;
		}
		return true;
	}
	
	public ICPageHome getICPageHome() throws RemoteException {
		ICPageHome sHome = (ICPageHome) getIDOHome(ICPage.class);
		return sHome;
	}
	
	public BuilderService getBuilderService() {
		if (builder == null) {
			synchronized (ThemesServiceBean.class) {
				if (builder == null) {
					try {
						builder = BuilderServiceFactory.getBuilderService(getIWApplicationContext());
					} catch (RemoteException e) {
						e.printStackTrace();
					}
				}
			}
		}
		return builder;
	}
	
	public ICPage getICPage(String pageKey) {
		if (pageKey == null) {
			return null;
		}
		int id = -1;
		try {
			id = Integer.valueOf(pageKey);
		} catch (NumberFormatException e) {
			e.printStackTrace();
			return null;
		}
		return getICPage(id);
	}
	
	public ICPage getICPage(int id) {
		ICPage page = null;
		try {
			page = getICPageHome().findByPrimaryKey(id);
		} catch (RemoteException e) {
			e.printStackTrace();
			return null;
		} catch (FinderException e) {
			e.printStackTrace();
			return null;
		}
		return page;
	}
	
	public ICDomain getDomain() {
		/*ICDomainHome domainHome = null;
		try {
			domainHome = (ICDomainHome) IDOLookup.getHome(ICDomain.class);
		} catch (IDOLookupException e) {
			log.error(e);
			return null;
		}
		try {
			return domainHome.findFirstDomain();
		} catch (FinderException e) {
			log.error(e);
			return null;
		}*/
		return IWApplicationContextFactory.getCurrentIWApplicationContext().getDomain();
	}
	
	public String createChildTemplateForThisTemplate(String parentTemplateKey) {
		if (parentTemplateKey == null) {
			return null;
		}
		
		BuilderService builder = getBuilderService();
		if (builder == null) {
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
			templateId = createIBPage(parentTemplateKey, name, builder.getTemplateKey(), parentTemplateKey, null, null, domain.getID(), builder.getIBXMLFormat(), null, null);
		} catch (Exception e) {
			e.printStackTrace();
		}
		
		if (templateId == -1) {
			return null;
		}
		
		return String.valueOf(templateId);
	}
}