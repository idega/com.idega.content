package com.idega.content.themes.business;

import java.rmi.RemoteException;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import javax.ejb.FinderException;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.slide.event.ContentEvent;

import com.idega.business.IBOServiceBean;
import com.idega.content.business.ContentConstants;
import com.idega.content.themes.helpers.Theme;
import com.idega.content.themes.helpers.ThemesConstants;
import com.idega.content.themes.helpers.ThemesHelper;
import com.idega.core.builder.business.BuilderService;
import com.idega.core.builder.business.BuilderServiceFactory;
import com.idega.core.builder.data.ICDomain;
import com.idega.core.builder.data.ICDomainHome;
import com.idega.core.builder.data.ICPage;
import com.idega.core.builder.data.ICPageHome;
import com.idega.data.IDOLookup;
import com.idega.data.IDOLookupException;
import com.idega.presentation.IWContext;
import com.idega.slide.business.IWContentEvent;
import com.idega.slide.business.IWSlideChangeListener;

public class ThemesServiceBean extends IBOServiceBean implements ThemesService, IWSlideChangeListener{

	private static final long serialVersionUID = -1765120426660957585L;
	private static final Log log = LogFactory.getLog(ThemesServiceBean.class);
	
	private volatile BuilderService builder = null;

	public void onSlideChange(IWContentEvent idegaWebContentEvent) {
		String uri = idegaWebContentEvent.getContentEvent().getUri();
		if (uri.indexOf(ThemesConstants.THEMES_PATH) == -1) { // If not proccesing theme
			return;
		}
		if (ContentEvent.REMOVE.equals(idegaWebContentEvent.getMethod())) {
			if (ThemesHelper.getInstance(false).isCorrectFile(uri)) {
				List <Theme> themes = new ArrayList<Theme>(ThemesHelper.getInstance(false).getThemesCollection());
				boolean foundTheme = false;
				Theme theme = null;
				for (int i = 0; (i < themes.size() && !foundTheme); i++) {
					theme = themes.get(i);
					if (uri.equals(ThemesHelper.getInstance(false).decodeUrl(theme.getLinkToSkeleton()))) {
						foundTheme = true;
					}
				}
				if (foundTheme && !theme.isLocked()) {
					ThemesHelper.getInstance(false).removeLastUsedTheme(String.valueOf(theme.getIBPageID()));
					deleteIBPage(theme);
					String themeID = theme.getId();
					ThemesHelper.getInstance(false).removeTheme(uri, themeID);
				}
			}
		}
		else {
			if (ThemesHelper.getInstance().isCorrectFile(uri) && isNewTheme(uri) && !ThemesHelper.getInstance().isCreatedManually(uri)) {
				ThemesHelper.getInstance().getThemesLoader().loadTheme(uri, ThemesHelper.getInstance().urlEncode(uri), true, false);
			}
		}
	}
	
	private boolean deleteIBPage(Theme theme) {
		if (theme == null) {
			return false;
		}
		if (theme.getIBPageID() == -1) {
			return true;
		}
		IWContext iwc = ThemesHelper.getInstance(false).getIWContext();
		int userID = -1;
		if (iwc != null) {
			userID = iwc.getCurrentUserId();
		}
		getBuilderService();
		ICDomain domain = null;
		try {
			domain = builder.getCurrentDomain();
		} catch (RemoteException e) {
			log.error(e);
			return false;
		}
		boolean result = false;
		result = builder.deletePage(String.valueOf(theme.getIBPageID()), false, null, userID, domain);
		if (result) {
			builder.clearAllCachedPages();
		}
		return result;
	}
	
	public boolean deleteIBPage(String pageID, boolean deleteChildren) {
		return deletePage(pageID, deleteChildren);
	}
	
	private boolean deletePage(String pageID, boolean deleteChildren) {
		if (pageID == null) {
			return false;
		}

		IWContext iwc = ThemesHelper.getInstance(false).getIWContext();
		
		if (iwc == null) {
			return false;
		}

		getBuilderService();

		Map tree = builder.getTree(iwc);

		ICDomain domain = null;
		try {
			domain = builder.getCurrentDomain();
		} catch (RemoteException e) {
			log.error(e);
			return false;
		}
		int domainId = domain.getID();
		
		if (pageID.equals(ThemesHelper.getInstance().getLastVisitedPage())) {
			ThemesHelper.getInstance().setLastVisitedPage(null);
		}
		
		boolean result = true;
		result =  builder.deletePage(pageID, deleteChildren, tree, iwc.getUserId(), domain);
		
		try {
			if (Integer.valueOf(pageID).intValue() == domain.getStartPageID()) {
				domain.setIBPage(null);
				domain.store();
			}
		} catch (NumberFormatException e) {
			log.error(e);
		}
		
		if (domainId != -1) {
			builder.clearAllCachedPages();
		}
		
		return result;
	}
	
	public boolean createIBPage(Theme theme) {
		if (theme == null) {
			return false;
		}
		int id = -1;
		IWContext iwc = ThemesHelper.getInstance(false).getIWContext();
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
		
		if (theme.getIBPageID() == -1) { // Creating IBPage for theme
			String parentId = builder.getTopLevelTemplateId(builder.getTopLevelTemplates(iwc));
			if (parentId == null || ThemesConstants.INCORRECT_PARENT_ID.equals(parentId)) { // No Top Level Template
				int topTemplate = ThemesHelper.getInstance().getThemesEngine().createRootTemplate(domain, builder, domainID, builder.getIBXMLFormat());
				ThemesHelper.getInstance().getThemesEngine().initializeCachedDomain(ThemesConstants.DEFAULT_DOMAIN_NAME, domain);
				parentId = String.valueOf(topTemplate);
			}
			String name = ThemesHelper.getInstance().removeSpaces(theme.getName());
			id = createIBPage(parentId, theme.getName(), builder.getTemplateKey(), null, ThemesConstants.THEMES + name +
					ContentConstants.SLASH, null, domainID, builder.getHTMLTemplateKey(), null);
			if (id == -1) {
				return false;
			}
			theme.setIBPageID(id);
		}
		
		return updatePageWebDav(theme.getIBPageID(), ContentConstants.CONTENT + theme.getLinkToSkeleton());
	}
	
	public boolean updatePageWebDav(int id, String uri) {
		ICPage page = getICPage(id);
		if (page == null) {
			return false;
		}
		
		page.setWebDavUri(uri);
		page.store();
		getBuilderService().clearAllCachedPages();
		return true;
	}
	public int createIBPage(String parentId, String name, String type, String templateId, String pageUri, String subType, int domainId, String format, String sourceMarkup){
		return createIBPage(parentId, name, type, templateId, pageUri, subType, domainId, format, sourceMarkup, null);
	}
	
	public int createIBPage(String parentId, String name, String type, String templateId, String pageUri, String subType, int domainId, String format, String sourceMarkup, String treeOrder) {
		IWContext iwc = ThemesHelper.getInstance(false).getIWContext();
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
			try {
				domain = builder.getCurrentDomain();
			} catch (RemoteException e) {
				log.error(e);
			}
			if (domain != null) {
				domainId = domain.getID();
			}
		}
		
		return builder.createNewPage(parentId, name, type, templateId, pageUri, tree, iwc, subType, domainId, format, sourceMarkup, treeOrder);
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
						log.error(e);
					}
				}
			}
		}
		return builder;
	}
	
	public ICPage getICPage(int id) {
		ICPage page = null;
		try {
			page = getICPageHome().findByPrimaryKey(id);
		} catch (RemoteException e) {
			log.error(e);
			return null;
		} catch (FinderException e) {
			log.error(e);
			return null;
		}
		return page;
	}
	
	public ICDomain getDomain() {
		ICDomainHome domainHome = null;
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
		}
	}
}