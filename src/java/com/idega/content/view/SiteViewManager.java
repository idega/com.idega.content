package com.idega.content.view;

import java.util.ArrayList;
import java.util.Collection;

import javax.faces.context.FacesContext;

import com.idega.content.business.ContentConstants;
import com.idega.core.accesscontrol.business.StandardRoles;
import com.idega.core.view.ApplicationViewNode;
import com.idega.core.view.DefaultViewNode;
import com.idega.core.view.ViewManager;
import com.idega.core.view.ViewNode;
import com.idega.idegaweb.IWBundle;
import com.idega.idegaweb.IWMainApplication;

public class SiteViewManager {
	private static final String IW_SITE_VIEW_MANAGER_KEY = "iw_siteviewmanager";
	private static final String SITE_ID = "site";
	private ViewNode siteRootNode = null;
	private IWMainApplication iwma = null;
	
	private SiteViewManager(IWMainApplication iwma){
		this.iwma=iwma;
	}

	public static synchronized SiteViewManager getInstance(IWMainApplication iwma) {
		SiteViewManager siteViewManager = (SiteViewManager) iwma.getAttribute(IW_SITE_VIEW_MANAGER_KEY);
		if (siteViewManager == null) {
			siteViewManager = new SiteViewManager(iwma);
			iwma.setAttribute(IW_SITE_VIEW_MANAGER_KEY, siteViewManager);
		}
		return siteViewManager;
	}
	
	public static SiteViewManager getInstance(FacesContext context){
		IWMainApplication iwma = IWMainApplication.getIWMainApplication(context);
		return getInstance(iwma);
	}
	
	public ViewManager getViewManager(){
		return ViewManager.getInstance(this.iwma);
	}
	
	public ViewNode getSiteNode(){
		IWBundle iwb = this.iwma.getBundle(ContentConstants.IW_BUNDLE_IDENTIFIER);
		if (this.siteRootNode == null) {
			this.siteRootNode = initalizeSiteNode(iwb);
		}
		return this.siteRootNode;
	}
	
	public ViewNode initalizeSiteNode(IWBundle contentBundle){
		ViewNode root = getViewManager().getWorkspaceRoot();
		DefaultViewNode siteNode = new ApplicationViewNode(SITE_ID, root);
		Collection<String> roles = new ArrayList<String>();
		roles.add(StandardRoles.ROLE_KEY_EDITOR);
		roles.add(StandardRoles.ROLE_KEY_AUTHOR);
		siteNode.setAuthorizedRoles(roles);
		
		siteNode.setJspUri(contentBundle.getJSPURI("site.jsp"));
		
		this.siteRootNode = siteNode;
		return this.siteRootNode;
	}
	
	
	public void initializeStandardNodes(IWBundle bundle){
		ViewNode siteNode = initalizeSiteNode(bundle);
		
		DefaultViewNode pagesNode = new DefaultViewNode("pages", siteNode);
		pagesNode.setJspUri(bundle.getJSPURI("pages.jsp"));
		pagesNode.setName("#{localizedStrings['com.idega.content']['pages']}");	
		
		DefaultViewNode themes = new DefaultViewNode("themes", siteNode);
		themes.setJspUri(bundle.getJSPURI("themes.jsp"));
		themes.setName("#{localizedStrings['com.idega.content']['themes']}");
		
		DefaultViewNode siteManagerNode = new DefaultViewNode("site_manager", siteNode);
		siteManagerNode.setJspUri(bundle.getJSPURI("site_manager.jsp"));
		siteManagerNode.setName("#{localizedStrings['com.idega.content']['site_manager']}");

		DefaultViewNode treeNode = new DefaultViewNode("tree", siteNode);
		treeNode.setJspUri(bundle.getJSPURI("tree.jsp"));
		treeNode.setName("#{localizedStrings['com.idega.content']['site_manager']}");
		treeNode.setVisibleInMenus(false);

	}

}