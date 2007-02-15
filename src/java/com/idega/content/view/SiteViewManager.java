package com.idega.content.view;

import java.util.ArrayList;
import java.util.Collection;

import javax.faces.context.FacesContext;

import com.idega.core.accesscontrol.business.StandardRoles;
import com.idega.core.view.ApplicationViewNode;
import com.idega.core.view.DefaultViewNode;
import com.idega.core.view.KeyboardShortcut;
import com.idega.core.view.ViewManager;
import com.idega.core.view.ViewNode;
import com.idega.idegaweb.IWBundle;
import com.idega.idegaweb.IWMainApplication;

public class SiteViewManager {
	private static final String IW_SITE_VIEW_MANAGER_KEY = "iw_siteviewmanager";
	private static final String CONTENT_ID="site";
	private static final String CONTENT_BUNDLE_IDENTIFIER="com.idega.content";
	private ViewNode contentRootNode;
	private IWMainApplication iwma;
	
	private SiteViewManager(IWMainApplication iwma){
		this.iwma=iwma;
	}

	  public static synchronized SiteViewManager getInstance(IWMainApplication iwma){
		  SiteViewManager siteViewManager = (SiteViewManager) iwma.getAttribute(IW_SITE_VIEW_MANAGER_KEY);
	    if(siteViewManager==null){
	    	siteViewManager = new SiteViewManager(iwma);
	      iwma.setAttribute(IW_SITE_VIEW_MANAGER_KEY,siteViewManager);
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
	
	
	public ViewNode getContentNode(){
		IWBundle iwb = this.iwma.getBundle(CONTENT_BUNDLE_IDENTIFIER);
		//ViewNode content = root.getChild(CONTENT_ID);
		if(this.contentRootNode==null){
			this.contentRootNode = initalizeContentNode(iwb);
		}
		return this.contentRootNode;
	}
	
	public ViewNode initalizeContentNode(IWBundle contentBundle){
		ViewNode root = getViewManager().getWorkspaceRoot();
		DefaultViewNode contentNode = new ApplicationViewNode(CONTENT_ID,root);
		Collection<String> roles = new ArrayList<String>();
		roles.add(StandardRoles.ROLE_KEY_EDITOR);
		roles.add(StandardRoles.ROLE_KEY_AUTHOR);
		contentNode.setAuthorizedRoles(roles);
		
		
		contentNode.setJspUri(contentBundle.getJSPURI("site.jsp"));
		contentNode.setKeyboardShortcut(new KeyboardShortcut("4"));
		
		this.contentRootNode = contentNode;
		return this.contentRootNode;
	}
	
	
	public void initializeStandardNodes(IWBundle bundle){
		ViewNode contentNode = initalizeContentNode(bundle);
		
		/* Page nodes begin */
		DefaultViewNode siteNode = new DefaultViewNode("site", contentNode);
		siteNode.setJspUri(bundle.getJSPURI("pages.jsp"));
		siteNode.setKeyboardShortcut(new KeyboardShortcut("p"));
		siteNode.setName("#{localizedStrings['com.idega.content']['site']}");
		
//		DefaultViewNode themes = new DefaultViewNode("themes_manager", siteNode);
//		themes.setJspUri(bundle.getJSPURI("themes.jsp"));
//		themes.setName("#{localizedStrings['com.idega.content']['themes_manager']}");		

		DefaultViewNode themes = new DefaultViewNode("themes", siteNode);
		themes.setJspUri(bundle.getJSPURI("themes.jsp"));
		themes.setName("#{localizedStrings['com.idega.content']['themes']}");				
		
		DefaultViewNode siteManagerNode = new DefaultViewNode("site_manager", siteNode);
		siteManagerNode.setJspUri(bundle.getJSPURI("site.jsp"));
		siteManagerNode.setName("#{localizedStrings['com.idega.content']['site_manager']}");

		DefaultViewNode pagesNode = new DefaultViewNode("pages", siteNode);
		pagesNode.setJspUri(bundle.getJSPURI("pages.jsp"));
		pagesNode.setName("#{localizedStrings['com.idega.content']['pages']}");		
		
		DefaultViewNode treeNode = new DefaultViewNode("tree", pagesNode);
		treeNode.setJspUri(bundle.getJSPURI("tree.jsp"));
		treeNode.setName("#{localizedStrings['com.idega.content']['site_map']}");
		treeNode.setVisibleInMenus(false);
		
//		DefaultViewNode pageListNode = new DefaultViewNode("list",pagesNode);
//		pageListNode.setJspUri(bundle.getJSPURI("pages.jsp"));
//		pageListNode.setName("#{localizedStrings['com.idega.content']['list_pages']}");
		
//		DefaultViewNode createPageNode = new DefaultViewNode("create",pagesNode);
//		createPageNode.setJspUri(bundle.getJSPURI("createpage.jsp"));
//		createPageNode.setName("#{localizedStrings['com.idega.content']['create_page']}");
//		createPageNode.setVisibleInMenus(false);
		
//		DefaultViewNode previewPageNode = new DefaultViewNode("preview",pagesNode);
//		previewPageNode.setJspUri(bundle.getJSPURI("pagepreview.jsp"));
//		previewPageNode.setVisibleInMenus(false);
		
//		DefaultViewNode detailsPageNode = new DefaultViewNode("details",pagesNode);
//		detailsPageNode.setJspUri(bundle.getJSPURI("pagedetails.jsp"));
//		detailsPageNode.setVisibleInMenus(false);
//		detailsPageNode.setName("#{localizedStrings['com.idega.content']['page_details']}");
		
//		DefaultViewNode simpleTemplateNode = new DefaultViewNode("templatesettings",pagesNode);
//		simpleTemplateNode.setJspUri(bundle.getJSPURI("simpletemplate.jsp"));
//		simpleTemplateNode.setName("#{localizedStrings['com.idega.content']['template_settings']}");
//		simpleTemplateNode.setVisibleInMenus(false);
		
//		DefaultViewNode pages = new DefaultViewNode("Change pages", pagesNode);
//		pages.setJspUri(bundle.getJSPURI("pages2.jsp"));
//		pages.setName("Change pages 2");
//		pages.setVisibleInMenus(false);
		
//		DefaultViewNode templatesTemplateNode = new DefaultViewNode("templates",pagesNode);
//		templatesTemplateNode.setJspUri(bundle.getJSPURI("templates.jsp"));
//		templatesTemplateNode.setName("#{localizedStrings['com.idega.content']['Templates']}");
//		templatesTemplateNode.setVisibleInMenus(false);
		/* Page nodes end */
		
		
//		DefaultViewNode documentsNode = new DefaultViewNode("documents",contentNode);
//		//documentsNode.setJspUri(bundle.getJSPURI("documents.jsp"));
//		documentsNode.setJspUri(bundle.getJSPURI("listDocuments.jsp"));
//		documentsNode.setKeyboardShortcut(new KeyboardShortcut("d"));
//		documentsNode.setName("#{localizedStrings['com.idega.content']['documents']}");
		
//		DefaultViewNode previewNode = new DefaultViewNode("preview",documentsNode);
//		previewNode.setJspUri(bundle.getJSPURI("listDocuments.jsp"));
//		previewNode.setVisibleInMenus(false);
		
//		DefaultViewNode permissionNode = new DefaultViewNode("permission",documentsNode);
//		permissionNode.setJspUri(bundle.getJSPURI("listDocuments.jsp"));
//		permissionNode.setVisibleInMenus(false);
		
//		DefaultViewNode searchNode = new DefaultViewNode("search",contentNode);
//		searchNode.setJspUri(bundle.getJSPURI("search.jsp"));	
//		searchNode.setKeyboardShortcut(new KeyboardShortcut("s"));
//		searchNode.setName("#{localizedStrings['com.idega.content']['search']}");
		
	}

}
