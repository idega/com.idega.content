/*
 * $Id: ContentViewManager.java,v 1.17 2005/10/19 18:23:34 tryggvil Exp $
 * Created on 2.11.2004
 *
 * Copyright (C) 2004 Idega Software hf. All Rights Reserved.
 *
 * This software is the proprietary information of Idega hf.
 * Use is subject to license terms.
 */
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
import com.idega.repository.data.Singleton;


/**
 *  This is the class modules should use to attatch themselves on to the Content application view structure.
 * 
 *  Last modified: $Date: 2005/10/19 18:23:34 $ by $Author: tryggvil $
 * 
 * @author <a href="mailto:tryggvil@idega.com">Tryggvi Larusson</a>
 * @version $Revision: 1.17 $
 */
public class ContentViewManager implements Singleton  {

	private static final String IW_CONTENT_VIEW_MANAGER_KEY = "iw_contentviewmanager";
	private static final String CONTENT_ID="content";
	private static final String CONTENT_BUNDLE_IDENTIFIER="com.idega.content";
	private ViewNode contentRootNode;
	private IWMainApplication iwma;
	
	private ContentViewManager(IWMainApplication iwma){
		this.iwma=iwma;
	}

	  public static synchronized ContentViewManager getInstance(IWMainApplication iwma){
	    ContentViewManager contentViewManager = (ContentViewManager) iwma.getAttribute(IW_CONTENT_VIEW_MANAGER_KEY);
	    if(contentViewManager==null){
	      contentViewManager = new ContentViewManager(iwma);
	      iwma.setAttribute(IW_CONTENT_VIEW_MANAGER_KEY,contentViewManager);
	    }
	    return contentViewManager;
	  }	
	
	public static ContentViewManager getInstance(FacesContext context){
		IWMainApplication iwma = IWMainApplication.getIWMainApplication(context);
		return getInstance(iwma);
	}
	
	public ViewManager getViewManager(){
		return ViewManager.getInstance(iwma);
	}
	
	
	public ViewNode getContentNode(){
		IWBundle iwb = iwma.getBundle(CONTENT_BUNDLE_IDENTIFIER);
		//ViewNode content = root.getChild(CONTENT_ID);
		if(contentRootNode==null){
			contentRootNode = initalizeContentNode(iwb);
		}
		return contentRootNode;
	}
	
	public ViewNode initalizeContentNode(IWBundle contentBundle){
		ViewNode root = getViewManager().getWorkspaceRoot();
		DefaultViewNode contentNode = new ApplicationViewNode(CONTENT_ID,root);
		Collection roles = new ArrayList();
		roles.add(StandardRoles.ROLE_KEY_EDITOR);
		roles.add(StandardRoles.ROLE_KEY_AUTHOR);
		contentNode.setAuthorizedRoles(roles);
		
		
		contentNode.setJspUri(contentBundle.getJSPURI("content.jsp"));
		contentNode.setKeyboardShortcut(new KeyboardShortcut("4"));
		
		contentRootNode = contentNode;
		return contentRootNode;
	}
	
	
	public void initializeStandardNodes(IWBundle bundle){
		ViewNode contentNode = initalizeContentNode(bundle);
		
		/* Page nodes begin */
		DefaultViewNode pagesNode = new DefaultViewNode("pages",contentNode);
		pagesNode.setJspUri(bundle.getJSPURI("pages.jsp"));
		pagesNode.setKeyboardShortcut(new KeyboardShortcut("p"));
		pagesNode.setName("#{localizedStrings['com.idega.content']['pages']}");
		
		//DefaultViewNode pageListNode = new DefaultViewNode("list",pagesNode);
		//pageListNode.setJspUri(bundle.getJSPURI("pages.jsp"));
		//pageListNode.setName("#{localizedStrings['com.idega.content']['list_pages']}");
		
		DefaultViewNode createPageNode = new DefaultViewNode("create",pagesNode);
		createPageNode.setJspUri(bundle.getJSPURI("createpage.jsp"));
		createPageNode.setName("#{localizedStrings['com.idega.content']['create_page']}");
		createPageNode.setVisibleInMenus(false);
		
		DefaultViewNode previewPageNode = new DefaultViewNode("preview",pagesNode);
		previewPageNode.setJspUri(bundle.getJSPURI("pagepreview.jsp"));
		previewPageNode.setVisibleInMenus(false);
		
		DefaultViewNode detailsPageNode = new DefaultViewNode("details",pagesNode);
		detailsPageNode.setJspUri(bundle.getJSPURI("pagedetails.jsp"));
		detailsPageNode.setVisibleInMenus(false);
		detailsPageNode.setName("#{localizedStrings['com.idega.content']['page_details']}");
		
		DefaultViewNode simpleTemplateNode = new DefaultViewNode("templatesettings",pagesNode);
		simpleTemplateNode.setJspUri(bundle.getJSPURI("simpletemplate.jsp"));
		simpleTemplateNode.setName("#{localizedStrings['com.idega.content']['template_settings']}");
		simpleTemplateNode.setVisibleInMenus(false);
		
		/* Page nodes end */
		
		
		DefaultViewNode documentsNode = new DefaultViewNode("documents",contentNode);
		//documentsNode.setJspUri(bundle.getJSPURI("documents.jsp"));
		documentsNode.setJspUri(bundle.getJSPURI("listDocuments.jsp"));
		documentsNode.setKeyboardShortcut(new KeyboardShortcut("d"));
		documentsNode.setName("#{localizedStrings['com.idega.content']['documents']}");
		
		DefaultViewNode previewNode = new DefaultViewNode("preview",documentsNode);
		previewNode.setJspUri(bundle.getJSPURI("listDocuments.jsp"));
		previewNode.setVisibleInMenus(false);
		
		DefaultViewNode permissionNode = new DefaultViewNode("permission",documentsNode);
		permissionNode.setJspUri(bundle.getJSPURI("listDocuments.jsp"));
		permissionNode.setVisibleInMenus(false);
		
		DefaultViewNode searchNode = new DefaultViewNode("search",contentNode);
		searchNode.setJspUri(bundle.getJSPURI("search.jsp"));	
		searchNode.setKeyboardShortcut(new KeyboardShortcut("s"));
		searchNode.setName("#{localizedStrings['com.idega.content']['search']}");
		
	}
}
