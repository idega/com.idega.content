/*
 * $Id: ContentViewManager.java,v 1.38 2007/11/14 16:20:41 valdas Exp $
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

import com.idega.content.business.ContentConstants;
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
 *  Last modified: $Date: 2007/11/14 16:20:41 $ by $Author: valdas $
 * 
 * @author <a href="mailto:tryggvil@idega.com">Tryggvi Larusson</a>
 * @version $Revision: 1.38 $
 */
public class ContentViewManager implements Singleton  {

	private static final String IW_CONTENT_VIEW_MANAGER_KEY = "iw_contentviewmanager";
	private static final String CONTENT_ID="content";
	private static final String DOCUMENTS_ID="documents";
	private ViewNode contentRootNode = null;
	private IWMainApplication iwma = null;
	
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
		return ViewManager.getInstance(this.iwma);
	}
	
	
	public ViewNode getContentNode(){
		IWBundle iwb = this.iwma.getBundle(ContentConstants.IW_BUNDLE_IDENTIFIER);
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
		
		contentNode.setJspUri(contentBundle.getJSPURI("pages.jsp"));
		contentNode.setKeyboardShortcut(new KeyboardShortcut("4"));
		contentNode.setName("#{localizedStrings['com.idega.content']['lucid_application']}");
		
		this.contentRootNode = contentNode;
		return this.contentRootNode;
	}
	
	
	public void initializeStandardNodes(IWBundle bundle){
		ViewNode contentNode = initalizeContentNode(bundle);

		DefaultViewNode pagesNode = new DefaultViewNode("pages", contentNode);
		pagesNode.setJspUri(bundle.getJSPURI("pages.jsp"));
		pagesNode.setKeyboardShortcut(new KeyboardShortcut("p"));
		pagesNode.setName("#{localizedStrings['com.idega.content']['pages']}");
		
		DefaultViewNode themesNode = new DefaultViewNode("themes_manager", contentNode);
		themesNode.setJspUri(bundle.getJSPURI("themes.jsp"));
		themesNode.setName("#{localizedStrings['com.idega.content']['themes_manager']}");		

		DefaultViewNode documentsNode = new DefaultViewNode("documents",contentNode);
		documentsNode.setJspUri(bundle.getJSPURI("listDocuments.jsp"));
		documentsNode.setKeyboardShortcut(new KeyboardShortcut("d"));
		documentsNode.setName("#{localizedStrings['com.idega.content']['documents']}");
		
		DefaultViewNode categoriesNode = new DefaultViewNode("categories",contentNode);
		categoriesNode.setJspUri(bundle.getJSPURI("categories.jsp"));	
		categoriesNode.setKeyboardShortcut(new KeyboardShortcut("c"));
		categoriesNode.setName("#{localizedStrings['com.idega.content']['categories']}");
		
		DefaultViewNode searchNode = new DefaultViewNode("search",contentNode);
		searchNode.setJspUri(bundle.getJSPURI("search.jsp"));	
		searchNode.setKeyboardShortcut(new KeyboardShortcut("s"));
		searchNode.setName("#{localizedStrings['com.idega.content']['search']}");
		
	}
}
