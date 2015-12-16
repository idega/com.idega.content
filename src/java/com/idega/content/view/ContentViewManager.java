/*
 * $Id: ContentViewManager.java,v 1.43 2008/02/22 18:10:08 eiki Exp $
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
import com.idega.util.CoreConstants;


/**
 *  This is the class modules should use to attatch themselves on to the Content application view structure.
 * 
 *  Last modified: $Date: 2008/02/22 18:10:08 $ by $Author: eiki $
 * 
 * @author <a href="mailto:tryggvil@idega.com">Tryggvi Larusson</a>
 * @version $Revision: 1.43 $
 */
public class ContentViewManager implements Singleton  {

	private static final String IW_CONTENT_VIEW_MANAGER_KEY = "iw_contentviewmanager";
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
		DefaultViewNode contentNode = new ApplicationViewNode(CoreConstants.CONTENT_VIEW_MANAGER_ID, root);
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
		Collection<String> editor = new ArrayList<String>();
		editor.add(StandardRoles.ROLE_KEY_EDITOR);
				
		ViewNode contentNode = initalizeContentNode(bundle);

		DefaultViewNode pagesNode = new DefaultViewNode(CoreConstants.PAGES_VIEW_MANAGER_ID, contentNode);
		pagesNode.setJspUri(bundle.getJSPURI("pages.jsp"));
		pagesNode.setKeyboardShortcut(new KeyboardShortcut("p"));
		pagesNode.setName("#{localizedStrings['com.idega.content']['pages']}");
		
		DefaultViewNode themesNode = new DefaultViewNode("themes", contentNode);
		themesNode.setJspUri(bundle.getJSPURI("themes.jsp"));
		themesNode.setKeyboardShortcut(new KeyboardShortcut("t"));
		themesNode.setName("#{localizedStrings['com.idega.content']['themes']}");

		DefaultViewNode usersNode = new DefaultViewNode("users", contentNode);
		usersNode.setJspUri(bundle.getJSPURI("users.jsp"));
		usersNode.setKeyboardShortcut(new KeyboardShortcut("u"));
		usersNode.setName("#{localizedStrings['com.idega.content']['users']}");
		//only editor!
		usersNode.setAuthorizedRoles(editor);
		
		DefaultViewNode documentsNode = new DefaultViewNode("documents",contentNode);
		documentsNode.setJspUri(bundle.getJSPURI("listDocuments.jsp"));
		documentsNode.setKeyboardShortcut(new KeyboardShortcut("d"));
		documentsNode.setName("#{localizedStrings['com.idega.content']['documents']}");
		
		DefaultViewNode categoriesNode = new DefaultViewNode("categories",contentNode);
		categoriesNode.setJspUri(bundle.getJSPURI("categories.jsp"));	
		categoriesNode.setKeyboardShortcut(new KeyboardShortcut("c"));
		categoriesNode.setName("#{localizedStrings['com.idega.content']['categories']}");
		//only editor!
		categoriesNode.setAuthorizedRoles(editor);
		
		DefaultViewNode searchNode = new DefaultViewNode("search",contentNode);
		searchNode.setJspUri(bundle.getJSPURI("search.jsp"));	
		searchNode.setKeyboardShortcut(new KeyboardShortcut("s"));
		searchNode.setName("#{localizedStrings['com.idega.content']['search']}");
		
		//trumpam
		
		bundle = IWMainApplication.getDefaultIWMainApplication().getBundle("com.idega.block.article");
		
		DefaultViewNode articleNode = new DefaultViewNode("article",contentNode);
		articleNode.setJspUri(bundle.getJSPURI("listarticles.jsp"));
		articleNode.setKeyboardShortcut(new KeyboardShortcut("a"));
		articleNode.setName("#{localizedStrings['com.idega.block.article']['article']}");

		DefaultViewNode createNewArticleNode = new DefaultViewNode("create",articleNode);
		String jspUri = bundle.getJSPURI("createarticle.jsp");
		createNewArticleNode.setJspUri(jspUri);
		createNewArticleNode.setVisibleInMenus(false);
		createNewArticleNode.setName("#{localizedStrings['com.idega.block.article']['create_article']}");

		DefaultViewNode editNewArticleNode = new DefaultViewNode("edit",articleNode);
		editNewArticleNode.setJspUri(bundle.getJSPURI("editarticle.jsp"));
		editNewArticleNode.setVisibleInMenus(false);
		editNewArticleNode.setName("#{localizedStrings['com.idega.block.article']['edit']}");

		DefaultViewNode deleteArticleNode = new DefaultViewNode("delete",articleNode);
		deleteArticleNode.setJspUri(bundle.getJSPURI("deletearticle.jsp"));
		deleteArticleNode.setVisibleInMenus(false);
		deleteArticleNode.setName("#{localizedStrings['com.idega.block.article']['delete']}");

		DefaultViewNode listArticlesNode = new DefaultViewNode("list",articleNode);
		listArticlesNode.setJspUri(bundle.getJSPURI("listarticles.jsp"));
		listArticlesNode.setVisibleInMenus(false);
		listArticlesNode.setName("#{localizedStrings['com.idega.block.article']['list_articles']}");


		DefaultViewNode previewArticlesNode = new DefaultViewNode("preview",articleNode);
		previewArticlesNode.setJspUri(bundle.getJSPURI("previewarticle.jsp"));
		previewArticlesNode.setVisibleInMenus(false);
		previewArticlesNode.setName("#{localizedStrings['com.idega.block.article']['preview']}");
		
		
	}
}
