/*
 * $Id: ContentViewManager.java,v 1.6 2005/01/24 17:49:04 thomas Exp $
 * Created on 2.11.2004
 *
 * Copyright (C) 2004 Idega Software hf. All Rights Reserved.
 *
 * This software is the proprietary information of Idega hf.
 * Use is subject to license terms.
 */
package com.idega.content.view;

import javax.faces.context.FacesContext;
import com.idega.core.view.ApplicationViewNode;
import com.idega.core.view.DefaultViewNode;
import com.idega.core.view.ViewManager;
import com.idega.core.view.ViewNode;
import com.idega.idegaweb.IWBundle;
import com.idega.idegaweb.IWMainApplication;


/**
 *  This is the class modules should use to attatch themselves on to the Content application view structure.
 * 
 *  Last modified: $Date: 2005/01/24 17:49:04 $ by $Author: thomas $
 * 
 * @author <a href="mailto:tryggvil@idega.com">Tryggvi Larusson</a>
 * @version $Revision: 1.6 $
 */
public class ContentViewManager {

	private static final String IW_CONTENT_VIEW_MANAGER_KEY = "iw_contentviewmanager";
	private static String CONTENT_ID="content";
	private static String CONTENT_BUNDLE_IDENTIFIER="com.idega.content";
	private ViewNode contentRootNode;
	private IWMainApplication iwma;
	
	private ContentViewManager(IWMainApplication iwma){
		this.iwma=iwma;
	}

	  public static ContentViewManager getInstance(IWMainApplication iwma){
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
		contentNode.setJspUri(contentBundle.getJSPURI("content.jsp"));
		contentRootNode = contentNode;
		return contentRootNode;
	}
	
	
	public void initializeStandardNodes(IWBundle bundle){
		ViewNode contentNode = initalizeContentNode(bundle);
		
		DefaultViewNode pagesNode = new DefaultViewNode("pages",contentNode);
		pagesNode.setJspUri(bundle.getJSPURI("pages.jsp"));
		
		DefaultViewNode documentsNode = new DefaultViewNode("documents",contentNode);
		//documentsNode.setJspUri(bundle.getJSPURI("documents.jsp"));
		documentsNode.setJspUri(bundle.getJSPURI("listDocuments.jsp"));
		
		DefaultViewNode searchNode = new DefaultViewNode("search",contentNode);
		searchNode.setJspUri(bundle.getJSPURI("search.jsp"));	
		
	}
}
