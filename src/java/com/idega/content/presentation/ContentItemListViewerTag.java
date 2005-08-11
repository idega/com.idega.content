/*
 * $Id: ContentItemListViewerTag.java,v 1.5 2005/08/11 18:01:08 dainis Exp $
 * Created on 31.1.2005
 *
 * Copyright (C) 2005 Idega Software hf. All Rights Reserved.
 *
 * This software is the proprietary information of Idega hf.
 * Use is subject to license terms.
 */
package com.idega.content.presentation;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.StringTokenizer;
import javax.faces.component.UIComponent;
import javax.faces.webapp.UIComponentTag;
import com.idega.content.presentation.ContentItemListViewer;


/**
 * 
 *  Last modified: $Date: 2005/08/11 18:01:08 $ by $Author: dainis $
 * 
 * @author <a href="mailto:gummi@idega.com">Gudmundur Agust Saemundsson</a>
 * @version $Revision: 1.5 $
 */
public class ContentItemListViewerTag extends UIComponentTag {

	private String resourcePath;
	private String managedBeanId;
	private String detailsViewerPath;
	private List categories = null;
	private static final String listDelim = ",";
	private String firstArticleItemStyleClass = null;
	
	/**
	 * 
	 */
	public ContentItemListViewerTag() {
		super();
	}
	
	
	
	public String getComponentType() {
		return "ContentItemListViewer";	
	}

	public String getRendererType() {
		return null;
	}
	
	public void release() {      
		super.release();      
		resourcePath = null; 
		managedBeanId = null;
		detailsViewerPath = null;
		categories = null;
		firstArticleItemStyleClass = null;
	}

	protected void setProperties(UIComponent component) {      
		super.setProperties(component);
		if (component != null) {
			ContentItemListViewer viewer = ((ContentItemListViewer)component);
			viewer.setBeanIdentifier(managedBeanId);
			viewer.setResourcePath(resourcePath);
			viewer.setDetailsViewerPath(detailsViewerPath);
			viewer.setCategories(categories);
			viewer.setFirstArticleItemStyleClass(firstArticleItemStyleClass);
		}
	}
	
	public void setResourcePath(String path) {
		this.resourcePath = path;
	}
	
	public String getResourcePath() {
		return resourcePath;
	}
	
	public void setBeanIdentifier(String identifier) {
		this.managedBeanId = identifier;
	}
	
	public String getBeanIdentifier() {
		return managedBeanId;
	}
	/**
	 * @return Returns the detailsViewerPath.
	 */
	public String getDetailsViewerPath() {
		return detailsViewerPath;
	}
	/**
	 * @param detailsViewerPath The detailsViewerPath to set.
	 */
	public void setDetailsViewerPath(String detailsViewerPath) {
		this.detailsViewerPath = detailsViewerPath;
	}
	/**
	 * @return Returns the categories.
	 */
	public String getCategories() {
		if(categories!=null){
			Iterator iter = categories.iterator();
			if(iter.hasNext()){
				StringBuffer catString = new StringBuffer();
				catString.append((String)iter.next());
				while(iter.hasNext()){
					catString.append(listDelim);
					catString.append((String)iter.next());
				}
			}
		}
		return null;
	}
	/**
	 * @param categories The categories to set.
	 */
	public void setCategories(String categories) {
		if(categories!=null){
			ArrayList cats = new ArrayList();
			StringTokenizer tokenizer = new StringTokenizer(categories,listDelim);
			while(tokenizer.hasMoreTokens()){
				cats.add(tokenizer.nextToken());
			}
			this.categories = (cats.isEmpty())?null:cats;
		} else {	
			this.categories = null;
		}
	}

	/**
	 * @return Returns the style class for first article list item.
	 */	
	public String getFirstArticleItemStyleClass() {
		return firstArticleItemStyleClass;
	}

	/**
	 * @param firstArticleItemStyleClass The first article item to set.
	 */
	public void setFirstArticleItemStyleClass(String firstArticleItemStyleClass) {
		this.firstArticleItemStyleClass = firstArticleItemStyleClass;
	}
}
