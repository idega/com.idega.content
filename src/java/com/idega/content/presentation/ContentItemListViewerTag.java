/*
 * $Id: ContentItemListViewerTag.java,v 1.8 2005/12/20 16:42:00 tryggvil Exp $
 * Created on 31.1.2005
 *
 * Copyright (C) 2005 Idega Software hf. All Rights Reserved.
 *
 * This software is the proprietary information of Idega hf.
 * Use is subject to license terms.
 */
package com.idega.content.presentation;

import javax.faces.component.UIComponent;
import javax.faces.webapp.UIComponentTag;


/**
 * 
 *  Last modified: $Date: 2005/12/20 16:42:00 $ by $Author: tryggvil $
 * 
 * @author <a href="mailto:gummi@idega.com">Gudmundur Agust Saemundsson</a>
 * @version $Revision: 1.8 $
 */
public class ContentItemListViewerTag extends UIComponentTag {

	private String baseFolderPath;
	private String managedBeanId;
	private String detailsViewerPath;
	private String categories = null;
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
		baseFolderPath = null; 
		managedBeanId = null;
		detailsViewerPath = null;
		categories = null;
		firstArticleItemStyleClass = null;
	}

	protected void setProperties(UIComponent component) {      
		super.setProperties(component);
		if (component != null) {
			ContentItemListViewer viewer = ((ContentItemListViewer)component);
			if(managedBeanId!=null){
				viewer.setBeanIdentifier(managedBeanId);
			}
			viewer.setBaseFolderPath(getBaseFolderPath());
			viewer.setDetailsViewerPath(detailsViewerPath);
			viewer.setCategories(categories);
			viewer.setFirstArticleItemStyleClass(firstArticleItemStyleClass);
		}
	}
	
	/**
	 * @deprecated replaced with setBaseFolderPath
	 */
	public void setResourcePath(String path) {
		setBaseFolderPath(path);
	}
	/**
	 * @deprecated replaced with getBaseFolderPath
	 */
	public String getResourcePath() {
		return getBaseFolderPath();
	}
	
	public void setBaseFolderPath(String path) {
		this.baseFolderPath = path;
	}
	
	public String getBaseFolderPath() {
		return baseFolderPath;
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
		return categories;
	}
	/**
	 * @param categories The categories to set.
	 */
	public void setCategories(String categories) {
		this.categories=categories;
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
