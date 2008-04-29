/*
 * $Id: ContentListViewerManagedBean.java,v 1.7 2008/04/29 09:19:44 valdas Exp $
 * Created on 27.1.2005
 *
 * Copyright (C) 2005 Idega Software hf. All Rights Reserved.
 *
 * This software is the proprietary information of Idega hf.
 * Use is subject to license terms.
 */
package com.idega.content.bean;

import java.util.List;

import com.idega.content.presentation.ContentItemViewer;



/**
 * 
 *  Last modified: $Date: 2008/04/29 09:19:44 $ by $Author: valdas $
 * 
 * @author <a href="mailto:gummi@idega.com">Gudmundur Agust Saemundsson</a>
 * @version $Revision: 1.7 $
 */
public interface ContentListViewerManagedBean {
	/**
	 * 
	 * @return list of ContentItems
	 */
	public List getContentItems();
	public ContentItemViewer getContentViewer();
	
	/**
	 * 
	 * @return list of ContentItemViewers
	 */
	public List getAttachmentViewers();
	public void setBaseFolderPath(String path);
	public String getBaseFolderPath();
	public void setDetailsViewerPath(String path);
	public void setCategories(List<String> categories);
	public void setViewerIdentifier(String viewerIdentifier);
	
	/**
	 * 
	 * @return can return null to indicate that default handler should be used
	 */
	public String getIWActionURIHandlerIdentifier();
	
	/**
	 * 
	 * @param maxItems max number of items to display, or -1 to display all
	 */
	public void setMaxNumberOfDisplayed(int maxItems);
	
	/**
	 * 
	 * @return max number of items to display, or -1 to display all
	 */
	public int getMaxNumberOfDisplayed();
	
}
