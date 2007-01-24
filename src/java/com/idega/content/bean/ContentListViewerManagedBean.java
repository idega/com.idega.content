/*
 * $Id: ContentListViewerManagedBean.java,v 1.5.2.1 2007/01/24 10:25:31 gediminas Exp $
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
 *  Last modified: $Date: 2007/01/24 10:25:31 $ by $Author: gediminas $
 * 
 * @author <a href="mailto:gummi@idega.com">Gudmundur Agust Saemundsson</a>
 * @version $Revision: 1.5.2.1 $
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
	public void setCategories(List categories);
	
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
