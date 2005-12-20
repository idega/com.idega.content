/*
 * $Id: ContentListViewerManagedBean.java,v 1.5 2005/12/20 16:42:00 tryggvil Exp $
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
 *  Last modified: $Date: 2005/12/20 16:42:00 $ by $Author: tryggvil $
 * 
 * @author <a href="mailto:gummi@idega.com">Gudmundur Agust Saemundsson</a>
 * @version $Revision: 1.5 $
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
	 * <p>
	 * TODO tryggvil describe method setMaxNumberOfDisplayed
	 * </p>
	 * @param maxItems
	 */
	public void setMaxNumberOfDisplayed(int maxItems);
	public int getMaxNumberOfDisplayed();
	
}
