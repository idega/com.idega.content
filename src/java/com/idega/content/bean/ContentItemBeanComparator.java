/*
 * $Id: ContentItemBeanComparator.java,v 1.4 2007/02/20 13:53:42 valdas Exp $
 * Created on 15.2.2005
 *
 * Copyright (C) 2005 Idega Software hf. All Rights Reserved.
 *
 * This software is the proprietary information of Idega hf.
 * Use is subject to license terms.
 */
package com.idega.content.bean;

import java.util.Comparator;
import java.util.Date;


/**
 * 
 *  Last modified: $Date: 2007/02/20 13:53:42 $ by $Author: valdas $
 * 
 * @author <a href="mailto:gummi@idega.com">Gudmundur Agust Saemundsson</a>
 * @version $Revision: 1.4 $
 */
public class ContentItemBeanComparator implements Comparator {

	private boolean reverse = false;
	
	/**
	 * 
	 */
	public ContentItemBeanComparator() {
		super();
	}
	
	public void setReverseOrder(boolean value){
		this.reverse = value;
	}

	/* (non-Javadoc)
	 * @see java.util.Comparator#compare(java.lang.Object, java.lang.Object)
	 */
	public int compare(Object o1, Object o2) {
		ContentItemBean item1 = (ContentItemBean)o1;
		ContentItemBean item2 = (ContentItemBean)o2;
		int returner = 0;
		
		Date item1Date = item1.getPublishedDate() == null ? item1.getCreationDate() : item1.getPublishedDate();
		Date item2Date = item2.getPublishedDate() == null ? item2.getCreationDate() : item2.getPublishedDate();
		
		if(item1Date==null && item2Date==null){
			returner = 0;
		}
		else if(item2Date==null){
			returner = 1;
		}
		else if(item1Date==null){
			returner = -1;
		}
		else{
			returner = item1Date.compareTo(item2Date);
		}
		return returner*((this.reverse)?-1:1);
	}
}
