/*
 * $Id: ContentItemBeanComparator.java,v 1.1 2005/02/21 16:12:45 gummi Exp $
 * Created on 15.2.2005
 *
 * Copyright (C) 2005 Idega Software hf. All Rights Reserved.
 *
 * This software is the proprietary information of Idega hf.
 * Use is subject to license terms.
 */
package com.idega.content.bean;

import java.util.Comparator;


/**
 * 
 *  Last modified: $Date: 2005/02/21 16:12:45 $ by $Author: gummi $
 * 
 * @author <a href="mailto:gummi@idega.com">Gudmundur Agust Saemundsson</a>
 * @version $Revision: 1.1 $
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
		reverse = value;
	}

	/* (non-Javadoc)
	 * @see java.util.Comparator#compare(java.lang.Object, java.lang.Object)
	 */
	public int compare(Object o1, Object o2) {
		ContentItemBean item1 = (ContentItemBean)o1;
		ContentItemBean item2 = (ContentItemBean)o2;
		int returner = 0;
		
		returner = item1.getCreationDate().compareTo(item2.getCreationDate());
		
		return returner*((reverse)?-1:1);
	}
}
