/*
 * $Id: WebDAVBeanComparator.java,v 1.2 2005/01/13 15:54:22 gimmi Exp $
 * Created on 11.1.2005
 *
 * Copyright (C) 2005 Idega Software hf. All Rights Reserved.
 *
 * This software is the proprietary information of Idega hf.
 * Use is subject to license terms.
 */
package com.idega.content.business;

import java.util.Comparator;
import java.util.Locale;
import com.idega.content.data.WebDAVBean;
import com.idega.core.business.ICTreeNodeComparator;


/**
 * 
 *  Last modified: $Date: 2005/01/13 15:54:22 $ by $Author: gimmi $
 * 
 * @author <a href="mailto:gimmi@idega.com">gimmi</a>
 * @version $Revision: 1.2 $
 */
public class WebDAVBeanComparator extends ICTreeNodeComparator implements Comparator {

	public static final int SORT_BY_NAME = 1;
	public static final int SORT_BY_SIZE = 2;
	public static final int SORT_BY_MODIFICATION_DATE = 3;
	
	private int sortBy;
	private int multiplier = 1;
	
	public WebDAVBeanComparator() {
		super();
	}
	
	public WebDAVBeanComparator(Locale locale, int sortBy, boolean desending) {
		super(locale);
		this.sortBy = sortBy;
		if (desending) {
			multiplier = -1;
		}
	}
	
	public int compare(Object o1, Object o2) {
		
		switch (sortBy) {
			case SORT_BY_NAME :
				return multiplier * super.compare(o1, o2);
			case SORT_BY_SIZE :
				return multiplier * sizeCompare(o1, o2);
			case SORT_BY_MODIFICATION_DATE :
				return multiplier * modDateCompare(o1, o2);
			default :
				return 0;
		}
		
	}
	
	public int sizeCompare(Object o1, Object o2) {
		WebDAVBean b1 = (WebDAVBean) o1;
		WebDAVBean b2 = (WebDAVBean) o2;
		long b1Size = b1.getLengthLong();
		long b2Size = b2.getLengthLong();

		if (b1Size < b2Size) {
			return -1;
		} else if (b1Size > b2Size) {
			return 1;
		} else {
			return 0;
		}
		
	}
	
	public int modDateCompare(Object o1, Object o2) {
		WebDAVBean b1 = (WebDAVBean) o1;
		WebDAVBean b2 = (WebDAVBean) o2;
		long s1 = b1.getModifiedDateLong();
		long s2 = b2.getModifiedDateLong();

		if (s1 < s2) {
			return -1;
		} else if (s2 < s1) {
			return 1;
		} else {
			return 0;
		}
	}

}
