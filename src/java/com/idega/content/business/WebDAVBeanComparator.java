/*
 * $Id: WebDAVBeanComparator.java,v 1.3 2006/04/09 12:01:55 laddi Exp $
 * Created on 11.1.2005
 *
 * Copyright (C) 2005 Idega Software hf. All Rights Reserved.
 *
 * This software is the proprietary information of Idega hf.
 * Use is subject to license terms.
 */
package com.idega.content.business;

import java.util.Locale;
import com.idega.content.data.WebDAVBean;
import com.idega.core.business.ICTreeNodeComparator;

/**
 * 
 *  Last modified: $Date: 2006/04/09 12:01:55 $ by $Author: laddi $
 * 
 * @author <a href="mailto:gimmi@idega.com">gimmi</a>
 * @version $Revision: 1.3 $
 */
public class WebDAVBeanComparator extends ICTreeNodeComparator {

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
			this.multiplier = -1;
		}
	}
	
	public int compare(WebDAVBean o1, WebDAVBean o2) {
		switch (this.sortBy) {
			case SORT_BY_NAME :
				return this.multiplier * super.compare(o1, o2);
			case SORT_BY_SIZE :
				return this.multiplier * sizeCompare(o1, o2);
			case SORT_BY_MODIFICATION_DATE :
				return this.multiplier * modDateCompare(o1, o2);
			default :
				return 0;
		}
	}
	
	public int sizeCompare(WebDAVBean o1, WebDAVBean o2) {
		long b1Size = o1.getLengthLong();
		long b2Size = o2.getLengthLong();

		if (b1Size < b2Size) {
			return -1;
		} else if (b1Size > b2Size) {
			return 1;
		} else {
			return 0;
		}
	}
	
	public int modDateCompare(WebDAVBean o1, WebDAVBean o2) {
		long s1 = o1.getModifiedDateLong();
		long s2 = o2.getModifiedDateLong();

		if (s1 < s2) {
			return -1;
		} else if (s2 < s1) {
			return 1;
		} else {
			return 0;
		}
	}
}