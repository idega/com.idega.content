/*
 * $Id: WebDAVBeanComparator.java,v 1.1 2005/01/11 11:12:10 gimmi Exp $
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
import com.idega.core.business.ICTreeNodeComparator;


/**
 * 
 *  Last modified: $Date: 2005/01/11 11:12:10 $ by $Author: gimmi $
 * 
 * @author <a href="mailto:gimmi@idega.com">gimmi</a>
 * @version $Revision: 1.1 $
 */
public class WebDAVBeanComparator extends ICTreeNodeComparator implements Comparator {

	public WebDAVBeanComparator() {
		super();
	}
	
	public WebDAVBeanComparator(Locale locale) {
		super(locale);
	}

}
