/*
 * $Id: ContentUtil.java,v 1.3 2005/01/17 17:20:18 gummi Exp $
 *
 * Copyright (C) 2004 Idega. All Rights Reserved.
 *
 * This software is the proprietary information of Idega.
 * Use is subject to license terms.
 *
 */
package com.idega.content.business;

import javax.faces.context.FacesContext;
import com.idega.idegaweb.IWBundle;
import com.idega.presentation.IWContext;


/**
 * 
 * Last modified: $Date: 2005/01/17 17:20:18 $ by $Author: gummi $
 *
 * @author Joakim Johnson
 * @version $Revision: 1.3 $
 */
public class ContentUtil {
	public static final String CONTENT_PATH = "/files/cms";
	public static final String ARTICLE_PATH = "/files/cms/article";
	public static final String PAGES_PATH = "/files/cms/pages";
	
	public static final String MODULE_PREFIX = "cms_";
	public static final String IW_BUNDLE_IDENTIFIER = "com.idega.content";
	
	private static IWBundle bundle = null;
	
	public static IWBundle getBundle() {
		if (bundle == null) {
			setupBundle();
		}
		return bundle;
	}

	private static void setupBundle() {
		FacesContext context = FacesContext.getCurrentInstance();
		IWContext iwContext = IWContext.getIWContext(context);
		bundle = iwContext.getIWMainApplication().getBundle(IW_BUNDLE_IDENTIFIER);
	}
}
