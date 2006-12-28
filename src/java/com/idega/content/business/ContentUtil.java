/*
 * $Id: ContentUtil.java,v 1.11 2006/12/28 11:50:11 gediminas Exp $
 *
 * Copyright (C) 2004 Idega. All Rights Reserved.
 *
 * This software is the proprietary information of Idega.
 * Use is subject to license terms.
 *
 */
package com.idega.content.business;

import javax.faces.context.FacesContext;
import com.idega.core.accesscontrol.business.AccessController;
import com.idega.core.accesscontrol.business.StandardRoles;
import com.idega.idegaweb.IWBundle;
import com.idega.idegaweb.IWMainApplication;
import com.idega.idegaweb.IWUserContext;

/**
 * 
 * Last modified: $Date: 2006/12/28 11:50:11 $ by $Author: gediminas $
 *
 * @author Joakim Johnson
 * @version $Revision: 1.11 $
 */
public class ContentUtil {
	public static final String CONTENT_PATH = "/files/cms";
	public static final String PAGES_PATH = "/files/cms/pages";
	
	public static final String MODULE_PREFIX = "cms_";
	public static final String IW_BUNDLE_IDENTIFIER = "com.idega.content";
	
	public static String FAMILY_CONTENT="iw_content";
	
	private static String[] defaultContentEditorRoles = new String[] {StandardRoles.ROLE_KEY_ADMIN,StandardRoles.ROLE_KEY_AUTHOR,StandardRoles.ROLE_KEY_EDITOR};
	
	private static IWBundle bundle = null;
	
	public static IWBundle getBundle() {
		if (bundle == null) {
			setupBundle();
		}
		return bundle;
	}

	private static void setupBundle() {
		IWMainApplication app = null;
		FacesContext context = FacesContext.getCurrentInstance();
		if (context != null) {
			app = IWMainApplication.getIWMainApplication(context);
		}
		else {
			app = IWMainApplication.getDefaultIWMainApplication();
		}
		bundle = app.getBundle(IW_BUNDLE_IDENTIFIER);
	}
	
	/**
	 * <p>
	 * This article returns the standard root or 'baseFolderPath' for content in the cms system.<br/>
	 * By default this is /files/cms
	 * </p>
	 * @return
	 */
	public static String getContentBaseFolderPath(){
		return CONTENT_PATH;
	}
	
	public static String getParentPath(String path){
//		if(null!=path) {
//			return new File(path).getParent();
//		}
		if (path != null) {
			int index = path.lastIndexOf("/");
			if (index == 0) {
				path = "";
			}else if (index == -1) {
				path = null;
			} else {
				path = path.substring(0, index);
			}
		} else {
			return null;
		}
		return path;
	}

	/**
	 * <p>
	 * Returns true if the user with the IWUserContext has the necessary roles
	 * to be an editor for the content system.
	 * </p>
	 * @param iwc
	 * @return
	 */
	public static boolean hasContentEditorRoles(IWUserContext iwc) {
		AccessController ac = iwc.getApplicationContext().getIWMainApplication().getAccessController();
		for (int i = 0; i < defaultContentEditorRoles.length; i++) {
			if(ac.hasRole(defaultContentEditorRoles[i],iwc)){
				return true;
			}
		}
		return false;
	}
}
