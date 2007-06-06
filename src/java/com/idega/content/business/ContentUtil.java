/*
 * $Id: ContentUtil.java,v 1.13 2007/06/06 12:08:03 valdas Exp $
 *
 * Copyright (C) 2004 Idega. All Rights Reserved.
 *
 * This software is the proprietary information of Idega.
 * Use is subject to license terms.
 *
 */
package com.idega.content.business;

import java.rmi.RemoteException;

import javax.faces.context.FacesContext;
import com.idega.core.accesscontrol.business.AccessController;
import com.idega.core.accesscontrol.business.StandardRoles;
import com.idega.core.builder.business.BuilderService;
import com.idega.core.builder.business.BuilderServiceFactory;
import com.idega.idegaweb.IWBundle;
import com.idega.idegaweb.IWMainApplication;
import com.idega.idegaweb.IWUserContext;
import com.idega.presentation.IWContext;
import com.idega.util.CoreConstants;

/**
 * 
 * Last modified: $Date: 2007/06/06 12:08:03 $ by $Author: valdas $
 *
 * @author Joakim Johnson
 * @version $Revision: 1.13 $
 */
public class ContentUtil {
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
		return CoreConstants.CONTENT_PATH;
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
	
	/**
	 * Creates path (uri) based on current time
	 * @return
	 */
	public static String getYearMonthPath(IWContext iwc) {
		BuilderService service = null;
		try {
			service = BuilderServiceFactory.getBuilderService(iwc);
		} catch (RemoteException e) {
			e.printStackTrace();
			return null;
		}
		
		return service.getYearMonthPath();
	}
}
