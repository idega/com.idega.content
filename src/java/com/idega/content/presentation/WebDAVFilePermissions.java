/*
 * $Id: WebDAVFilePermissions.java,v 1.1 2005/01/07 19:46:49 gummi Exp $
 * Created on 29.12.2004
 *
 * Copyright (C) 2004 Idega Software hf. All Rights Reserved.
 *
 * This software is the proprietary information of Idega hf.
 * Use is subject to license terms.
 */
package com.idega.content.presentation;

import javax.faces.component.html.HtmlCommandButton;
import javax.faces.component.html.HtmlOutputText;
import com.idega.slide.util.WebdavExtendedResource;
import com.idega.webface.WFList;
import com.idega.webface.WFUtil;


/**
 * 
 *  Last modified: $Date: 2005/01/07 19:46:49 $ by $Author: gummi $
 * 
 * @author <a href="mailto:gummi@idega.com">Gudmundur Agust Saemundsson</a>
 * @version $Revision: 1.1 $
 */
public class WebDAVFilePermissions extends ContentBlock {

	private static final String ATTRIBUTE_RESOURCE_PATH = "resourcePath";
	private static final String BEANID_STANDARD_MATRIX = "standardPermissionMatrix";
	private static final String BEANID_ROLE_MATRIX = "rolePermissionMatrix";
	private static final String BEANID_GROUP_MATRIX = "groupPermissionMatrix";
	private static final String BEANID_USER_MATRIX = "userPermissionMatrix";
	private static final String BEANID_SHARED_MATRIX_BEAN = "sharedPermissionMatrix";
	
	public WebDAVFilePermissions(){
	}
	
	/* (non-Javadoc)
	 * @see com.idega.presentation.IWBaseComponent#initializeContent()
	 */
	protected void initializeContent() {		
		
		initializePermissionMatirx(BEANID_STANDARD_MATRIX,"permissionmatrix.standard");
		initializePermissionMatirx(BEANID_ROLE_MATRIX,"permissionmatrix.roles");
		initializePermissionMatirx(BEANID_GROUP_MATRIX,"permissionmatrix.groups");
		initializePermissionMatirx(BEANID_USER_MATRIX,"permissionmatrix.users");
		
		
		HtmlCommandButton refreshButton = new HtmlCommandButton();
		refreshButton.setId("refresh_permissions");
		getBundle().getLocalizedUIComponent("refresh", refreshButton);
		refreshButton.setAction(WFUtil.createMethodBinding("#{"+BEANID_SHARED_MATRIX_BEAN+".refresh}", new Class[0]));
		
		HtmlCommandButton button = new HtmlCommandButton();
		button.setId("save_permissions");
		getBundle().getLocalizedUIComponent("save", button);
		button.setAction(WFUtil.createMethodBinding("#{"+BEANID_SHARED_MATRIX_BEAN+".saveACL}", new Class[0]));
		
		getChildren().add(refreshButton);
		getChildren().add(button);
	}
	
	
	protected void initializePermissionMatirx(String beanID, String headerLocalizedKey){
		
		WFList pMatrix = new WFList(beanID);
		pMatrix.setId(beanID);
		
		String resourcePath = (String) this.getAttributes().get(ATTRIBUTE_RESOURCE_PATH);
		if(resourcePath!=null){
			WFUtil.invoke(beanID, "setResourcePath", resourcePath);
		} else {
			WebdavExtendedResource resource = getWebdavExtendedResource();
			if(resource != null){
				WFUtil.invoke(beanID, "setResourcePath", resource.getEncodedPath());
			}
		}
		
		HtmlOutputText headerStandard = getBundle().getLocalizedText(headerLocalizedKey);
		headerStandard.setId(beanID+".title");

		HtmlCommandButton addButton = new HtmlCommandButton();
		addButton.setId(beanID+".add_principal");
		addButton.setStyleClass("permission_add_button");
		addButton.setValue("+");
//		getBundle().getLocalizedUIComponent("add", addButton);
		
		getChildren().add(headerStandard);
		getChildren().add(pMatrix);
		getChildren().add(addButton);

	}
	
	
	public void setResourcePath(String resourcePath){
		this.getAttributes().put(ATTRIBUTE_RESOURCE_PATH,resourcePath);
	}
	
	
}
