/*
 * $Id: WebDAVFilePermissions.java,v 1.3 2005/01/18 17:44:31 gummi Exp $
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
import javax.faces.context.FacesContext;
import com.idega.content.business.ContentUtil;
import com.idega.webface.WFList;
import com.idega.webface.WFUtil;


/**
 * 
 *  Last modified: $Date: 2005/01/18 17:44:31 $ by $Author: gummi $
 * 
 * @author <a href="mailto:gummi@idega.com">Gudmundur Agust Saemundsson</a>
 * @version $Revision: 1.3 $
 */
public class WebDAVFilePermissions extends ContentBlock {

	private static final String ATTRIBUTE_RESOURCE_PATH = "resourcePath";
	private static final String BEANID_STANDARD_MATRIX = "standardPermissionMatrix";
	private static final String BEANID_ROLE_MATRIX = "rolePermissionMatrix";
	private static final String BEANID_GROUP_MATRIX = "groupPermissionMatrix";
	private static final String BEANID_USER_MATRIX = "userPermissionMatrix";
	private static final String BEANID_SHARED_MATRIX_BEAN = "sharedPermissionMatrix";
	
	private String[] permissionMatrixIDs = new String[] {BEANID_STANDARD_MATRIX,BEANID_ROLE_MATRIX,BEANID_GROUP_MATRIX,BEANID_USER_MATRIX,BEANID_SHARED_MATRIX_BEAN};
	
	public WebDAVFilePermissions(){
	}
	
	/* (non-Javadoc)
	 * @see com.idega.presentation.IWBaseComponent#initializeContent()
	 */
	protected void initializeContent() {
		
		
		String resourcePath = getCurrentResourcePath();
		if(resourcePath!=null){
			WFUtil.invoke(BEANID_SHARED_MATRIX_BEAN, "setResourcePath", resourcePath);
		} else {
			System.err.println("[WARNING]["+getClass().getName()+"]: resource path is null");
		}
		initializePermissionMatirx(BEANID_STANDARD_MATRIX,"permissionmatrix.standard",resourcePath);
		initializePermissionMatirx(BEANID_ROLE_MATRIX,"permissionmatrix.roles",resourcePath);
//		initializePermissionMatirx(BEANID_GROUP_MATRIX,"permissionmatrix.groups",resourcePath);
//		
//		HtmlCommandButton addButton = new HtmlCommandButton();
//		addButton.setId(BEANID_GROUP_MATRIX+".add_principal");
//		addButton.setStyleClass("permission_add_button");
//		addButton.setValue("+");
////		getBundle().getLocalizedUIComponent("add", addButton);
//		getChildren().add(addButton);
		
//		initializePermissionMatirx(BEANID_USER_MATRIX,"permissionmatrix.users",resourcePath);
		
		
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
	

	protected void initializePermissionMatirx(String beanID, String headerLocalizedKey, String resourcePath){
		
		if(resourcePath!=null){
			WFUtil.invoke(beanID, "setResourcePath", resourcePath);
		} else {
			System.err.println("[WARNING]["+getClass().getName()+"]: resource path is null");
		}
		
		WFList pMatrix = new WFList(beanID);
		pMatrix.setId(beanID);
		pMatrix.setListStyleClass(ContentUtil.MODULE_PREFIX+"permission_matrix");
		pMatrix.setRowClasses("cms_permission_listoddrow,cms_permission_listevenrow");
		
		HtmlOutputText headerStandard = getBundle().getLocalizedText(headerLocalizedKey);
		headerStandard.setId(beanID+".title");
		headerStandard.setStyleClass(ContentUtil.MODULE_PREFIX+"permission_matrix_header");

		

		
		getChildren().add(headerStandard);
		getChildren().add(pMatrix);
		

	}	
	
	
	/**
	 * @see javax.faces.component.StateHolder#restoreState(javax.faces.context.FacesContext,
	 *      java.lang.Object)
	 */
	public void processRestoreState(FacesContext ctx, Object state) {
		super.processRestoreState(ctx, state);
		String resourcePath = getCurrentResourcePath();
		if(resourcePath!=null){
			for (int i = 0; i < permissionMatrixIDs.length; i++) {
				WFUtil.invoke(permissionMatrixIDs[i], "setResourcePath", resourcePath);
			}
		} else {
			System.err.println("[WARNING]["+getClass().getName()+"]: resource path can not be restored for managed beans");
		}
	}

}
