/*
 * $Id: WebDAVFilePermissionsManagedBean.java,v 1.9 2009/05/15 07:23:54 valdas Exp $ Created
 * on 29.12.2004
 * 
 * Copyright (C) 2004 Idega Software hf. All Rights Reserved.
 * 
 * This software is the proprietary information of Idega hf. Use is subject to
 * license terms.
 */
package com.idega.content.presentation;

import java.io.IOException;
import java.rmi.RemoteException;
import java.util.ArrayList;
import java.util.Collection;
import javax.faces.component.UIComponent;
import javax.faces.component.html.HtmlOutputText;
import javax.faces.component.html.HtmlSelectOneMenu;
import javax.faces.event.AbortProcessingException;
import org.apache.commons.httpclient.HttpException;
import com.idega.business.IBOLookup;
import com.idega.business.IBOLookupException;
import com.idega.content.business.ContentUtil;
import com.idega.content.business.WebDAVFilePermissionResource;
import com.idega.idegaweb.UnavailableIWContext;
import com.idega.presentation.IWContext;
import com.idega.slide.business.IWSlideService;
import com.idega.slide.business.IWSlideSession;
import com.idega.slide.util.IWSlideConstants;
import com.idega.webface.bean.AbstractWFEditableListManagedBean;
import com.idega.webface.bean.WFEditableListDataBean;

/**
 * 
 * Last modified: $Date: 2009/05/15 07:23:54 $ by $Author: valdas $
 * 
 * @author <a href="mailto:gummi@idega.com">Gudmundur Agust Saemundsson </a>
 * @version $Revision: 1.9 $
 */
public class WebDAVFilePermissionsManagedBean extends AbstractWFEditableListManagedBean {

	protected String[] localizationKey = new String[] { "principal_name", "privileges_all", "privileges_read",
			"privileges_write", "privileges_read_acl", "privileges_write_acl" };

	
	private int _resourceType = -1;
	public static final int RESOURCE_TYPE_STANDARD = 0;
	public static final int RESOURCE_TYPE_ROLE = 1;
	public static final int RESOURCE_TYPE_GROUP = 2;
	public static final int RESOURCE_TYPE_USER = 3;
	public static final int RESOURCE_TYPE_SHARED = 4;

	private String _resourcePath = null;

	private int minRows = 5;
	/**
	 *  
	 */
	public WebDAVFilePermissionsManagedBean() {
		super();
	}

	public void setResourceType(Integer type){
		this._resourceType = type.intValue();
	}
	
	public void setResourceType(int type){
		this._resourceType = type;
	}
	
	public void setResourcePath(String path){
		IWContext iwc = IWContext.getInstance();
		this._resourcePath = path;
		try {
			IWSlideService service = IBOLookup.getServiceInstance(iwc,IWSlideService.class);
			this._resourcePath = service.getPath(path);
		}
		catch (IBOLookupException e) {
			e.printStackTrace();
		}
		catch (RemoteException e) {
			e.printStackTrace();
		}
	}
	
	/*
	 * (non-Javadoc)
	 * 
	 * @see com.idega.webface.bean.WFPropertyMatrixManagedBean#getData()
	 */
	@Override
	public WFEditableListDataBean[] getData() {
		IWContext iwc = IWContext.getInstance();
		try {
			WebDAVFilePermissionResource resource = (WebDAVFilePermissionResource) IBOLookup.getSessionInstance(
					iwc, WebDAVFilePermissionResource.class);
			Collection aces = null;
			
			switch (this._resourceType) {
				case RESOURCE_TYPE_STANDARD:
					aces = resource.getStandardAces(this._resourcePath);
					break;
				case RESOURCE_TYPE_ROLE:
					aces = resource.getRoleAces(this._resourcePath);
					break;
				case RESOURCE_TYPE_GROUP:
					aces = resource.getGroupAces(this._resourcePath);
					break;
				case RESOURCE_TYPE_USER:
					aces = resource.getUserAces(this._resourcePath);
					break;
				default:
					aces = resource.getAllAces(this._resourcePath);
					break;
			}
			
			if(aces.size() < this.minRows){
				Collection tmp = aces;
				aces = new ArrayList();  //!!!!!! Swaping collection behind the aces variable;
				aces.addAll(tmp);
				EmptyRow emptyRow = new EmptyRow();
				for(int i = aces.size(); i <= this.minRows; i++){
					aces.add(emptyRow);
				}
			}
				
			if (aces != null) {
				WFEditableListDataBean[] toReturn =  (WFEditableListDataBean[])aces.toArray(new WFEditableListDataBean[aces.size()]);
				return toReturn;
			}
		}
		catch (IBOLookupException e) {
			e.printStackTrace();
		}
		catch (HttpException e) {
			e.printStackTrace();
		}
		catch (RemoteException e) {
			e.printStackTrace();
		}
		catch (IOException e) {
			e.printStackTrace();
		}

		return new WFEditableListDataBean[] {};
	}
	
	public void saveACL() throws AbortProcessingException {
		IWContext iwc = IWContext.getInstance();
		try {
			WebDAVFilePermissionResource resource = (WebDAVFilePermissionResource) IBOLookup.getSessionInstance(
					iwc, WebDAVFilePermissionResource.class);
			resource.store(this._resourcePath);
		}
		catch (IBOLookupException e) {
			e.printStackTrace();
		}
		catch (HttpException e) {
			e.printStackTrace();
		}
		catch (RemoteException e) {
			e.printStackTrace();
		}
		catch (IOException e) {
			e.printStackTrace();
		}
	}
	
	public void refresh() throws AbortProcessingException {
		IWContext iwc = IWContext.getInstance();
		try {
			WebDAVFilePermissionResource resource = (WebDAVFilePermissionResource) IBOLookup.getSessionInstance(
					iwc, WebDAVFilePermissionResource.class);
			resource.clear();
		}
		catch (IBOLookupException e) {
			e.printStackTrace();
		}
		catch (RemoteException e) {
			e.printStackTrace();
		}
	}

	protected String getUIComponentID(String var, int columnIndex) {
		return String.valueOf(var + "_" + this.localizationKey[columnIndex] + "-id");
	}


	/**
	 * @return
	 */
	private UIComponent getPrincipalUIComponent(String var) {
		HtmlOutputText t = new HtmlOutputText();
		t.setStyleClass("wf_listtext");
//		t.setStyleClass(ContentConstants.MODULE_PREFIX+"permission_list_text");
		return t;
	}

	/* (non-Javadoc)
	 * @see com.idega.webface.bean.AbstractWFEditableListManagedBean#getNumberOfColumns()
	 */
	@Override
	public int getNumberOfColumns() {
		return 6;
	}


	@Override
	public UIComponent getUIComponent(String var, int columnIndex) {
		int index = columnIndex;
		UIComponent component = null;
		switch (index) {
			case 0:
				component = getPrincipalUIComponent(var);
				break;
			default:
				component = getPrivilegesUIComponent(var);
				break;
		}
		component.setId(getUIComponentID(var, index));
		return component;
	}
	
	private UIComponent getPrivilegesUIComponent(String var) {
		HtmlSelectOneMenu radio = new HtmlSelectOneMenu();		
		radio.setStyleClass(ContentUtil.MODULE_PREFIX+"permission_list_input");
		return radio;
	}

	/* (non-Javadoc)
	 * @see com.idega.webface.bean.AbstractWFEditableListManagedBean#getHeader(int)
	 */
	@Override
	public UIComponent getHeader(int columnIndex) {
		return ContentBlock.getBundle().getLocalizedText(this.localizationKey[columnIndex]);
	}
	
	public boolean isAllowedToWriteACL(){
		try {
			IWContext iwc = IWContext.getInstance();
			IWSlideSession session = (IWSlideSession)IBOLookup.getSessionInstance(iwc,IWSlideSession.class);
			return session.hasPermission(this._resourcePath,IWSlideConstants.PRIVILEGE_WRITE_ACL);
		}
		catch (IBOLookupException e) {
			e.printStackTrace();
		}
		catch (UnavailableIWContext e) {
			e.printStackTrace();
		}
		catch (RemoteException e) {
			e.printStackTrace();
		}   
		return false;
	}
	
}