/*
 * $Id: WebDAVFilePermissionsManagedBean.java,v 1.2 2005/01/10 13:52:17 gummi Exp $ Created
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
import java.util.Collection;
import javax.faces.component.UIComponent;
import javax.faces.component.UIInput;
import javax.faces.component.html.HtmlOutputText;
import javax.faces.component.html.HtmlSelectOneMenu;
import javax.faces.event.AbortProcessingException;
import org.apache.commons.httpclient.HttpException;
import com.idega.business.IBOLookup;
import com.idega.business.IBOLookupException;
import com.idega.content.business.WebDAVFilePermissionResource;
import com.idega.content.data.ACEBean;
import com.idega.presentation.IWContext;
import com.idega.webface.bean.AbstractWFEditableListManagedBean;

/**
 * 
 * Last modified: $Date: 2005/01/10 13:52:17 $ by $Author: gummi $
 * 
 * @author <a href="mailto:gummi@idega.com">Gudmundur Agust Saemundsson </a>
 * @version $Revision: 1.2 $
 */
public class WebDAVFilePermissionsManagedBean extends AbstractWFEditableListManagedBean {

	protected String[] componentIDToken = new String[] { "principalName", "privilegesAll", "privilegesRead",
			"privilegesWrite" };

	protected String[] localizationKey = new String[] { "principal_name", "privileges_all", "privileges_Read",
			"privileges_write" };

	
	private int _resourceType = -1;
	public static final int RESOURCE_TYPE_STANDARD = 0;
	public static final int RESOURCE_TYPE_ROLE = 1;
	public static final int RESOURCE_TYPE_GROUP = 2;
	public static final int RESOURCE_TYPE_USER = 3;
	public static final int RESOURCE_TYPE_SHARED = 4;

	private String _resourcePath = "/files/users/Administrator/dropbox";

	private int minRows = 20;
	/**
	 *  
	 */
	public WebDAVFilePermissionsManagedBean() {
		super();
	}

	public void setResourceType(Integer type){
		_resourceType = type.intValue();
	}
	
	public void setResourceType(int type){
		_resourceType = type;
	}
	
	public void setResourcePath(String path){
		_resourcePath = path;
	}
	
	
	/*
	 * (non-Javadoc)
	 * 
	 * @see com.idega.webface.bean.WFPropertyMatrixManagedBean#getData()
	 */
	public Object[] getData() {
		IWContext iwc = IWContext.getInstance();
		try {
			WebDAVFilePermissionResource resource = (WebDAVFilePermissionResource) IBOLookup.getSessionInstance(
					iwc, WebDAVFilePermissionResource.class);
			Collection aces = null;
			
			switch (_resourceType) {
				case RESOURCE_TYPE_STANDARD:
					aces = resource.getStandardAces(_resourcePath);
					break;
				case RESOURCE_TYPE_ROLE:
					aces = resource.getRoleAces(_resourcePath);
					break;
				case RESOURCE_TYPE_GROUP:
					aces = resource.getGroupAces(_resourcePath);
					break;
				case RESOURCE_TYPE_USER:
					aces = resource.getUserAces(_resourcePath);
					break;
				default:
					aces = resource.getAllAces(_resourcePath);
					break;
			}
				
				
			if (aces != null) {
				Object[] toReturn =  aces.toArray(new ACEBean[aces.size()]);
				System.out.println("["+this.getClass().getName()+"]: getData() return array length is "+((toReturn==null)?-1:toReturn.length));
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

		return new Object[] {};
	}
	
	public void saveACL() throws AbortProcessingException {
		IWContext iwc = IWContext.getInstance();
		try {
			WebDAVFilePermissionResource resource = (WebDAVFilePermissionResource) IBOLookup.getSessionInstance(
					iwc, WebDAVFilePermissionResource.class);
			resource.store(_resourcePath);
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

	protected String getUIComponentID(String var, String idToken) {
		return String.valueOf(var + "." + componentIDToken + ".id");
	}


	/**
	 * @return
	 */
	private UIComponent getPrincipalUIComponent(String var) {
		HtmlOutputText t = new HtmlOutputText();
		t.setStyleClass("wf_listtext");
		return t;
	}

	/* (non-Javadoc)
	 * @see com.idega.webface.bean.AbstractWFEditableListManagedBean#getNumberOfColumns()
	 */
	public int getNumberOfColumns() {
		return 4;
	}


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
		component.setId(getUIComponentID(var, componentIDToken[index]));
		return component;
	}
	
	private UIComponent getPrivilegesUIComponent(String var) {
		UIInput radio = new HtmlSelectOneMenu();		
		return radio;
	}

	/* (non-Javadoc)
	 * @see com.idega.webface.bean.AbstractWFEditableListManagedBean#getHeader(int)
	 */
	public UIComponent getHeader(int columnIndex) {
		return ContentBlock.getBundle().getLocalizedText(localizationKey[columnIndex]);
	}
	
}