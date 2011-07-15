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

import java.util.ArrayList;
import java.util.Collection;

import javax.faces.component.UIComponent;
import javax.faces.component.html.HtmlOutputText;
import javax.faces.component.html.HtmlSelectOneMenu;
import javax.faces.event.AbortProcessingException;
import javax.jcr.RepositoryException;
import javax.jcr.Session;
import javax.jcr.security.Privilege;

import com.idega.business.IBOLookup;
import com.idega.business.IBOLookupException;
import com.idega.content.business.ContentUtil;
import com.idega.content.business.WebDAVFilePermissionResource;
import com.idega.core.accesscontrol.business.NotLoggedOnException;
import com.idega.idegaweb.UnavailableIWContext;
import com.idega.presentation.IWContext;
import com.idega.repository.access.RepositoryAccessManager;
import com.idega.repository.access.RepositoryPrivilege;
import com.idega.user.data.bean.User;
import com.idega.util.CoreUtil;
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
		this._resourcePath = path;
		this._resourcePath = getRepository().getPath(path);
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
			WebDAVFilePermissionResource resource = IBOLookup.getSessionInstance(iwc, WebDAVFilePermissionResource.class);
			Collection<? extends WFEditableListDataBean> aces = null;

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

			if (aces != null && aces.size() < this.minRows) {
				Collection<AbstractWFEditableListManagedBean.EmptyRow> empty = new ArrayList<AbstractWFEditableListManagedBean.EmptyRow>();
				AbstractWFEditableListManagedBean.EmptyRow emptyRow = new AbstractWFEditableListManagedBean.EmptyRow();
				for(int i = aces.size(); i <= this.minRows; i++){
					empty.add(emptyRow);
				}
				aces = empty;
			}

			if (aces != null) {
				WFEditableListDataBean[] toReturn =  aces.toArray(new WFEditableListDataBean[aces.size()]);
				return toReturn;
			}
		} catch (IBOLookupException e) {
			e.printStackTrace();
		} catch (RepositoryException e) {
			e.printStackTrace();
		}

		return new WFEditableListDataBean[] {};
	}

	public void saveACL() throws AbortProcessingException {
		IWContext iwc = IWContext.getInstance();
		try {
			WebDAVFilePermissionResource resource = (WebDAVFilePermissionResource) IBOLookup.getSessionInstance(iwc, WebDAVFilePermissionResource.class);
			resource.store(this._resourcePath);
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	public void refresh() throws AbortProcessingException {
		IWContext iwc = IWContext.getInstance();
		try {
			WebDAVFilePermissionResource resource = (WebDAVFilePermissionResource) IBOLookup.getSessionInstance(iwc, WebDAVFilePermissionResource.class);
			resource.clear();
		}
		catch (Exception e) {
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

	@Override
	public UIComponent getHeader(int columnIndex) {
		return ContentBlock.getBundle().getLocalizedText(this.localizationKey[columnIndex]);
	}

	public boolean isAllowedToWriteACL() {
		Session session = null;
		try {
			IWContext iwc = CoreUtil.getIWContext();
			User user = iwc.getLoggedInUser();
			session = getRepository().getSession(user);
			RepositoryAccessManager racm = (RepositoryAccessManager) session.getAccessControlManager();
			Privilege pri = new RepositoryPrivilege(Privilege.JCR_MODIFY_ACCESS_CONTROL);
			return racm.hasPermission(user, this._resourcePath, pri);
		} catch (UnavailableIWContext e) {
			e.printStackTrace();
		} catch (NotLoggedOnException e) {
			e.printStackTrace();
		} catch (RepositoryException e) {
			e.printStackTrace();
		} finally {
			session.logout();
		}
		return Boolean.FALSE;
	}

}