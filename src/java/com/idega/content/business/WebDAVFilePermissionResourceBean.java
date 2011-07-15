/*
 * $Id: WebDAVFilePermissionResourceBean.java,v 1.7 2006/04/09 12:01:55 laddi Exp $
 * Created on 30.12.2004
 *
 * Copyright (C) 2004 Idega Software hf. All Rights Reserved.
 *
 * This software is the proprietary information of Idega hf.
 * Use is subject to license terms.
 */
package com.idega.content.business;

import java.security.Principal;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Comparator;
import java.util.ConcurrentModificationException;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.TreeSet;
import java.util.logging.Level;

import javax.jcr.RepositoryException;
import javax.jcr.security.AccessControlException;

import org.apache.jackrabbit.core.security.principal.PrincipalImpl;
import org.springframework.beans.factory.annotation.Autowired;

import com.idega.business.IBOSessionBean;
import com.idega.content.data.ACEBean;
import com.idega.core.accesscontrol.data.bean.ICRole;
import com.idega.jackrabbit.repository.access.JackrabbitAccessControlEntry;
import com.idega.repository.RepositoryConstants;
import com.idega.repository.access.AccessControlEntry;
import com.idega.repository.access.AccessControlList;
import com.idega.repository.authentication.AuthenticationBusiness;
import com.idega.util.ArrayUtil;
import com.idega.util.ListUtil;
import com.idega.util.expression.ELUtil;

/**
 *
 *  Last modified: $Date: 2006/04/09 12:01:55 $ by $Author: laddi $
 *
 * @author <a href="mailto:gummi@idega.com">Gudmundur Agust Saemundsson</a>
 * @version $Revision: 1.7 $
 */
public class WebDAVFilePermissionResourceBean extends IBOSessionBean implements WebDAVFilePermissionResource {

	private AccessControlList currentList = null;
	private Collection<ACEBean> allACEBeans = null;
	private Collection<ACEBean> aceBeansForGroups = null;
	private Collection<ACEBean> aceBeansForRoles = null;
	private Collection<ACEBean> aceBeansForUsers = null;
	private Collection<ACEBean> aceBeansForStandard = null;
	private Collection<ACEBean> aceBeansForOthers = null;

	private Comparator<ACEBean> aceBeanComparator = null;

	private final static int BEANTYPE_ALL = -1;
	private final static int BEANTYPE_STANDARD = 0;
	private final static int BEANTYPE_ROLES = 1;
	private final static int BEANTYPE_GROUPS = 2;
	private final static int BEANTYPE_USERS = 3;

	@Autowired
	private AuthenticationBusiness authBusiness;

	public WebDAVFilePermissionResourceBean() {
		super();
	}

	@Override
	public void clear(){
		this.currentList = null;
		this.allACEBeans = null;
		this.aceBeansForGroups = null;
		this.aceBeansForRoles = null;
		this.aceBeansForUsers = null;
		this.aceBeansForStandard = null;
		this.aceBeansForOthers = null;
	}

	/**
	 * @param path
	 */
	protected void updateACEValues() {
		updateACEValues(this.allACEBeans);
		updateACEValues(this.aceBeansForGroups);
		updateACEValues(this.aceBeansForRoles);
		updateACEValues(this.aceBeansForUsers);
		updateACEValues(this.aceBeansForStandard);
		updateACEValues(this.aceBeansForOthers);
	}

	protected void updateACEValues(Collection<ACEBean> aceBeanCollection) {
		if (ListUtil.isEmpty(aceBeanCollection))
			return;

		for (ACEBean bean: aceBeanCollection) {
			bean.updateACEs();
		}
	}

	protected AccessControlList getACL(String path) throws RepositoryException {
		if (currentList == null) {
			try {
				this.currentList = getRepositorySession(getUserContext()).getAccessControlList(path);
			} catch (RepositoryException e) {
				e.printStackTrace();
			}
		}

		checkPath(path);
		return this.currentList;
	}

	protected void checkPath(String path){
		if(this.currentList!=null){
			String currentListPath = this.currentList.getResourcePath();
			if(currentListPath==null && path==null){
				return;
			}
			if(( ((currentListPath==null || path==null)&&currentListPath!=path) || !((currentListPath!=null) && currentListPath.equals(path)) )){
				throw new ConcurrentModificationException("Asking for ACL for path '"+path+"' while current ACL is for '"+this.currentList.getResourcePath()+
						"'. The #clear() method needs to be invoked first.");
			}
		}
	}

	@Override
	public Collection<ACEBean> getAllAces(String path) throws RepositoryException {
		if (this.allACEBeans == null){
			AccessControlList acl = getACL(path);
			javax.jcr.security.AccessControlEntry[] entries = acl.getAccessControlEntries();
			if(ArrayUtil.isEmpty(entries)){
				this.allACEBeans = getACEBeans(entries, path, BEANTYPE_ALL);
			}
		}
		checkPath(path);
		return this.allACEBeans;
	}


	@Override
	public Collection<ACEBean> getRoleAces(String path) throws RepositoryException {
		if(this.aceBeansForRoles == null){
			AccessControlList acl = getACL(path);
			this.aceBeansForRoles = getACEBeans(acl.getAccessControlEntries(), path, BEANTYPE_ROLES);
		}
		checkPath(path);
		return this.aceBeansForRoles;
	}


	@Override
	public Collection<ACEBean> getStandardAces(String path) throws RepositoryException {
		if (this.aceBeansForStandard == null) {
			AccessControlList acl = getACL(path);
			Collection<ACEBean> c = new ArrayList<ACEBean>();
			c.addAll(getACEBeans(acl.getAccessControlEntries(), path, BEANTYPE_STANDARD));

			this.aceBeansForStandard = c;
		}
		checkPath(path);
		return this.aceBeansForStandard;
	}


	@Override
	public Collection<ACEBean> getGroupAces(String path) throws RepositoryException  {
		if(this.aceBeansForGroups == null){
			AccessControlList acl = getACL(path);
			this.aceBeansForGroups = getACEBeans(acl.getAccessControlEntries(), path, BEANTYPE_GROUPS);
		}
		checkPath(path);
		return this.aceBeansForGroups;
	}


	@Override
	public Collection<ACEBean> getUserAces(String path) throws RepositoryException {
		if(this.aceBeansForUsers == null){
			AccessControlList acl = getACL(path);
			this.aceBeansForUsers = getACEBeans(acl.getAccessControlEntries(), path, BEANTYPE_USERS);
		}
		checkPath(path);
		return this.aceBeansForUsers;
	}

	protected Collection<ACEBean> getACEBeans(javax.jcr.security.AccessControlEntry[] entries, String path,int beantype) throws RepositoryException {
		Map<Principal, ACEBean> m = new HashMap<Principal, ACEBean>();
		if (!ArrayUtil.isEmpty(entries)) {
			for (javax.jcr.security.AccessControlEntry ace: entries) {
				AccessControlEntry iwAce = new JackrabbitAccessControlEntry(ace);

				ACEBean bean = m.get(ace.getPrincipal());
				if(bean == null){
					bean = new ACEBean(iwAce, getUserContext().getCurrentLocale());
					m.put(ace.getPrincipal(),bean);
				} else {
					if(iwAce.isNegative()){
						bean.setNegativeAccessControlEntry(iwAce);
					} else {
						bean.setPositiveAccessControlEntry(iwAce);
					}
				}
			}
		}

		Collection<ACEBean> beanCollection = new TreeSet<ACEBean>(getACEBeanComparator());
		beanCollection.addAll(m.values());

		//add missing AccesscontrolEntry to ACEBeans
		for (Iterator<ACEBean> iter = beanCollection.iterator(); iter.hasNext();) {
			ACEBean aBean = iter.next();
			if(!aBean.hasNegativeAccessControlEntry()){
				AccessControlEntry theOtherAce = aBean.getPositiveAccessControlEntry();

				AccessControlEntry thisAce = new JackrabbitAccessControlEntry(theOtherAce.getPrincipal(),true,false,false,null,theOtherAce.getPrincipalType());
				this.currentList.add(thisAce);
				aBean.setNegativeAccessControlEntry(thisAce);
			} else if(!aBean.hasPositiveAccessControlEntry()){
				AccessControlEntry theOtherAce = aBean.getNegativeAccessControlEntry();
				AccessControlEntry thisAce = new JackrabbitAccessControlEntry(theOtherAce.getPrincipal(),false,false,false,null,theOtherAce.getPrincipalType());
				this.currentList.add(thisAce);
				aBean.setPositiveAccessControlEntry(thisAce);
			}
		}


		//fill with missing aces
		switch (beantype) {
			case BEANTYPE_STANDARD:
				for (String principal: RepositoryConstants.ALL_STANDARD_SUBJECT_URIS) {
					if(!m.containsKey(principal) && !principal.equals(RepositoryConstants.SUBJECT_URI_OWNER) && !principal.equals(RepositoryConstants.SUBJECT_URI_SELF)) {
						addACEBeanSkippingPathCheck(principal, AccessControlEntry.PRINCIPAL_TYPE_STANDARD, beanCollection);
					}
				}
				break;
			case BEANTYPE_ROLES:
				try {
					AuthenticationBusiness authBean = getAuthenticationBusiness();
					String rootRoleURI = authBean.getRoleURI(RepositoryConstants.ROLENAME_ROOT);
					//Remove root role
					if(m.containsKey(rootRoleURI)){
						for (Iterator<ACEBean> iter = beanCollection.iterator(); iter.hasNext();) {
							if (rootRoleURI.equals(iter.next().getPrincipalName())) {
								iter.remove();
							}
						}
					}

					//add all missing roles
					Collection<ICRole> roles = getIWApplicationContext().getIWMainApplication().getAccessController().getAllRoles();
					for (Iterator<ICRole> iter = roles.iterator(); iter.hasNext();) {
						String roleURI = authBean.getRoleURI(iter.next().getRoleKey());
						if(!m.containsKey(roleURI)){
							addACEBeanSkippingPathCheck(roleURI,AccessControlEntry.PRINCIPAL_TYPE_ROLE,beanCollection);
						}
					}

				} catch (RepositoryException e) {
					e.printStackTrace();
				}
				break;
			default:
				break;
		}

		return beanCollection;
	}

	/**
	 * @return
	 */
	private Comparator<ACEBean> getACEBeanComparator() {
		if(this.aceBeanComparator == null){
			this.aceBeanComparator = new ACEBeanComparator(getUserContext().getCurrentLocale());
		}
		return this.aceBeanComparator;
	}



	/**
	 * @param principal
	 * @param principal
	 * @param principalType
	 * @return
	 */
	public ACEBean getAdditionalACEBean(String path, String principal, int principalType) {
		checkPath(path);
		Collection<ACEBean> l = null;
		switch (principalType) {
			case AccessControlEntry.PRINCIPAL_TYPE_STANDARD:
				l = this.aceBeansForStandard;
				break;
			case AccessControlEntry.PRINCIPAL_TYPE_ROLE:
				l = this.aceBeansForStandard;
				break;
			case AccessControlEntry.PRINCIPAL_TYPE_GROUP:
				l = this.aceBeansForGroups;
				break;
			case AccessControlEntry.PRINCIPAL_TYPE_USER:
				l = this.aceBeansForUsers;
				break;
			case AccessControlEntry.PRINCIPAL_TYPE_OTHER:
				l = this.allACEBeans;
				break;
			default:
				break;
		}
		ACEBean aBean = null;
		try {
			aBean = addACEBeanSkippingPathCheck(principal, principalType, l);
		} catch (AccessControlException e) {
			e.printStackTrace();
		} catch (RepositoryException e) {
			e.printStackTrace();
		}
		return aBean;
	}

	/**
	 * @param principal
	 * @param principalType
	 * @return
	 */
	private ACEBean addACEBeanSkippingPathCheck(String principal, int principalType, Collection<ACEBean> aceBeanList) throws AccessControlException, RepositoryException {
		AccessControlEntry negativeAce = new JackrabbitAccessControlEntry(new PrincipalImpl(principal), true, false, false, null, principalType);
		this.currentList.add(negativeAce);
		AccessControlEntry positiveAce = new JackrabbitAccessControlEntry(new PrincipalImpl(principal), false, false, false, null, principalType);
		this.currentList.add(positiveAce);
		ACEBean aBean = new ACEBean(positiveAce,negativeAce,getUserContext().getCurrentLocale());
		if(aceBeanList!=null){
			aceBeanList.add(aBean);
		} else {
			System.err.println("[WARNING]["+this.getClass().getName()+"]: current aceBeanList has not been constructed before attempt to add to it.");
		}
		return aBean;
	}



	@Override
	public void store(String path) throws RepositoryException {
		checkPath(path);
		updateACEValues();

		AccessControlList acl = getACL(path);
		boolean successful = getRepositorySession(getUserContext()).storeAccessControlList(acl);
		log(Level.INFO, "Save ACL for resource '"+acl.getResourcePath()+((successful)?"' was successful":"' failed"));
		clear();
	}

	private AuthenticationBusiness getAuthenticationBusiness() {
		if (authBusiness == null)
			ELUtil.getInstance().autowire(this);
		return authBusiness;
	}
}