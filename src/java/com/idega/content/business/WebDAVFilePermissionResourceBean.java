/*
 * $Id: WebDAVFilePermissionResourceBean.java,v 1.2 2005/01/12 11:46:59 gummi Exp $
 * Created on 30.12.2004
 *
 * Copyright (C) 2004 Idega Software hf. All Rights Reserved.
 *
 * This software is the proprietary information of Idega hf.
 * Use is subject to license terms.
 */
package com.idega.content.business;

import java.io.IOException;
import java.rmi.RemoteException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.ConcurrentModificationException;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;
import org.apache.commons.httpclient.HttpException;
import com.idega.business.IBOSessionBean;
import com.idega.content.data.ACEBean;
import com.idega.slide.business.IWSlideSession;
import com.idega.slide.util.AccessControlEntry;
import com.idega.slide.util.AccessControlList;


/**
 * 
 *  Last modified: $Date: 2005/01/12 11:46:59 $ by $Author: gummi $
 * 
 * @author <a href="mailto:gummi@idega.com">Gudmundur Agust Saemundsson</a>
 * @version $Revision: 1.2 $
 */
public class WebDAVFilePermissionResourceBean extends IBOSessionBean implements WebDAVFilePermissionResource{

	private AccessControlList currentList = null;
	private Collection allACEBeans = null;
	private Collection aceBeansForGroups = null;
	private Collection aceBeansForRoles = null;
	private Collection aceBeansForUsers = null;
	private Collection aceBeansForStandard = null;
	private Collection aceBeansForOthers = null;
	
	
	
	/**
	 * 
	 */
	public WebDAVFilePermissionResourceBean() {
		super();
	}
	
	
	
	public void clear(){
		currentList = null;
		allACEBeans = null;
		aceBeansForGroups = null;
		aceBeansForRoles = null;
		aceBeansForUsers = null;
		aceBeansForStandard = null;
		aceBeansForOthers = null;
	}
	
	/**
	 * @param path
	 */
	protected void updateACEValues() {
		updateACEValues(allACEBeans);	
		updateACEValues(aceBeansForGroups);
		updateACEValues(aceBeansForRoles);
		updateACEValues(aceBeansForUsers);
		updateACEValues(aceBeansForStandard);
		updateACEValues(aceBeansForOthers);
	}
	
	
	
	/**
	 * @param aceBeanCollection
	 */
	protected void updateACEValues(Collection aceBeanCollection) {
		if(aceBeanCollection!=null){
			for (Iterator iter = aceBeanCollection.iterator(); iter.hasNext();) {
				ACEBean bean = (ACEBean) iter.next();
				bean.updateACEs();
			}
		}
	}



	protected AccessControlList getACL(String path) throws HttpException, RemoteException, IOException{
		if(currentList==null){
			IWSlideSession slideSession = (IWSlideSession)getSessionInstance(IWSlideSession.class);
			currentList = slideSession.getAccessControlList(path);
		}
		checkPath(path);
		return currentList;
	}
	
	protected void checkPath(String path){
		if(currentList!=null){ 
			String currentListPath = currentList.getResourcePath();
			if(currentListPath==null && path==null){
				return;
			}
			if(( ((currentListPath==null || path==null)&&currentListPath!=path) || !((currentListPath!=null) && currentListPath.equals(path)) )){
				throw new ConcurrentModificationException("Asking for ACL for path '"+path+"' while current ACL is for '"+currentList.getResourcePath()+"'. The #clear() method needs to be invoked first.");
			}
		}
	}
	
	public Collection getAllAces(String path) throws HttpException, RemoteException, IOException{		
		if(allACEBeans == null){
			AccessControlList acl = getACL(path);
			List l = acl.getAccessControlEntries();
			if(l!=null){
				allACEBeans = getACEBeans(l,path);
			}
		}
		checkPath(path);
		return allACEBeans;
	}
	
	
	public Collection getRoleAces(String path) throws HttpException, RemoteException, IOException{
		if(aceBeansForRoles == null){
			AccessControlList acl = getACL(path);
			List l = acl.getAccessControlEntriesForRoles();
			aceBeansForRoles = getACEBeans(l,path);
		}
		checkPath(path);
		return aceBeansForRoles;
	}
	
	
	public Collection getStandardAces(String path) throws HttpException, RemoteException, IOException {
		if(aceBeansForStandard == null){
			AccessControlList acl = getACL(path);
			List l = acl.getAccessControlEntriesForStandardPrincipals();
			Collection c = new ArrayList();
			c.addAll(getACEBeans(l,path));
			
//			List l2 = acl.getAccessControlEntriesForOthers();
//			c.addAll(getACEBeans(l2,path));
			
			aceBeansForStandard = c;
			
		}
		checkPath(path);
		return aceBeansForStandard;
	}


	public Collection getGroupAces(String path) throws HttpException, RemoteException, IOException {
		if(aceBeansForGroups == null){
			AccessControlList acl = getACL(path);
			List l = acl.getAccessControlEntriesForGroups();
			aceBeansForGroups = getACEBeans(l,path);
		}
		checkPath(path);
		return aceBeansForGroups;
	}


	public Collection getUserAces(String path) throws HttpException, RemoteException, IOException {
		if(aceBeansForUsers == null){
			AccessControlList acl = getACL(path);
			List l = acl.getAccessControlEntriesForUsers();
			aceBeansForUsers = getACEBeans(l,path);
		}
		checkPath(path);
		return aceBeansForUsers;
	}
	
	
	/**
	 * @param l List of <code>AccessControlEntry</code>s
	 */
	protected Collection getACEBeans(List l,String path) {
		Map m = new TreeMap();
		if(l!=null){
			for (Iterator iter = l.iterator(); iter.hasNext();) {
				AccessControlEntry ace = (AccessControlEntry) iter.next();
				ACEBean bean = (ACEBean) m.get(ace.getPrincipal());
				if(bean == null){
					bean = new ACEBean(ace);
					m.put(ace.getPrincipal(),bean);
				} else {
					if(ace.isNegative()){
						bean.setNegativeAccessControlEntry(ace);
					} else {
						bean.setPositiveAccessControlEntry(ace);
					}
				}
			}
		}
		
		Collection c = m.values();
		for (Iterator iter = c.iterator(); iter.hasNext();) {
			ACEBean aBean = (ACEBean) iter.next();
			if(!aBean.hasNegativeAccessControlEntry()){
				AccessControlEntry theOtherAce = aBean.getPositiveAccessControlEntry();
				AccessControlEntry thisAce = new AccessControlEntry(theOtherAce.getPrincipal(),true,false,false,null,theOtherAce.getPrincipalType());
				currentList.add(thisAce);
				aBean.setNegativeAccessControlEntry(thisAce);
			} else if(!aBean.hasPositiveAccessControlEntry()){
				AccessControlEntry theOtherAce = aBean.getNegativeAccessControlEntry();
				AccessControlEntry thisAce = new AccessControlEntry(theOtherAce.getPrincipal(),false,false,false,null,theOtherAce.getPrincipalType());
				currentList.add(thisAce);
				aBean.setPositiveAccessControlEntry(thisAce);
			}
		}
		
		return m.values();
	}
	
	public void store(String path) throws HttpException, RemoteException, IOException{
		if(currentList==null){
			return;
		}
		if(!currentList.getResourcePath().equals(path)){
			throw new ConcurrentModificationException("Asking for ACL for path '"+path+"' while current ACL is for '"+currentList.getResourcePath()+"'. The #clear() method needs to be invoked first.");
		}
		updateACEValues();
		
		AccessControlList acl = getACL(path);
		IWSlideSession slideSession = (IWSlideSession)getSessionInstance(IWSlideSession.class);
		boolean successful = slideSession.storeAccessControlList(acl);
		System.out.println("Save ACL for resource '"+acl.getResourcePath()+((successful)?"' was successful":"' failed"));
		clear();
	}
	
	
}
