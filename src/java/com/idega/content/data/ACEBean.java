/*
 * $Id: ACEBean.java,v 1.9 2006/04/09 12:01:55 laddi Exp $
 * Created on 3.1.2005
 *
 * Copyright (C) 2005 Idega Software hf. All Rights Reserved.
 *
 * This software is the proprietary information of Idega hf.
 * Use is subject to license terms.
 */
package com.idega.content.data;

import java.security.Principal;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

import javax.ejb.CreateException;
import javax.ejb.FinderException;
import javax.faces.model.SelectItem;
import javax.jcr.security.Privilege;

import com.idega.content.business.ContentUtil;
import com.idega.content.presentation.ContentBlock;
import com.idega.core.accesscontrol.data.ICRole;
import com.idega.core.accesscontrol.data.ICRoleHome;
import com.idega.data.IDOLookup;
import com.idega.data.IDOLookupException;
import com.idega.data.IDOStoreException;
import com.idega.idegaweb.block.presentation.Builderaware;
import com.idega.presentation.IWContext;
import com.idega.repository.access.AccessControlEntry;
import com.idega.repository.access.RepositoryPrivilege;
import com.idega.util.ArrayUtil;
import com.idega.webface.bean.WFEditableListDataBean;

/**
 *
 *  Last modified: $Date: 2006/04/09 12:01:55 $ by $Author: laddi $
 *
 * @author <a href="mailto:gummi@idega.com">Gudmundur Agust Saemundsson</a>
 * @version $Revision: 1.9 $
 */

public class ACEBean implements WFEditableListDataBean {

	private Principal principal;
	private int principalType;
	private Locale locale;
	private static final int ARRAYINDEX_ALL = 0;
	private static final int ARRAYINDEX_READ = 1;
	private static final int ARRAYINDEX_WRITE = 2;
	private static final int ARRAYINDEX_READ_ACE = 3;
	private static final int ARRAYINDEX_WRITE_ACE = 4;

	private static final String[] PROPERTY_NAMES = new String[] {Privilege.JCR_ALL, Privilege.JCR_READ,
		Privilege.JCR_WRITE, Privilege.JCR_READ_ACCESS_CONTROL, Privilege.JCR_MODIFY_ACCESS_CONTROL};
	private static final Privilege[] PRIVILEGES = new Privilege[] {new RepositoryPrivilege(Privilege.JCR_ALL), new RepositoryPrivilege(Privilege.JCR_READ),
		new RepositoryPrivilege(Privilege.JCR_WRITE), new RepositoryPrivilege(Privilege.JCR_READ_ACCESS_CONTROL),
		new RepositoryPrivilege(Privilege.JCR_MODIFY_ACCESS_CONTROL)};

	private String[] privligeInitialValue;
	private String[] inheritedFrom;
	private boolean[] isInherited;

	private Object[] selectItemArray = null;
	private Object[] columnValueArray = null;

	public static final String PRIVLIDGE_VALUE_GRANTED = "granted";
	public static final String PRIVLIDGE_VALUE_DENIED = "denied";
	public static final String PRIVLIDGE_VALUE_NOT_SET = "not_set";

	private AccessControlEntry grantedACE = null;
	private AccessControlEntry deniedACE = null;

	private ACEBean() {
		super();
		int arraySize = PROPERTY_NAMES.length;
		this.privligeInitialValue = new String[arraySize];
		for (int i = 0; i < PROPERTY_NAMES.length; i++) {
			this.privligeInitialValue[i]=PRIVLIDGE_VALUE_NOT_SET;
		}
		this.inheritedFrom = new String[arraySize];
		this.isInherited = new boolean[arraySize];
	}

	public ACEBean(Locale locale){
		this();
		this.locale = locale;
	}

	public ACEBean(Principal principal, int type, Locale locale){
		this(locale);
		initialize(principal,type);
	}

	public ACEBean(AccessControlEntry ace, Locale locale){
		this(locale);
		initialize(ace);
		initializePrivlidges(ace);
	}

	public ACEBean(AccessControlEntry granted, AccessControlEntry denied, Locale locale){
		this(locale);
		if(granted!=null && denied!=null){
			initialize(granted);
			this.deniedACE = denied;
			if(this.principal.equals(denied.getPrincipal()) && this.principalType==denied.getPrincipalType() && granted.isNegative()!=denied.isNegative()){
				initializePrivlidges(granted);
				initializePrivlidges(denied);
			} else {
				throw new RuntimeException("Both granted and denied AccessControlEntry objects must be for the same principal and one should be negative but not the other.");
			}
		} else if(granted!=null){
			initialize(granted);
			initializePrivlidges(granted);
		} else if(denied!=null){
			initialize(denied);
			initializePrivlidges(denied);
		}else {  //granted and denied is null

		}
	}

	protected void initializePrivlidges(AccessControlEntry ace) {
		Privilege[] privileges = ace.getPrivileges();
		if (ArrayUtil.isEmpty(privileges))
			return;

		for (Privilege privilege: privileges) {
			int arrayIndex = -1;
			if (Privilege.JCR_ALL.equals(privilege.getName())) {
				arrayIndex = ARRAYINDEX_ALL;
			} else if (Privilege.JCR_READ.equals(privilege.getName())) {
				arrayIndex = ARRAYINDEX_READ;
			} else if (Privilege.JCR_WRITE.equals(privilege.getName())) {
				arrayIndex = ARRAYINDEX_WRITE;
			} else if (Privilege.JCR_READ_ACCESS_CONTROL.equals(privilege.getName())) {
				arrayIndex = ARRAYINDEX_READ_ACE;
			} else if (Privilege.JCR_MODIFY_ACCESS_CONTROL.equals(privilege.getName())) {
				arrayIndex = ARRAYINDEX_WRITE_ACE;
			}

			if (arrayIndex > -1) {
				boolean granted = !ace.isNegative();
				this.privligeInitialValue[arrayIndex] = ((granted) ? PRIVLIDGE_VALUE_GRANTED:PRIVLIDGE_VALUE_DENIED);
				if(ace.isInherited()){
					this.isInherited[arrayIndex]=true;
					this.inheritedFrom[arrayIndex]=ace.getInheritedFrom();
				} else {
					this.isInherited[arrayIndex]=false;
				}
			}
		}
	}

	private void swapPrivlidges(AccessControlEntry oldAce, AccessControlEntry newAce) {
		if(!(this.principal.equals(newAce.getPrincipal()) && this.principalType==newAce.getPrincipalType() && oldAce.isNegative()==newAce.isNegative())){
			throw new RuntimeException("The old and new objects must be for the same principal and either both negative or both positive.");
		}

		Privilege[] privileges = oldAce.getPrivileges();
		if (!ArrayUtil.isEmpty(privileges)) {
			for (Privilege privilege: privileges) {
				int arrayIndex = -1;
				if (Privilege.JCR_ALL.equals(privilege.getName())) {
					arrayIndex = ARRAYINDEX_ALL;
				} else if (Privilege.JCR_READ.equals(privilege.getName())) {
					arrayIndex = ARRAYINDEX_READ;
				} else if (Privilege.JCR_WRITE.equals(privilege.getName())) {
					arrayIndex = ARRAYINDEX_WRITE;
				} else if (Privilege.JCR_READ_ACCESS_CONTROL.equals(privilege.getName())) {
					arrayIndex = ARRAYINDEX_READ_ACE;
				} else if (Privilege.JCR_MODIFY_ACCESS_CONTROL.equals(privilege.getName())) {
					arrayIndex = ARRAYINDEX_WRITE_ACE;
				}
				if(arrayIndex>-1){
					this.privligeInitialValue[arrayIndex]=null;
					this.isInherited[arrayIndex]=false;
					this.inheritedFrom[arrayIndex]=null;
				}
			}
		}

		initializePrivlidges(newAce);
	}


	protected void initialize(AccessControlEntry ace){
		boolean granted = !ace.isNegative();
		if(granted){
			this.grantedACE = ace;
		} else {
			this.deniedACE = ace;
		}
		initialize(ace.getPrincipal(), ace.getPrincipalType());
	}

	protected void initialize(Principal principal, int principalType){
		this.principal=principal;
		this.principalType=principalType;
	}

	public void setNegativeAccessControlEntry(AccessControlEntry entry){
		if(!entry.isNegative()){
			throw new RuntimeException("ACE is not negative");
		}
		if(this.deniedACE!=null){
			swapPrivlidges(this.deniedACE,entry);
//			propertySupport.firePropertyChange()
		} else {
			this.deniedACE=entry;
			initializePrivlidges(entry);
		}
	}

	public void setPositiveAccessControlEntry(AccessControlEntry entry){
		if(entry.isNegative()){
			throw new RuntimeException("ACE is negative");
		}
		if(this.grantedACE!=null){
			swapPrivlidges(this.grantedACE,entry);
//			propertySupport.firePropertyChange()
		} else {
			this.grantedACE=entry;
			initializePrivlidges(entry);
		}
	}


	public String getPrincipalName(){
		return this.principal.getName();
	}

	public String getPrincipalDisplayName(){
		switch (this.principalType) {
			case AccessControlEntry.PRINCIPAL_TYPE_STANDARD:
				return ContentUtil.getBundle().getLocalizedString("principal."+this.principal,this.locale);
			case AccessControlEntry.PRINCIPAL_TYPE_ROLE:
				String principal = getPrincipalName();
				String roleKey = principal;
				int lastIndexOfSlash = principal.lastIndexOf('/');
				if(lastIndexOfSlash > -1){
					roleKey = principal.substring(lastIndexOfSlash+1);
				}
				String roleName = roleKey;

				ICRole role = null;
				try {
					role = ((ICRoleHome) IDOLookup.getHome(ICRole.class)).findByPrimaryKey(roleKey);
				} catch (IDOLookupException e) {
					e.printStackTrace();
				} catch (FinderException e) {
					try {
						role = ((ICRoleHome)IDOLookup.getHome(ICRole.class)).create();
						role.setRoleKey(roleKey);
						role.setRoleNameLocalizableKey("repository_role_"+roleKey);
						role.setRoleDescriptionLocalizableKey("repository_role_"+roleKey+"_desc");
						role.store();
					}
					catch (IDOLookupException e1) {
						e1.printStackTrace();
					}
					catch (IDOStoreException e1) {
						e1.printStackTrace();
					}
					catch (CreateException e1) {
						e1.printStackTrace();
					}
				}

				if(role != null) {
					IWContext iwc = IWContext.getInstance();
					roleName = iwc.getIWMainApplication().getBundle(Builderaware.IW_CORE_BUNDLE_IDENTIFIER).getLocalizedString(role.getRoleNameLocalizableKey(),roleName);
				}

				return roleName;
			default:
				break;
		}
		return getPrincipalName();
	}

	public String getPrivilegesAll(){
		return this.privligeInitialValue[ARRAYINDEX_ALL];
	}

	public String getPrivilegesRead(){
		return this.privligeInitialValue[ARRAYINDEX_READ];
	}

	public String getPrivilegesWrite(){
		return this.privligeInitialValue[ARRAYINDEX_WRITE];
	}

	public void setPrivilegesAll(String value){
		System.out.println(getPrincipalName()+".setPrivilegesAll("+value+")");
		this.privligeInitialValue[ARRAYINDEX_ALL]=value;
	}

	public void setPrivilegesRead(String value){
		System.out.println(getPrincipalName()+".setPrivilegesRead("+value+")");
		this.privligeInitialValue[ARRAYINDEX_READ]=value;
	}

	public void setPrivilegesWrite(String value){
		System.out.println(getPrincipalName()+".setPrivilegesRead("+value+")");
		this.privligeInitialValue[ARRAYINDEX_WRITE]=value;
	}

	public void updateACEs() {
		System.out.println("[ACEBean - Update]: "+getPrincipalName());

		Object[] values = getValues();
		for (int i = 0; i < values.length; i++) {
			boolean valueChanged = setColumnValue(i,values[i]);
			if(valueChanged){
				if(PRIVLIDGE_VALUE_GRANTED.equals(values[i])){
					this.grantedACE.setInherited(false);
					this.grantedACE.setInheritedFrom(null);
				} else if(PRIVLIDGE_VALUE_DENIED.equals(values[i])){
					this.deniedACE.setInherited(false);
					this.deniedACE.setInheritedFrom(null);
				}
				else {
					//ace cannot be inherited! (?)
				}
			}
		}

		this.grantedACE.clearPrivileges();
		this.deniedACE.clearPrivileges();

		for (int i = 0; i < this.privligeInitialValue.length; i++) {
			String value = this.privligeInitialValue[i];
			if(PRIVLIDGE_VALUE_GRANTED.equals(value)){
				this.grantedACE.addPrivilege(PRIVILEGES[i]);
				this.isInherited[i] = this.grantedACE.isInherited();
				this.inheritedFrom[i] = this.grantedACE.getInheritedFrom();
			} else if(PRIVLIDGE_VALUE_DENIED.equals(value)){
				this.deniedACE.addPrivilege(PRIVILEGES[i]);
				this.isInherited[i] = this.deniedACE.isInherited();
				this.inheritedFrom[i] = this.deniedACE.getInheritedFrom();
			} else {
				this.isInherited[i] = false;
				this.inheritedFrom[i] = null;
			}
		}
	}


	public boolean hasPositiveAccessControlEntry(){
		return this.grantedACE!=null;
	}

	public boolean hasNegativeAccessControlEntry(){
		return this.deniedACE!=null;
	}

	public AccessControlEntry getPositiveAccessControlEntry(){
		return this.grantedACE;
	}

	public AccessControlEntry getNegativeAccessControlEntry(){
		return this.deniedACE;
	}

	public List<SelectItem> getSelectItemList(int index){
		Locale locale = IWContext.getInstance().getCurrentLocale();
		String label = ContentBlock.getBundle().getLocalizedString("permission.option."+ACEBean.PRIVLIDGE_VALUE_GRANTED,locale);
		String label2 = ContentBlock.getBundle().getLocalizedString("permission.option."+ACEBean.PRIVLIDGE_VALUE_DENIED,locale);

		SelectItem item = new SelectItem(ACEBean.PRIVLIDGE_VALUE_GRANTED, label,"Granted", false);
		SelectItem item2 = new SelectItem(ACEBean.PRIVLIDGE_VALUE_DENIED, label2,"Denied", false);

		List<SelectItem> l = new ArrayList<SelectItem>();
		l.add(item);
		l.add(item2);
		if(!this.isInherited[index]){
			String label3 = ContentBlock.getBundle().getLocalizedString("permission.option."+ACEBean.PRIVLIDGE_VALUE_NOT_SET,locale);
			SelectItem item3 = new SelectItem(ACEBean.PRIVLIDGE_VALUE_NOT_SET, label3,"Inherited", false);
			l.add(item3);
		}
		return l;
	}

	public Object getColumnValue(int index){
		switch (index) {
			case 0:
				return getPrincipalDisplayName();
			default:
				return this.privligeInitialValue[index-1];
		}
	}

	public boolean setColumnValue(int index, Object obj){
		switch (index) {
			case 0:
				return false;
			default:
				boolean toReturn = false;
				if(this.privligeInitialValue[index-1]!=null){
					toReturn = this.privligeInitialValue[index-1].equals(obj);
				} else {
					toReturn = obj != null;
				}
				this.privligeInitialValue[index-1]=((String)obj);
				return toReturn;
		}
	}

	@Override
	public Object[] getSelectItemListArray(){
		if(this.selectItemArray==null){
			//+1 because the first column has no UIInput item
			this.selectItemArray = new Object[PROPERTY_NAMES.length+1];
			for (int i = 0; i < this.selectItemArray.length-1; i++) {
				this.selectItemArray[i+1]=getSelectItemList(i);
			}
		}
		return this.selectItemArray;
	}

	@Override
	public Object[] getValues() {
		if(this.columnValueArray==null){
			this.columnValueArray = new Object[PROPERTY_NAMES.length+1];
			for (int i = 0; i < this.columnValueArray.length; i++) {
				this.columnValueArray[i]=getColumnValue(i);
			}
		}
		return this.columnValueArray;
	}

	public int getPrincipalType() {
		return this.principalType;
	}

	@Override
	public Boolean getRendered() {
		return Boolean.TRUE;
	}
}