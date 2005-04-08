/*
 * $Id: ACEBean.java,v 1.5 2005/04/08 17:11:54 gummi Exp $
 * Created on 3.1.2005
 *
 * Copyright (C) 2005 Idega Software hf. All Rights Reserved.
 *
 * This software is the proprietary information of Idega hf.
 * Use is subject to license terms.
 */
package com.idega.content.data;

import java.beans.PropertyChangeSupport;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.List;
import java.util.Locale;
import javax.ejb.FinderException;
import javax.faces.model.SelectItem;
import org.apache.webdav.lib.Privilege;
import com.idega.content.business.ContentUtil;
import com.idega.content.presentation.ContentBlock;
import com.idega.core.accesscontrol.data.ICRole;
import com.idega.core.accesscontrol.data.ICRoleHome;
import com.idega.data.IDOLookup;
import com.idega.data.IDOLookupException;
import com.idega.idegaweb.block.presentation.Builderaware;
import com.idega.presentation.IWContext;
import com.idega.slide.util.AccessControlEntry;
import com.idega.slide.util.IWSlideConstants;
import com.idega.webface.bean.WFEditableListDataBean;


/**
 * 
 *  Last modified: $Date: 2005/04/08 17:11:54 $ by $Author: gummi $
 * 
 * @author <a href="mailto:gummi@idega.com">Gudmundur Agust Saemundsson</a>
 * @version $Revision: 1.5 $
 */
public class ACEBean implements WFEditableListDataBean {

	
	private String principal;
	private int principalType;
	private Locale locale;
	private static final int ARRAYINDEX_ALL = 0;
	private static final int ARRAYINDEX_READ = 1;
	private static final int ARRAYINDEX_WRITE = 2;
	private static final int ARRAYINDEX_READ_ACE = 3;
	private static final int ARRAYINDEX_WRITE_ACE = 4;
	private static final String[] PROPERTY_NAMES = new String[] {"all","read","write","read_acl","write_acl"};
	private static final Privilege[] PRIVILEGES = new Privilege[] {IWSlideConstants.PRIVILEGE_ALL,IWSlideConstants.PRIVILEGE_READ,IWSlideConstants.PRIVILEGE_WRITE,IWSlideConstants.PRIVILEGE_READ_ACL,IWSlideConstants.PRIVILEGE_WRITE_ACL};
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
	
    private PropertyChangeSupport propertySupport;
	
	/**
	 * 
	 */
	private ACEBean() {
		super();
		int arraySize = PROPERTY_NAMES.length;
		privligeInitialValue = new String[arraySize];
		for (int i = 0; i < PROPERTY_NAMES.length; i++) {
			privligeInitialValue[i]=PRIVLIDGE_VALUE_NOT_SET;
		}
		inheritedFrom = new String[arraySize];
		isInherited = new boolean[arraySize];
		propertySupport = new PropertyChangeSupport(this);
	}
	
	public ACEBean(Locale locale){
		this();
		this.locale = locale;
	}
	
	public ACEBean(String principal, int type, Locale locale){
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
			deniedACE = denied;
			if(principal.equals(denied.getPrincipal()) && principalType==denied.getPrincipalType() && granted.isNegative()!=denied.isNegative()){
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

	
	
	/**
	 * @param ace
	 */
	protected void initializePrivlidges(AccessControlEntry ace) {
		Enumeration enumAces = ace.enumeratePrivileges();
		while (enumAces.hasMoreElements()) {
			Privilege element = (Privilege) enumAces.nextElement();
			int arrayIndex=-1;
			if(IWSlideConstants.PRIVILEGE_ALL.equals(element)){
				arrayIndex=ARRAYINDEX_ALL;
			} else if(IWSlideConstants.PRIVILEGE_READ.equals(element)){
				arrayIndex=ARRAYINDEX_READ;
			} else if(IWSlideConstants.PRIVILEGE_WRITE.equals(element)){
				arrayIndex=ARRAYINDEX_WRITE;
			} else if(IWSlideConstants.PRIVILEGE_READ_ACL.equals(element)){
				arrayIndex=ARRAYINDEX_READ_ACE;
			} else if(IWSlideConstants.PRIVILEGE_WRITE_ACL.equals(element)){
				arrayIndex=ARRAYINDEX_WRITE_ACE;
			}
			if(arrayIndex>-1){
				boolean granted = !ace.isNegative();
				privligeInitialValue[arrayIndex]=((granted)?PRIVLIDGE_VALUE_GRANTED:PRIVLIDGE_VALUE_DENIED);
				if(ace.isInherited()){
					isInherited[arrayIndex]=true;
					inheritedFrom[arrayIndex]=ace.getInheritedFrom();
				} else {
					isInherited[arrayIndex]=false;
				}
			}
		}
	}

	
	/**
	 * @param ace
	 */
	private void swapPrivlidges(AccessControlEntry oldAce, AccessControlEntry newAce) {
		if(!(principal.equals(newAce.getPrincipal()) && principalType==newAce.getPrincipalType() && oldAce.isNegative()==newAce.isNegative())){
			throw new RuntimeException("The old and new objects must be for the same principal and either both negative or both positive.");
		}
		
		Enumeration enumPriv = oldAce.enumeratePrivileges();
		while (enumPriv.hasMoreElements()) {
			Privilege element = (Privilege) enumPriv.nextElement();
			int arrayIndex=-1;
			if(IWSlideConstants.PRIVILEGE_ALL.equals(element)){
				arrayIndex=ARRAYINDEX_ALL;
			} else if(IWSlideConstants.PRIVILEGE_READ.equals(element)){
				arrayIndex=ARRAYINDEX_READ;
			} else if(IWSlideConstants.PRIVILEGE_WRITE.equals(element)){
				arrayIndex=ARRAYINDEX_WRITE;
			} else if(IWSlideConstants.PRIVILEGE_READ_ACL.equals(element)){
				arrayIndex=ARRAYINDEX_READ_ACE;
			} else if(IWSlideConstants.PRIVILEGE_WRITE_ACL.equals(element)){
				arrayIndex=ARRAYINDEX_WRITE_ACE;
			}
			if(arrayIndex>-1){
				privligeInitialValue[arrayIndex]=null;
				isInherited[arrayIndex]=false;
				inheritedFrom[arrayIndex]=null;
			}
		}
		
		initializePrivlidges(newAce);
	}
	
	
	protected void initialize(AccessControlEntry ace){
		boolean granted = !ace.isNegative();
		if(granted){
			grantedACE = ace;
		} else {
			deniedACE = ace;
		}
		initialize(ace.getPrincipal(), ace.getPrincipalType());
	}
	
	protected void initialize(String principal, int principalType){
		this.principal=principal;
		this.principalType=principalType;
	}
	
	
	/**
	 * @param principal
	 */
	private void setPrincipal(String principal) {
		this.principal=principal;
	}
	
	
	/**
	 * 
	 * @param entry
	 */
	public void setNegativeAccessControlEntry(AccessControlEntry entry){
		if(!entry.isNegative()){
			throw new RuntimeException("ACE is not negative");
		}
		if(deniedACE!=null){
			swapPrivlidges(deniedACE,entry);			
//			propertySupport.firePropertyChange()
		} else {
			deniedACE=entry;
			initializePrivlidges(entry);
		}
	}
	
	/**
	 * 
	 * @param entry
	 */
	public void setPositiveAccessControlEntry(AccessControlEntry entry){
		if(entry.isNegative()){
			throw new RuntimeException("ACE is negative");
		}
		if(grantedACE!=null){
			swapPrivlidges(grantedACE,entry);			
//			propertySupport.firePropertyChange()
		} else {
			grantedACE=entry;
			initializePrivlidges(entry);
		}
	}
	
	
	public String getPrincipalName(){
		return principal;
	}
	
	public String getPrincipalDisplayName(){
		switch (principalType) {
			case AccessControlEntry.PRINCIPAL_TYPE_STANDARD:
				return ContentUtil.getBundle().getLocalizedString("principal."+principal,locale);
			case AccessControlEntry.PRINCIPAL_TYPE_ROLE:
				String principal = getPrincipalName();
				String roleKey = principal;
				int lastIndexOfSlash = principal.lastIndexOf('/');
				if(lastIndexOfSlash > -1){
					roleKey = principal.substring(lastIndexOfSlash+1);
				}
				String roleName = roleKey;
				
				try {
					ICRole role = (ICRole)((ICRoleHome)IDOLookup.getHome(ICRole.class)).findByPrimaryKey(roleName);
					IWContext iwc = IWContext.getInstance();
					roleName = iwc.getIWMainApplication().getBundle(Builderaware.IW_CORE_BUNDLE_IDENTIFIER).getLocalizedString(role.getRoleNameLocalizableKey(),roleName);
				}
				catch (IDOLookupException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
				catch (FinderException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
				
				return roleName;
			default:
				break;
		}
		return getPrincipalName();
	}
	
	public String getPrivilegesAll(){
		return privligeInitialValue[ARRAYINDEX_ALL];
	}
	
	public String getPrivilegesRead(){
		return privligeInitialValue[ARRAYINDEX_READ];
	}
	
	public String getPrivilegesWrite(){
		return privligeInitialValue[ARRAYINDEX_WRITE];
	}
	
	public void setPrivilegesAll(String value){
		System.out.println(getPrincipalName()+".setPrivilegesAll("+value+")");
		privligeInitialValue[ARRAYINDEX_ALL]=value;
	}
	
	public void setPrivilegesRead(String value){
		System.out.println(getPrincipalName()+".setPrivilegesRead("+value+")");
		privligeInitialValue[ARRAYINDEX_READ]=value;
	}
	
	public void setPrivilegesWrite(String value){
		System.out.println(getPrincipalName()+".setPrivilegesRead("+value+")");
		privligeInitialValue[ARRAYINDEX_WRITE]=value;
	}

	/**
	 * 
	 */
	public void updateACEs() {
		System.out.println("[ACEBean - Update]: "+getPrincipalName());	
		
		Object[] values = getValues();
		for (int i = 0; i < values.length; i++) {
			boolean valueChanged = setColumnValue(i,values[i]);
			if(valueChanged){
				if(PRIVLIDGE_VALUE_GRANTED.equals(values[i])){
					grantedACE.setInherited(false);
					grantedACE.setInheritedFrom(null);
				} else if(PRIVLIDGE_VALUE_DENIED.equals(values[i])){
					deniedACE.setInherited(false);
					deniedACE.setInheritedFrom(null);
				} 
				else {
					//ace cannot be inherited! (?)
				}
			}
		}
		
		grantedACE.clearPrivileges();
		deniedACE.clearPrivileges();
		
		for (int i = 0; i < privligeInitialValue.length; i++) {
			String value = privligeInitialValue[i];
			if(PRIVLIDGE_VALUE_GRANTED.equals(value)){
				grantedACE.addPrivilege(PRIVILEGES[i]);
				isInherited[i] = grantedACE.isInherited();
				inheritedFrom[i] = grantedACE.getInheritedFrom();
			} else if(PRIVLIDGE_VALUE_DENIED.equals(value)){
				deniedACE.addPrivilege(PRIVILEGES[i]);
				isInherited[i] = deniedACE.isInherited();
				inheritedFrom[i] = deniedACE.getInheritedFrom();
			} else {
				isInherited[i] = false;
				inheritedFrom[i] = null;
			}
		}
	}
	
	
	public boolean hasPositiveAccessControlEntry(){
		return grantedACE!=null;
	}
	
	public boolean hasNegativeAccessControlEntry(){
		return deniedACE!=null;
	}
	
	public AccessControlEntry getPositiveAccessControlEntry(){
		return grantedACE;
	}
	
	public AccessControlEntry getNegativeAccessControlEntry(){
		return deniedACE;
	}
	
	public List getSelectItemList(int index){
		Locale locale = IWContext.getInstance().getCurrentLocale();
		String label = ContentBlock.getBundle().getLocalizedString("permission.option."+ACEBean.PRIVLIDGE_VALUE_GRANTED,locale);
		String label2 = ContentBlock.getBundle().getLocalizedString("permission.option."+ACEBean.PRIVLIDGE_VALUE_DENIED,locale);
		
		SelectItem item = new SelectItem(ACEBean.PRIVLIDGE_VALUE_GRANTED, label,"Granted", false);
		SelectItem item2 = new SelectItem(ACEBean.PRIVLIDGE_VALUE_DENIED, label2,"Denied", false);
		
		List l = new ArrayList();
		l.add(item);
		l.add(item2);
		if(!isInherited[index]){
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
				return privligeInitialValue[index-1];
		}
	}
	
	/**
	 * 
	 * @param index
	 * @param obj
	 * @return returns true if value has changed, else false
	 */
	public boolean setColumnValue(int index, Object obj){
		switch (index) {
			case 0:
				return false;
			default:
				boolean toReturn = false;
				if(privligeInitialValue[index-1]!=null){
					toReturn = privligeInitialValue[index-1].equals(obj);
				} else {
					toReturn = obj != null;
				}
				privligeInitialValue[index-1]=((String)obj);
				return toReturn;
		}
	}
	
	/* (non-Javadoc)
	 * @see com.idega.webface.bean.WFEditableListDataBean#getSelectItemListArray()
	 */
	public Object[] getSelectItemListArray(){
		if(selectItemArray==null){
			//+1 because the first column has no UIInput item
			selectItemArray = new Object[PROPERTY_NAMES.length+1];
			for (int i = 0; i < selectItemArray.length-1; i++) {
				selectItemArray[i+1]=getSelectItemList(i);
			}
		}
		return selectItemArray;
	}

	/* (non-Javadoc)
	 * @see com.idega.webface.bean.WFEditableListDataBean#getValues()
	 */
	public Object[] getValues() {
		if(columnValueArray==null){
			columnValueArray = new Object[PROPERTY_NAMES.length+1];
			for (int i = 0; i < columnValueArray.length; i++) {
				columnValueArray[i]=getColumnValue(i);
			}
		}
		return columnValueArray;
	}

	
	/**
	 * @return Returns the principalType.
	 */
	public int getPrincipalType() {
		return principalType;
	}

	/* (non-Javadoc)
	 * @see com.idega.webface.bean.WFEditableListDataBean#getRendered()
	 */
	public Boolean getRendered() {
		return Boolean.TRUE;
	}
}
