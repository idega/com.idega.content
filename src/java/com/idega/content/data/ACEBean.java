/*
 * $Id: ACEBean.java,v 1.1 2005/01/07 19:46:49 gummi Exp $
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
import javax.faces.model.SelectItem;
import org.apache.webdav.lib.Privilege;
import com.idega.content.presentation.ContentBlock;
import com.idega.presentation.IWContext;
import com.idega.slide.util.AccessControlEntry;
import com.idega.slide.util.IWSlideConstants;


/**
 * 
 *  Last modified: $Date: 2005/01/07 19:46:49 $ by $Author: gummi $
 * 
 * @author <a href="mailto:gummi@idega.com">Gudmundur Agust Saemundsson</a>
 * @version $Revision: 1.1 $
 */
public class ACEBean { // implements WFPropertyMatrixDataBean {

	
	private String principal;
	private int principalType;
	private static final int ARRAYINDEX_ALL = 0;
	private static final int ARRAYINDEX_READ = 1;
	private static final int ARRAYINDEX_WRITE = 2;
	private static final int ARRAYINDEX_READ_ACE = 3;
	private static final int ARRAYINDEX_WRITE_ACE = 4;
	private static final String[] PROPERTY_NAMES = new String[] {"all","read","write","read_acl","write_acl"};
	private static final Privilege[] PRIVILEGES = new Privilege[] {IWSlideConstants.PRIVILEGE_ALL,IWSlideConstants.PRIVILEGE_READ,IWSlideConstants.PRIVILEGE_WRITE,IWSlideConstants.PRIVILEGE_READ,IWSlideConstants.PRIVILEGE_WRITE_ACL};
	private String[] privligeValue;
	private String[] inheritedFrom;
	private boolean[] isInherited;
	
	private Object[] selectItemArray = null;
	
	public static final String PRIVLIDGE_VALUE_GRANTED = "granted";
	public static final String PRIVLIDGE_VALUE_DENIED = "denied";
	public static final String PRIVLIDGE_VALUE_NOT_SET = "not_set";
	
	private AccessControlEntry grantedACE = null;
	private AccessControlEntry deniedACE = null;
	
    private PropertyChangeSupport propertySupport;
	
	/**
	 * 
	 */
	public ACEBean() {
		super();
		int arraySize = PROPERTY_NAMES.length;
		privligeValue = new String[arraySize];
		for (int i = 0; i < PROPERTY_NAMES.length; i++) {
			privligeValue[i]=PRIVLIDGE_VALUE_NOT_SET;
		}
		inheritedFrom = new String[arraySize];
		isInherited = new boolean[arraySize];
		propertySupport = new PropertyChangeSupport(this);
	}
	
	public ACEBean(String principal, int type){
		this();
		initialize(principal,type);
	}
	
	public ACEBean(AccessControlEntry ace){
		this();
		initialize(ace);
		initializePrivlidges(ace);
	}

	public ACEBean(AccessControlEntry granted, AccessControlEntry denied){
		this();
		if(granted!=null && denied!=null){
			initialize(granted);
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
				privligeValue[arrayIndex]=((granted)?PRIVLIDGE_VALUE_GRANTED:PRIVLIDGE_VALUE_DENIED);
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
				privligeValue[arrayIndex]=null;
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
	
	public String getPrivilegesAll(){
		return privligeValue[ARRAYINDEX_ALL];
	}
	
	public String getPrivilegesRead(){
		return privligeValue[ARRAYINDEX_READ];
	}
	
	public String getPrivilegesWrite(){
		return privligeValue[ARRAYINDEX_WRITE];
	}
	
	public void setPrivilegesAll(String value){
		System.out.println(getPrincipalName()+".setPrivilegesAll("+value+")");
		privligeValue[ARRAYINDEX_ALL]=value;
	}
	
	public void setPrivilegesRead(String value){
		System.out.println(getPrincipalName()+".setPrivilegesRead("+value+")");
		privligeValue[ARRAYINDEX_READ]=value;
	}
	
	public void setPrivilegesWrite(String value){
		System.out.println(getPrincipalName()+".setPrivilegesRead("+value+")");
		privligeValue[ARRAYINDEX_WRITE]=value;
	}

	/**
	 * 
	 */
	public void updateACEs() {
		// TODO Auto-generated method stub
		//  Get values from arrays and uppdate granted and denied.
		//	Add ace to session bean if needed.
		System.out.println("[ACEBean - Update]: "+getPrincipalName());	
		
		grantedACE.clearPrivileges();
		deniedACE.clearPrivileges();
		
		for (int i = 0; i < privligeValue.length; i++) {
			String value = privligeValue[i];
			if(PRIVLIDGE_VALUE_GRANTED.equals(value)){
				grantedACE.addPrivilege(PRIVILEGES[i]);
			} else if(PRIVLIDGE_VALUE_DENIED.equals(value)){
				deniedACE.addPrivilege(PRIVILEGES[i]);
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
	 * @see com.idega.content.data.WFPropertyMatrixDataBean#getColumnValue()
	 */
	public Object[] getColumnValue() {
		// TODO Auto-generated method stub
		return null;
	}

	/* (non-Javadoc)
	 * @see com.idega.content.data.WFPropertyMatrixDataBean#getDoRender()
	 */
	public Boolean[] getDoRender() {
		// TODO Auto-generated method stub
		return null;
	}
	
}
