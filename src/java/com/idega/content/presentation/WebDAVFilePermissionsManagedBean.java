/*
 * $Id: WebDAVFilePermissionsManagedBean.java,v 1.1 2005/01/07 19:46:49 gummi Exp $ Created
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
import java.util.HashMap;
import javax.faces.component.UIComponent;
import javax.faces.component.UIInput;
import javax.faces.component.UISelectItems;
import javax.faces.component.html.HtmlOutputText;
import javax.faces.component.html.HtmlSelectOneMenu;
import javax.faces.event.AbortProcessingException;
import org.apache.commons.httpclient.HttpException;
import com.idega.business.IBOLookup;
import com.idega.business.IBOLookupException;
import com.idega.content.business.WebDAVFilePermissionResource;
import com.idega.content.data.ACEBean;
import com.idega.presentation.IWContext;
import com.idega.webface.WFUtil;
import com.idega.webface.bean.AbstractWFEditableListManagedBean;

/**
 * 
 * Last modified: $Date: 2005/01/07 19:46:49 $ by $Author: gummi $
 * 
 * @author <a href="mailto:gummi@idega.com">Gudmundur Agust Saemundsson </a>
 * @version $Revision: 1.1 $
 */
public class WebDAVFilePermissionsManagedBean extends AbstractWFEditableListManagedBean {

	protected String[] bindingNames = new String[] { "principalName", "privilegesAll", "privilegesRead",
			"privilegesWrite" };

	protected String[] localizationKey = new String[] { "principal_name", "privileges_all", "privileges_Read",
			"privileges_write" };

	protected int[] bindingNameHashValues = new int[] { hash(bindingNames[0]), hash(bindingNames[1]),
			hash(bindingNames[2]), hash(bindingNames[3]) };

	protected HashMap bindingLocalizationKeyMap;
	
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
		bindingLocalizationKeyMap = new HashMap();
		for (int i = 0; i < bindingNames.length; i++) {
			String binding = bindingNames[i];
			bindingLocalizationKeyMap.put(bindingNames[i], localizationKey[i]);
		}
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
//				System.out.println("["+this.getClass().getName()+"]: getData() return array length is "+((toReturn==null)?-1:toReturn.length));
				return toReturn;
			}
		}
		catch (IBOLookupException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		catch (HttpException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		catch (RemoteException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		System.out.println("["+this.getClass().getName()+"]: getData() return array -> no ACEs found");
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
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		catch (HttpException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		catch (RemoteException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		catch (IOException e) {
			// TODO Auto-generated catch block
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
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		catch (RemoteException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.idega.webface.bean.WFPropertyMatrixManagedBean#getBindingNames()
	 */
	public String[] getBindingNames() {
		return bindingNames;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.idega.webface.bean.WFPropertyMatrixManagedBean#getUIComponent(java.lang.String)
	 */
	public UIComponent getUIComponent(String var, String binding) {
		int bindingHash = hash(binding);
		int i = 0;
		if (bindingHash == bindingNameHashValues[i] && bindingNames[i].equals(binding)) {
			return getPrincipalUIComponent(var);
		}
		i++;
		if (bindingHash == bindingNameHashValues[i] && bindingNames[i].equals(binding)) {
			return getAllPrivilegesUIComponent(var);
		}
		i++;
		if (bindingHash == bindingNameHashValues[i] && bindingNames[i].equals(binding)) {
			return getReadPrivilegesUIComponent(var);
		}
		i++;
		if (bindingHash == bindingNameHashValues[i] && bindingNames[i].equals(binding)) {
			return getWritePrivilegesUIComponent(var);
		}
		i++;
		throw new RuntimeException("[" + this.getClass().getName() + "]: Binding '" + binding + "' is not known.");
	}

	protected String getUIComponentID(String var, String binding) {
		return String.valueOf(hash(var + "." + binding + ".id"));
	}

	/**
	 * @return
	 */
	private UIComponent getWritePrivilegesUIComponent(String var) {
		// TODO Auto-generated method stub
		UIInput radio = new HtmlSelectOneMenu();
		int bindingIndex = 3;
		radio.setId(getUIComponentID(var, bindingNames[bindingIndex]));

//		Locale locale = IWContext.getInstance().getCurrentLocale();
//		String label = ContentBlock.getBundle().getLocalizedString("permission.option."+ACEBean.PRIVLIDGE_VALUE_GRANTED,locale);
//		String label2 = ContentBlock.getBundle().getLocalizedString("permission.option."+ACEBean.PRIVLIDGE_VALUE_DENIED,locale);
//		String label3 = ContentBlock.getBundle().getLocalizedString("permission.option."+ACEBean.PRIVLIDGE_VALUE_NOT_SET,locale);
//		
//		SelectItem item = new SelectItem(ACEBean.PRIVLIDGE_VALUE_GRANTED, label,"Granted", false);
//		SelectItem item2 = new SelectItem(ACEBean.PRIVLIDGE_VALUE_DENIED, label2,"Denied", false);
//		SelectItem item3 = new SelectItem(ACEBean.PRIVLIDGE_VALUE_NOT_SET, label3,"Inherited", false);
//		List l = new ArrayList();
//		l.add(item);
//		l.add(item2);
//		l.add(item3);
		UISelectItems sItems = new UISelectItems();
//		sItems.setValue(l);
		
		String ref = var + ".selectItemListArray["+(bindingIndex)+"]";
		sItems.setValueBinding("value", WFUtil.createValueBinding("#{" + ref + "}"));
		
		radio.getChildren().add(sItems);
		return radio;
	}

	/**
	 * @return
	 */
	private UIComponent getReadPrivilegesUIComponent(String var) {
		UIInput radio = new HtmlSelectOneMenu();
		int bindingIndex = 2;
		radio.setId(getUIComponentID(var, bindingNames[bindingIndex]));
		
		UISelectItems sItems = new UISelectItems();		
		String ref = var + ".selectItemListArray["+(bindingIndex)+"]";
		sItems.setValueBinding("value", WFUtil.createValueBinding("#{" + ref + "}"));
		
		radio.getChildren().add(sItems);
		return radio;
	}

	/**
	 * @return
	 */
	private UIComponent getAllPrivilegesUIComponent(String var) {
		UIInput radio = new HtmlSelectOneMenu();
		int bindingIndex = 1;
		radio.setId(getUIComponentID(var, bindingNames[bindingIndex]));
		
		UISelectItems sItems = new UISelectItems();		
		String ref = var + ".selectItemListArray["+(bindingIndex)+"]";
		sItems.setValueBinding("value", WFUtil.createValueBinding("#{" + ref + "}"));
		
		radio.getChildren().add(sItems);
		return radio;
	}

	/**
	 * @return
	 */
	private UIComponent getPrincipalUIComponent(String var) {
		// TODO Auto-generated method stub
		HtmlOutputText t = new HtmlOutputText();
		t.setStyleClass("wf_listtext");
		t.setId(getUIComponentID(var, bindingNames[0]));
		return t;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see com.idega.webface.bean.WFPropertyMatrixManagedBean#getHeader(java.lang.String)
	 */
	public UIComponent getHeader(String binding) {
		return ContentBlock.getBundle().getLocalizedText(getLocalizationKey(binding));
	}

	public String getLocalizationKey(String binding) {
		return (String) bindingLocalizationKeyMap.get(binding);
	}

	/**
	 * This code is borrowed from java.util.HashMap
	 * 
	 * Returns a hash value for the specified object. In addition to the
	 * object's own hashCode, this method applies a "supplemental hash
	 * function," which defends against poor quality hash functions. This is
	 * critical because HashMap uses power-of two length hash tables.
	 * <p>
	 * 
	 * The shift distances in this function were chosen as the result of an
	 * automated search over the entire four-dimensional search space.
	 */
	protected static int hash(Object x) {
		int h = x.hashCode();
		h += ~(h << 9);
		h ^= (h >>> 14);
		h += (h << 4);
		h ^= (h >>> 10);
		return h;
	}
	
}