/*
 * $Id: MetadataValueBean.java,v 1.1 2005/01/10 10:26:01 joakim Exp $
 * 
 * Copyright (C) 2004 Idega. All Rights Reserved.
 * 
 * This software is the proprietary information of Idega. Use is subject to
 * license terms.
 *  
 */
package com.idega.content.data;

import java.beans.PropertyChangeSupport;
import com.idega.slide.util.WebdavExtendedResource;

/**
 * 
 * Last modified: $Date: 2005/01/10 10:26:01 $ by $Author: joakim $
 * 
 * @author Joakim Johnson
 * @version $Revision: 1.1 $
 */
public class MetadataValueBean {

	public static final String PROP_ID = "id";
	public static final String PROP_TYPE = "type";
	public static final String PROP_VALUES = "values";

	private int id;
	private String type = "";
	private String values = "";

	private PropertyChangeSupport propertySupport;
	private WebdavExtendedResource me;

	public MetadataValueBean() {
		propertySupport = new PropertyChangeSupport(this);
		setId((int) (Math.random() * 1000));
	}

	public MetadataValueBean(String t, String v) {
		this();
		setType(t);
		setValues(v);
	}

	public int getId() {
		return id;
	}

	public void setId(int value) {
		int oldValue = id;
		id = value;
		propertySupport.firePropertyChange(PROP_ID, oldValue, id);
	}

	public String getType() {
		return type;
	}

	public void setType(String value) {
		String oldValue = type;
		type = value;
		propertySupport.firePropertyChange(PROP_TYPE, oldValue, type);
	}

	public String getValues() {
		return values;
	}

	public void setValues(String v) {
		String oldValue = values;
		values = v;
		propertySupport.firePropertyChange(PROP_VALUES, oldValue, values);
	}

//	private WebdavExtendedResource getMe() {
//		if (me == null) {
//			try {
//				IWContext iwc = IWContext.getInstance();
//				IWSlideSession ss = (IWSlideSession) IBOLookup.getSessionInstance(iwc, IWSlideSession.class);
//				WebdavExtendedResource resource = ss.getWebdavResource(getWebDavUrl().replaceFirst(
//						ss.getWebdavServerURI(), ""));
//				this.me = resource;
//			}
//			catch (Exception e) {
//				//				e.printStackTrace();
//				setName("Error getting resource (" + getWebDavUrl() + ")");
//			}
//		}
//		return me;
//	}
}