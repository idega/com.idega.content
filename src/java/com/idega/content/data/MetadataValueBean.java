/*
 * $Id: MetadataValueBean.java,v 1.3 2005/01/18 17:44:31 gummi Exp $
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
import com.idega.webface.bean.WFEditableListDataBean;

/**
 * 
 * Last modified: $Date: 2005/01/18 17:44:31 $ by $Author: gummi $
 * 
 * @author Joakim Johnson
 * @version $Revision: 1.3 $
 */
public class MetadataValueBean implements WFEditableListDataBean {

	public static final String PROP_TYPE = "type";
	public static final String PROP_VALUES = "values";

	private final static int VALUE_ARRAY_INDEX_TYPE = 0;
	private final static int VALUE_ARRAY_INDEX_VALUES = 1;
	private final static int VALUE_ARRAY_INDEX_DELETE = 2;
	
//	private Object[] column = new Object[]{"","","Delete"};
	Object[] column = new Object[]{"Categories","Sports, Headline","Delete"};

	private PropertyChangeSupport propertySupport;
	private WebdavExtendedResource me;

	public MetadataValueBean() {
		propertySupport = new PropertyChangeSupport(this);
	}

	public MetadataValueBean(String t, String v, String l) {
		this();
		setType(t);
		setMetadatavalues(v);
		setDelete(l);
//		localizedString = l;//TODO create setter
	}
	
	private void setDelete(String value) {
		String oldValue = (String)column[VALUE_ARRAY_INDEX_DELETE];
		column[VALUE_ARRAY_INDEX_DELETE]=value;
		propertySupport.firePropertyChange(PROP_TYPE, oldValue, value);
	}

	public String getDelete() {
		return (String)column[VALUE_ARRAY_INDEX_DELETE];
	}

	public void setType(String value) {
		String oldValue = (String)column[VALUE_ARRAY_INDEX_TYPE];
		column[VALUE_ARRAY_INDEX_TYPE]=value;
		propertySupport.firePropertyChange(PROP_TYPE, oldValue, value);
	}

	public String getType() {
		return (String)column[VALUE_ARRAY_INDEX_TYPE];
	}

	public void setMetadatavalues(String value) {
		String oldValue = (String)column[VALUE_ARRAY_INDEX_VALUES];
		column[VALUE_ARRAY_INDEX_VALUES]=value;
		propertySupport.firePropertyChange(PROP_TYPE, oldValue, value);
	}

	public String getMetadatavalues() {
		return (String)column[VALUE_ARRAY_INDEX_VALUES];
	}

	/* (non-Javadoc)
	 * @see com.idega.webface.bean.WFEditableListDataBean#getSelectItemListArray()
	 */
	public Object[] getSelectItemListArray() {
		return new Object[column.length];
	}

	public Object[] getValues() {
		return column;
	}

	/* (non-Javadoc)
	 * @see com.idega.webface.bean.WFEditableListDataBean#getRendered()
	 */
	public Boolean getRendered() {
		return Boolean.TRUE;
	}
}