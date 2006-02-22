/*
 * $Id: MetadataValueBean.java,v 1.6 2006/02/22 21:02:21 laddi Exp $
 * 
 * Copyright (C) 2004 Idega. All Rights Reserved.
 * 
 * This software is the proprietary information of Idega. Use is subject to
 * license terms.
 *  
 */
package com.idega.content.data;

import java.beans.PropertyChangeSupport;
import com.idega.content.presentation.ContentBlock;
import com.idega.webface.bean.WFEditableListDataBean;

/**
 * Last modified: $Date: 2006/02/22 21:02:21 $ by $Author: laddi $
 * Data bean that holds information about metadata type - value pair
 * used to display data in MetadataListManagedBean
 * 
 * @author Joakim Johnson
 * @version $Revision: 1.6 $
 */
public class MetadataValueBean implements WFEditableListDataBean {

	public static final String PROP_TYPE = "type";
	public static final String PROP_VALUES = "values";

	private final static int VALUE_ARRAY_INDEX_TYPE = 0;
	private final static int VALUE_ARRAY_INDEX_VALUES = 1;
	Object[] column = new Object[]{"","","Delete"};

	private PropertyChangeSupport propertySupport;
	public MetadataValueBean() {
		propertySupport = new PropertyChangeSupport(this);
	}

	public MetadataValueBean(String t, String v
			) {
		this();
		setType(t);
		setMetadatavalues(v);
	}
	
	public void setType(String value) {
		String oldValue = (String)column[VALUE_ARRAY_INDEX_TYPE];
		column[VALUE_ARRAY_INDEX_TYPE]=value;
		propertySupport.firePropertyChange(PROP_TYPE, oldValue, value);
	}

	public String getType() {
		return (String)column[VALUE_ARRAY_INDEX_TYPE];
	}

	public String getLocalizedType() {
		String locType = ContentBlock.getBundle().getLocalizedString((String)column[VALUE_ARRAY_INDEX_TYPE]);
		return locType;
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