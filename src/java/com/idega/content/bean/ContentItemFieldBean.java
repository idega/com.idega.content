/*
 * $Id: ContentItemFieldBean.java,v 1.3 2009/06/22 14:16:49 valdas Exp $
 *
 * Copyright (C) 2004 Idega. All Rights Reserved.
 *
 * This software is the proprietary information of Idega.
 * Use is subject to license terms.
 *
 */
package com.idega.content.bean;

import java.io.Serializable;

/**
 * Bean for idegaWeb content item fields.   
 * <p>
 * Last modified: $Date: 2009/06/22 14:16:49 $ by $Author: valdas $
 *
 * @author Anders Lindman
 * @version $Revision: 1.3 $
 */

public class ContentItemFieldBean implements Serializable, ContentItemField {
	
	private static final long serialVersionUID = 707754626900940615L;
	
	private int _contentItemFieldId = 0;
	private int _versionId = 0;
	private String _key = null;
	private Object _value = null;
	private byte[] _binaryValue = null;
	private int _orderNo = 0;
	private String _fieldType = null;
	
	private String _name = null; // Transient, not stored in database 

	/**
	 * Default constructor.
	 */
	public ContentItemFieldBean() {}
	
	/**
	 * Constructs a new content item field bean with the specified parameters. 
	 */
	public ContentItemFieldBean(
			int contentItemFieldId,
			int versionId,
			String key,
			Object value,
			int orderNo,
			String fieldType) {
		this._contentItemFieldId = contentItemFieldId;
		this._versionId = versionId;
		this._key = key;
		if("create_date".equals(key)){
			System.out.println(this.toString());
			System.out.println(key+": "+value);
		}
		this._value = value;
		this._orderNo = orderNo;
		this._fieldType = fieldType;
	}
		
	public int getContentItemFieldId() { return this._contentItemFieldId; }
	public int getVersionId() { return this._versionId; }
	public String getKey() { return this._key; }
	public Object getValue() {
		return this._value; 
	}
	public byte[] getBinaryValue() { return this._binaryValue; }
	public int getOrderNo() { return this._orderNo; }
	public String getOrderNoString() { return String.valueOf(this._orderNo); }
	public String getFieldType() { return this._fieldType; }

	public void setContentItemFieldId(int id) { this._contentItemFieldId = id; } 
	public void setVersionId(int id) { this._versionId = id; }
	public void setKey(String s) { this._key = s; }
	public void setValue(Object obj) {
		this._value = obj; 
	}
	public void setBinaryValue(byte[] binaryValue) { this._binaryValue = binaryValue; this._fieldType = FIELD_TYPE_BINARY; } 
	public void setOrderNo(int n) { this._orderNo = n; }
	public void setFieldType(String s) { this._fieldType = s; }
	
	public String getImageURI() { return "showimg.jsp?image_number=" + this._orderNo; }
	public String getName() { return this._name; }
	public void setName(String s) { this._name = s; } 
}
