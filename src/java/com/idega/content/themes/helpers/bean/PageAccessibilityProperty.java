package com.idega.content.themes.helpers.bean;

import com.idega.builder.bean.AdvancedProperty;

public class PageAccessibilityProperty extends AdvancedProperty {

	private static final long serialVersionUID = -1571667200593514586L;

	private String code = null;
	private String columnName = null;
	private String elementId = null;

	public PageAccessibilityProperty(String id, String value) {
		super(id, value);
	}

	public String getCode() {
		return code;
	}

	public void setCode(String code) {
		this.code = code;
	}

	public String getColumnName() {
		return columnName;
	}

	public void setColumnName(String columnName) {
		this.columnName = columnName;
	}

	public String getElementId() {
		return elementId;
	}

	public void setElementId(String elementId) {
		this.elementId = elementId;
	}

}
