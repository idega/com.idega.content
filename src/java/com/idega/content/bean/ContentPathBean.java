package com.idega.content.bean;

import java.io.Serializable;

public class ContentPathBean implements Serializable {

	private static final long serialVersionUID = -6231795467918737015L;

	public static final String BEAN_ID = "ContentPathBean";
	
	private String path = null;
	
	public ContentPathBean() {}

	public String getPath() {
		String clone = path;
		path = null;
		return clone;
	}

	public void setPath(String path) {
		this.path = path;
	}
	
}
