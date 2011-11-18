package com.idega.content.tree;

import java.io.Serializable;

public class Template implements Serializable {
	
	private static final long serialVersionUID = -2064488146401067511L;
	
	private String name = null;
	private String type = null;
	private String iconFile = null;
	private String templateFile = null;
	
	public Template() {}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getType() {
		return type;
	}

	public void setType(String type) {
		this.type = type;
	}

	public String getIconFile() {
		return iconFile;
	}

	public void setIconFile(String iconFile) {
		this.iconFile = iconFile;
	}

	public String getTemplateFile() {
		return templateFile;
	}

	public void setTemplateFile(String templateFile) {
		this.templateFile = templateFile;
	}

}