package com.idega.content.tree;

public class PageTemplate {

	private String name = null;
	private String type = null;
	private String iconFile = null;
	private String templateFile = null;
	
	
	public PageTemplate() {
		super();
	}

	public PageTemplate(String name, String type, String iconFile, String templateFile) {
		super();
		this.name = name;
		this.type = type;
		this.iconFile = iconFile;
		this.templateFile = templateFile;
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

	
}