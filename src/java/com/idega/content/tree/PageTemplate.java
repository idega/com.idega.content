package com.idega.content.tree;

public class PageTemplate {

	private String iconFile = null;
	private String templateFile = null;
	
	
	public PageTemplate() {
		super();
	}

	public PageTemplate(String iconFile, String templateFile) {
		super();
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

	
}
