package com.idega.content.tree;

public class PageTemplate extends Template {
	
	public PageTemplate() {
		super();
	}

	public PageTemplate(String name, String type, String iconFile, String templateFile) {
		this();
		super.setName(name);
		super.setType(type);
		super.setIconFile(iconFile);
		super.setTemplateFile(templateFile);
	}

	
}
