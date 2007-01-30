package com.idega.content.tree;

import java.util.ArrayList;
import java.util.List;

public class SiteTemplateStructure {

	private String name = null;
	private String type = null;
	private String iconFile = null;
	private String templateFile = null;
	private ArrayList <SiteTemplateStructure> childStructure = new ArrayList <SiteTemplateStructure> ();
	
	public SiteTemplateStructure() {
		super();
	}

//	public SiteTemplateStructure(String name, String type, String iconFile, String templateFile) {
//		super();
//		this.name = name;
//		this.type = type;
//		this.iconFile = iconFile;
//		this.templateFile = templateFile;
//	}

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

	public ArrayList<SiteTemplateStructure> getChildStructure() {
		return childStructure;
	}

	public void setChildStructure(ArrayList<SiteTemplateStructure> childStructure) {
		this.childStructure = childStructure;
	}
	
	public void addChild (SiteTemplateStructure child) {
		childStructure.add(child);
	}

	
}