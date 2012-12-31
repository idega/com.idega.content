package com.idega.content.tree;

import java.util.ArrayList;
import java.util.List;

public class SiteTemplate extends Template {
	
	private static final long serialVersionUID = 5950983612728912305L;
	
	private List<SiteTemplate> childStructure = new ArrayList <SiteTemplate> ();
	
	public SiteTemplate() {
		super();
	}

	public List<SiteTemplate> getChildStructure() {
		return childStructure;
	}

	public void setChildStructure(List<SiteTemplate> childStructure) {
		this.childStructure = childStructure;
	}
	
	public void addChild (SiteTemplate child) {
		childStructure.add(child);
	}

}