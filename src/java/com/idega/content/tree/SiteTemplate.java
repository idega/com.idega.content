package com.idega.content.tree;

import java.util.ArrayList;

public class SiteTemplate extends Template {
	
	private ArrayList <SiteTemplate> childStructure = new ArrayList <SiteTemplate> ();
	
	public SiteTemplate() {
		super();
	}

	public ArrayList<SiteTemplate> getChildStructure() {
		return childStructure;
	}

	public void setChildStructure(ArrayList<SiteTemplate> childStructure) {
		this.childStructure = childStructure;
	}
	
	public void addChild (SiteTemplate child) {
		childStructure.add(child);
	}

}