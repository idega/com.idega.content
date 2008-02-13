package com.idega.content.themes.helpers.bean;

import java.util.List;

public class SimplifiedTheme {
	
	private String id = null;
	private String name = null;
	private String linkToSmallPreview = null;
	private String linkToBigPreview = null;
	
	private List<SimplifiedTheme> children = null;
	
	private boolean used = false;
	
	public SimplifiedTheme() {}
	
	public SimplifiedTheme(String id, String name) {
		this();
		
		this.id = id;
		this.name = name;
	}

	public String getId() {
		return id;
	}

	public void setId(String id) {
		this.id = id;
	}

	public String getLinkToBigPreview() {
		return linkToBigPreview;
	}

	public void setLinkToBigPreview(String linkToBigPreview) {
		this.linkToBigPreview = linkToBigPreview;
	}

	public String getLinkToSmallPreview() {
		return linkToSmallPreview;
	}

	public void setLinkToSmallPreview(String linkToSmallPreview) {
		this.linkToSmallPreview = linkToSmallPreview;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public boolean isUsed() {
		return used;
	}

	public void setUsed(boolean used) {
		this.used = used;
	}

	public List<SimplifiedTheme> getChildren() {
		return children;
	}

	public void setChildren(List<SimplifiedTheme> children) {
		this.children = children;
	}

}
