package com.idega.content.themes.helpers.bean;

public class SimplifiedTheme {
	
	private String id = null;
	private String name = null;
	private String linkToSmallPreview = null;
	private String linkToBigPreview = null;
	
	private boolean used = false;

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

}
