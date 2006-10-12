package com.idega.content.themes.helpers;

import java.util.ArrayList;
import java.util.List;

public class ThemeStyleGroupMember {
	
	private String name;
	private String type;
	
	private boolean enabled;

	private List <String> styleFiles;
	
	public ThemeStyleGroupMember() {
		styleFiles = new ArrayList <String> ();
	}

	public boolean isEnabled() {
		return enabled;
	}

	public void setEnabled(boolean enabled) {
		this.enabled = enabled;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public List<String> getStyleFiles() {
		return styleFiles;
	}

	public void addStyleFile(String styleFile) {
		styleFiles.add(styleFile);
	}

	public String getType() {
		return type;
	}

	public void setType(String type) {
		this.type = type;
	}
	
}
