package com.idega.content.themes.helpers;

import java.util.ArrayList;
import java.util.List;

public class ThemeStyleGroupMember {
	
	private String name;
	private String type;
	private String groupName;
	
	private boolean enabled;
	private boolean limitedSelection;

	private List <String> styleFiles;
	
	public ThemeStyleGroupMember() {
		styleFiles = new ArrayList <String> ();
	}
	
	public ThemeStyleGroupMember(ThemeStyleGroupMember parent) {
		this.name = parent.getName();
		this.type = parent.getType();
		this.groupName = parent.getGroupName();
		
		this.enabled = parent.isEnabled();
		this.limitedSelection = parent.isLimitedSelection();
		
		this.styleFiles = new ArrayList <String> (parent.getStyleFiles());
	}

	public boolean isEnabled() {
		return enabled;
	}

	protected void setEnabled(boolean enabled) {
		this.enabled = enabled;
	}

	public String getName() {
		return name;
	}

	protected void setName(String name) {
		this.name = name;
	}

	public List<String> getStyleFiles() {
		return styleFiles;
	}

	protected void addStyleFile(String styleFile) {
		styleFiles.add(styleFile);
	}

	protected String getType() {
		return type;
	}

	protected void setType(String type) {
		this.type = type;
	}

	public boolean isLimitedSelection() {
		return limitedSelection;
	}

	protected void setLimitedSelection(boolean limitedSelection) {
		this.limitedSelection = limitedSelection;
	}

	public String getGroupName() {
		return groupName;
	}

	protected void setGroupName(String groupName) {
		this.groupName = groupName;
	}

}
