package com.idega.content.themes.helpers.bean;

import java.util.ArrayList;
import java.util.List;

public class ThemeStyleGroupMember {
	
	private String name = null;
	private String type = null;
	private String groupName = null;
	private String colour = null;
	private String variable = null;
	
	private boolean stylesheet = true;
	private boolean enabled = false;
	private boolean limitedSelection = false;;

	private List <String> styleFiles = null;
	
	public ThemeStyleGroupMember() {
		styleFiles = new ArrayList <String> ();
	}
	
	public ThemeStyleGroupMember(ThemeStyleGroupMember parent) {
		this.name = parent.getName();
		this.type = parent.getType();
		this.groupName = parent.getGroupName();
		this.colour = parent.getColour();
		this.variable = parent.getVariable();
		
		this.stylesheet = parent.isStylesheet();
		this.enabled = parent.isEnabled();
		this.limitedSelection = parent.isLimitedSelection();
		
		this.styleFiles = new ArrayList<String>(parent.getStyleFiles());
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

	protected String getType() {
		return type;
	}

	public void setType(String type) {
		this.type = type;
	}

	public boolean isLimitedSelection() {
		return limitedSelection;
	}

	public void setLimitedSelection(boolean limitedSelection) {
		this.limitedSelection = limitedSelection;
	}

	public String getGroupName() {
		return groupName;
	}

	public void setGroupName(String groupName) {
		this.groupName = groupName;
	}

	public boolean isStylesheet() {
		return stylesheet;
	}

	public void setStylesheet(boolean stylesheet) {
		this.stylesheet = stylesheet;
	}

	public String getColour() {
		return colour;
	}

	public void setColour(String colour) {
		this.colour = colour;
	}

	public String getVariable() {
		return variable;
	}

	public void setVariable(String variable) {
		this.variable = variable;
	}
	
}
