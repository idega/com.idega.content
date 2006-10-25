package com.idega.content.themes.helpers;

public class ThemeChange {
	
	private String styleGroupName;
	private String styleGroupMember;
	
	private boolean enabled;
	private boolean limitedSelection;

	public boolean isEnabled() {
		return enabled;
	}

	public void setEnabled(boolean enabled) {
		this.enabled = enabled;
	}

	public String getStyleGroupMember() {
		return styleGroupMember;
	}

	public void setStyleGroupMember(String styleGroupMember) {
		this.styleGroupMember = styleGroupMember;
	}

	public String getStyleGroupName() {
		return styleGroupName;
	}

	public void setStyleGroupName(String styleGroupName) {
		this.styleGroupName = styleGroupName;
	}

	public boolean isLimitedSelection() {
		return limitedSelection;
	}

	public void setLimitedSelection(boolean limitedSelection) {
		this.limitedSelection = limitedSelection;
	}
	
}
