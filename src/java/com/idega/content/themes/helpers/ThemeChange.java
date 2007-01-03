package com.idega.content.themes.helpers;

public class ThemeChange {
	
	private String styleGroupName;
	private String styleGroupMember;
	
	private boolean enabled;
	private boolean limitedSelection;

	protected boolean isEnabled() {
		return enabled;
	}

	protected void setEnabled(boolean enabled) {
		this.enabled = enabled;
	}

	protected String getStyleGroupMember() {
		return styleGroupMember;
	}

	protected void setStyleGroupMember(String styleGroupMember) {
		this.styleGroupMember = styleGroupMember;
	}

	protected String getStyleGroupName() {
		return styleGroupName;
	}

	protected void setStyleGroupName(String styleGroupName) {
		this.styleGroupName = styleGroupName;
	}

	protected boolean isLimitedSelection() {
		return limitedSelection;
	}

	protected void setLimitedSelection(boolean limitedSelection) {
		this.limitedSelection = limitedSelection;
	}
	
}
