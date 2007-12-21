package com.idega.content.themes.helpers.bean;

public class ThemeChange {
	
	private String themeId = null;
	private String styleGroupName = null;
	private String variation = null;
	private boolean radio = false;
	private boolean enabled = false;
	
	private boolean limitedSelection;

	public String getThemeId() {
		return themeId;
	}

	public void setThemeId(String themeID) {
		this.themeId = themeID;
	}

	public boolean isRadio() {
		return radio;
	}

	public void setRadio(boolean radio) {
		this.radio = radio;
	}

	public boolean isEnabled() {
		return enabled;
	}

	public void setEnabled(boolean enabled) {
		this.enabled = enabled;
	}

	public String getVariation() {
		return variation;
	}

	public void setVariation(String variation) {
		this.variation = variation;
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
