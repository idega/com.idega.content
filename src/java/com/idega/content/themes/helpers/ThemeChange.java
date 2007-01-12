package com.idega.content.themes.helpers;

public class ThemeChange {
	
	private String themeID;
	private String styleGroupName;
	private String variation;
	private String variationType;
	private boolean enabled;
	
	private boolean limitedSelection;
	
	public ThemeChange() {}
	
	public ThemeChange(String themeID, String styleGroupName, String variation, String variationType, boolean enabled) {
		this.themeID = themeID;
		this.styleGroupName = styleGroupName;
		this.variation = variation;
		this.variationType = variationType;
		this.enabled = enabled;
	}

	protected String getThemeId() {
		return themeID;
	}

	protected void setThemeId(String themeID) {
		this.themeID = themeID;
	}

	protected String getVariationType() {
		return variationType;
	}

	protected void setVariationType(String variationType) {
		this.variationType = variationType;
	}

	protected boolean isEnabled() {
		return enabled;
	}

	protected void setEnabled(boolean enabled) {
		this.enabled = enabled;
	}

	protected String getVariation() {
		return variation;
	}

	protected void setVariation(String variation) {
		this.variation = variation;
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
