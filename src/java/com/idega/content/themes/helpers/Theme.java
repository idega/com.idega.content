package com.idega.content.themes.helpers;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;


public class Theme {
	
	private boolean propertiesExtracted;
	private boolean locked;
	private boolean newTheme;
	
	private String themeId;
	private String linkToSkeleton;
	private String linkToDraft;
	private String linkToProperties;
	private String linkToBase;
	private String linkToBaseAsItIs;
	private String linkToThemePreview;
	private String linkToDraftPreview;
	private String linkToSmallPreview;
	
	private String name;
	private String changedName;
	
	private List <String> styleGroupsNames;
	private List <ThemeChange> changes;
	private Map <String, ThemeStyleGroupMember> styleGroupsMembers;
	
	public Theme(String themeId) {
		styleGroupsNames = new ArrayList<String>();
		changes = new ArrayList<ThemeChange>();
		styleGroupsMembers = new HashMap<String, ThemeStyleGroupMember>();
		this.themeId = themeId;
	}
	
	public String getLinkToSkeleton() {
		return linkToSkeleton;
	}
	
	public void setLinkToSkeleton(String linkToSkeleton) {
		this.linkToSkeleton = linkToSkeleton;
	}
	
	public String getThemeId() {
		return themeId;
	}

	public String getLinkToProperties() {
		return linkToProperties;
	}

	public void setLinkToProperties(String linkToProperties) {
		this.linkToProperties = linkToProperties;
	}

	public String getLinkToBase() {
		return linkToBase;
	}

	public void setLinkToBase(String themeBase) {
		this.linkToBase = themeBase;
	}

	public boolean isPropertiesExtracted() {
		return propertiesExtracted;
	}

	public void setPropertiesExtracted(boolean propertiesExtracted) {
		this.propertiesExtracted = propertiesExtracted;
	}

	public String getLinkToThemePreview() {
		return linkToThemePreview;
	}

	public void setLinkToThemePreview(String linkToImage) {
		this.linkToThemePreview = linkToImage;
	}

	public String getName() {
		return name;
	}

	public void setName(String themeName) {
		this.name = themeName;
	}

	public List<String> getStyleGroupsNames() {
		return styleGroupsNames;
	}

	public void addStyleGroupName(String styleGroupName) {
		this.styleGroupsNames.add(styleGroupName);
	}

	public Map<String, ThemeStyleGroupMember> getStyleGroupsMembers() {
		return styleGroupsMembers;
	}

	public void addStyleGroupMember(String groupName, ThemeStyleGroupMember groupMember) {
		styleGroupsMembers.put(groupName, groupMember);
	}

	public String getLinkToDraft() {
		return linkToDraft;
	}

	public void setLinkToDraft(String linkToDraft) {
		this.linkToDraft = linkToDraft;
	}

	public boolean isLocked() {
		return locked;
	}

	protected synchronized void setLocked(boolean locked) {
		this.locked = locked;
	}

	protected boolean isNewTheme() {
		return newTheme;
	}

	protected void setNewTheme(boolean newTheme) {
		this.newTheme = newTheme;
	}

	public String getLinkToSmallPreview() {
		return linkToSmallPreview;
	}

	public void setLinkToSmallPreview(String linkToSmallPreview) {
		this.linkToSmallPreview = linkToSmallPreview;
	}

	public String getLinkToBaseAsItIs() {
		return linkToBaseAsItIs;
	}

	public void setLinkToBaseAsItIs(String linkToBaseAsItIs) {
		this.linkToBaseAsItIs = linkToBaseAsItIs;
	}

	public String getChangedName() {
		return changedName;
	}

	public void setChangedName(String changedName) {
		this.changedName = changedName;
	}

	public List<ThemeChange> getChanges() {
		return changes;
	}

	public void setChanges(List<ThemeChange> changes) {
		this.changes = changes;
	}
	
	public void addThemeChange(ThemeChange change) {
		changes.add(0, change);
	}

	public String getLinkToDraftPreview() {
		return linkToDraftPreview;
	}

	public void setLinkToDraftPreview(String linkToDraftPreview) {
		this.linkToDraftPreview = linkToDraftPreview;
	}
	
}