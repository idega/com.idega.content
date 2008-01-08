package com.idega.content.themes.helpers.bean;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import com.idega.builder.bean.AdvancedProperty;

public class Theme {
	
	private boolean propertiesExtracted;
	private boolean locked;
	private boolean newTheme;
	private boolean loading = true;
	
	private String id;
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
	
	private List<String> styleGroupsNames;
	private List<String> styleVariationsCacheKeys;
	private List<String> colourFiles = null;
	private List<String> originalColourFiles = null;
	private List<ThemeChange> changes;
	private List<AdvancedProperty> extraRegions = null;

	private Map<String, ThemeStyleGroupMember> styleGroupsMembers;
	private Map<String, String> styleVariables = null;
	
	private int templateId = -1;

	public Theme(String id) {
		this.styleGroupsNames = new ArrayList<String>();
		this.styleVariationsCacheKeys = new ArrayList<String>();
		this.colourFiles = new ArrayList<String>();
		this.originalColourFiles = new ArrayList<String>();
		this.extraRegions = new ArrayList<AdvancedProperty>();
		this.changes = new ArrayList<ThemeChange>();
		
		this.styleGroupsMembers = new HashMap<String, ThemeStyleGroupMember>();
		this.styleVariables = new HashMap<String, String>();
		
		this.id = id;
	}
	
	public String getLinkToSkeleton() {
		return linkToSkeleton;
	}
	
	public void setLinkToSkeleton(String linkToSkeleton) {
		this.linkToSkeleton = linkToSkeleton;
	}
	
	public String getId() {
		return id;
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
	
	public ThemeStyleGroupMember getStyleGroupsMember(String styleGroupName) {
		if (styleGroupsMembers == null) {
			return null;
		}
		return styleGroupsMembers.get(styleGroupName);
	}

	public void addStyleGroupMember(String styleGroupName, ThemeStyleGroupMember groupMember) {
		styleGroupsMembers.put(styleGroupName, groupMember);
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

	public synchronized void setLocked(boolean locked) {
		this.locked = locked;
	}

	public boolean isNewTheme() {
		return newTheme;
	}

	public void setNewTheme(boolean newTheme) {
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

	public boolean isLoading() {
		return loading;
	}

	public synchronized void setLoading(boolean loading) {
		this.loading = loading;
	}
	
	public int getIBPageID() {
		return templateId;
	}

	public void setIBPageID(int pageID) {
		templateId = pageID;
	}
	
	public void addStyleVariationsCacheKey(String cacheKey) {
		if (styleVariationsCacheKeys.contains(cacheKey)) {
			return;
		}
		styleVariationsCacheKeys.add(cacheKey);
	}
	
	public List<String> getStyleVariationsCacheKeys() {
		return styleVariationsCacheKeys;
	}
	
	public void clearStyleVariationsCacheKeys() {
		styleVariationsCacheKeys.clear();
	}
	
	public void reloadProperties() {
		styleGroupsNames.clear();
		styleVariationsCacheKeys.clear();
		styleGroupsMembers.clear();
		colourFiles.clear();
		originalColourFiles.clear();
		styleVariables.clear();
		extraRegions.clear();
	}

	public List<String> getColourFiles() {
		return colourFiles;
	}

	public void setColourFiles(List<String> colourFiles) {
		this.colourFiles = colourFiles;
	}
	
	public void addColourFile(String colourFile) {
		if (colourFile == null) {
			return;
		}
		
		colourFiles.add(colourFile);
	}
	
	public void addStyleVariable(String variable, String value) {
		if (variable == null || value == null) {
			return;
		}
		
		styleVariables.put(variable, value);
	}
	
	public Map<String, String> getStyleVariables() {
		return styleVariables;
	}
	
	public List<String> getStyleVariablesKeys() {
		return new ArrayList<String>(styleVariables.keySet());
	}
	
	public String getStyleVariableValue(String variable) {
		if (variable == null) {
			return null;
		}
		
		return styleVariables.get(variable);
	}

	public List<String> getOriginalColourFiles() {
		return originalColourFiles;
	}

	public void setOriginalColourFiles(List<String> originalColourFiles) {
		this.originalColourFiles = originalColourFiles;
	}
	
	public boolean hasColourFiles() {
		if (colourFiles == null || colourFiles.size() == 0) {
			return false;
		}
		
		return true;
	}
	
	public void addExtraRegion(String parentRegion, String childRegion) {
		this.extraRegions.add(new AdvancedProperty(parentRegion, childRegion));
	}
	
	public List<AdvancedProperty> getExtraRegions() {
		return extraRegions;
	}
	
	public void setExtraRegions(List<AdvancedProperty> extraRegions) {
		this.extraRegions = extraRegions;
	}

}
