package com.idega.content.themes.helpers;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.jdom.Document;
import org.jdom.Element;

public class ThemesPropertiesExtractor {
	
	private ThemesHelper helper = ThemesHelper.getInstance();
	
	private static final String LIMITED_SELECTION = "1";
	
	private static final int BIG_WIDTH = 800;
	private static final int BIG_HEIGHT = 600;
	
	private static final String CSS_EXTENSION = ".css";
	
//	private static final int SMALL_WIDTH = 115;
//	private static final int SMALL_HEIGHT = 125;
	
	public synchronized boolean proceedFileExtractor(Theme theme) {
		List files = helper.getFiles(theme.getLinkToBaseAsItIs());
		if ((theme.isPropertiesExtracted() && !theme.isLoading()) || files == null) {
			return true;
		}
		String webRoot = helper.getFullWebRoot();
		String url = helper.getWebRootWithoutContent(webRoot);
		String linkToProperties = null;
		boolean foundedPropertiesFile = false;
		for (int j = 0; (j < files.size() && !foundedPropertiesFile); j++) {
			linkToProperties = files.get(j).toString();
			for (int k = 0; (k < ThemesConstants.PROPERTIES_FILES.size() && !foundedPropertiesFile); k++) {
				if (helper.isCorrectFile(helper.getFileNameWithExtension(linkToProperties), ThemesConstants.PROPERTIES_FILES.get(k))) {
					foundedPropertiesFile = true;
				}
			}
		}
		
		if (foundedPropertiesFile) { // Extraxting properties and preparing theme, style files for usage
			theme.setLinkToProperties(linkToProperties);
			extractProperties(theme, url + linkToProperties);
			if (theme.isNewTheme()) {
				helper.getThemeChanger().prepareThemeForUsage(theme);
				helper.getThemeChanger().prepareThemeStyleFiles(theme);
			}
		}
		
		String searchName = theme.getName();
		String skeletonName = null;
		if (theme.getLinkToSkeleton().indexOf(ThemesConstants.THEME) != -1) {
			skeletonName = helper.decode(helper.getFileNameWithExtension(theme.getLinkToSkeleton()), true);
			searchName = helper.extractValueFromString(skeletonName, 0, skeletonName.indexOf(ThemesConstants.THEME));
		}
		
		String linkToConfig = null;
		for (int i = 0; (i < files.size() && linkToConfig == null); i++) {
			if (files.get(i).toString().endsWith(searchName + ThemesConstants.IDEGA_THEME_INFO)) {
				linkToConfig = files.get(i).toString();
			}
		}
		if (linkToConfig != null) {
			extractConfiguration(theme, url + linkToConfig);
		}
		
		if (theme.getLinkToThemePreview() == null) {
			searchForPreviews(theme, files);
		}
		
		if (theme.getLinkToThemePreview() == null) { // Big preview
			if (helper.getPreviewGenerator().generatePreview(webRoot + theme.getLinkToSkeleton(), theme.getName() + ThemesConstants.THEME_PREVIEW, theme.getLinkToBaseAsItIs(), BIG_WIDTH, BIG_HEIGHT)) {
				theme.setLinkToThemePreview(theme.getName() + ThemesConstants.THEME_PREVIEW + ThemesConstants.DOT +	helper.getPreviewGenerator().getFileType());
			}
		}
		
		// TODO: small preview = big preview's thumbnail
		
		if (theme.getName() == null) {
			theme.setName(helper.getFileName(theme.getLinkToSkeleton()));
		}
		theme.setNewTheme(false);
		theme.setPropertiesExtracted(true);
		return true;
	}
	
	private void extractConfiguration(Theme theme, String link) {
		Document doc = helper.getXMLDocument(link);
		if (doc == null || theme == null) {
			return;
		}
		disableAllStyles(theme);
		
		Element root = doc.getRootElement();
		Element name = root.getChild(ThemesConstants.CON_NAME);
		theme.setName(name.getTextNormalize());
		
		List styles = root.getChild(ThemesConstants.CON_STYLES).getChildren();
		if (styles == null) {
			return;
		}
		for (int i = 0; i < styles.size(); i++) {
			setEnabledStyles(theme, (Element) styles.get(i));
		}
		
		Element preview = root.getChild(ThemesConstants.CON_PREVIEW);
		theme.setLinkToThemePreview(preview.getTextNormalize());
	}
	
	private void setEnabledStyles(Theme theme, Element style) {
		String styleGroupName = style.getChildTextNormalize(ThemesConstants.CON_GROUP);
		String variation = style.getChildTextNormalize(ThemesConstants.CON_VARIATION);
		ThemeStyleGroupMember member = helper.getThemeChanger().getStyleMember(theme, styleGroupName, variation);
		if (member != null) {
			member.setEnabled(true);
		}
	}
	
	private void disableAllStyles(Theme theme) {
		List <String> groupNames = theme.getStyleGroupsNames();
		ThemeStyleGroupMember member = null;
		String styleGroupName = null;
		Map <String, ThemeStyleGroupMember> styleMembers = theme.getStyleGroupsMembers();
		for (int i = 0; i < groupNames.size(); i++) {
			styleGroupName = groupNames.get(i);
			int j = 0;
			member = styleMembers.get(styleGroupName + ThemesConstants.AT + j);
			while (member != null) {
				member.setEnabled(false);
				j++;
				member = styleMembers.get(styleGroupName + ThemesConstants.AT + j);
			}
		}
	}
	
	private void searchForPreviews(Theme theme, List files) {
		if (theme == null || files == null) {
			return;
		}
		String uri = null;
		for (int i = 0; i < files.size(); i++) {
			uri = files.get(i).toString();
			if ((theme.getName() + ThemesConstants.THEME_PREVIEW).equals(helper.getFileName(uri))) {
				theme.setLinkToThemePreview(helper.getFileNameWithExtension(uri));
				return;
			}
		}
	}
	
	public synchronized boolean proceedFileExtractor() {
		boolean result = true;
		List <Theme> themes = new ArrayList<Theme>(helper.getThemesCollection());
		for (int i = 0; (i < themes.size() && result); i++) {
			result = proceedFileExtractor(themes.get(i));
		}
		return result;
	}
	
	private void extractProperties(Theme theme, String link) {
		Document doc = helper.getXMLDocument(link);
		if (doc == null) {
			return;
		}
		Element base = doc.getRootElement().getChild(ThemesConstants.TAG_DICT);
		theme.setName(getValueFromNextElement(ThemesConstants.RW_THEME_NAME, base));
		
		/*if (extractStyles(theme, ThemesConstants.RW_STYLE_VARIATIONS, base.getChildren())) {
			theme.setPropertiesExtracted(true);
		}*/
		extractStyles(theme, ThemesConstants.RW_STYLE_VARIATIONS, base.getChildren());
	}
	
	private boolean extractStyles(Theme theme, String elementSearchKey, List elements) {
		if (theme == null || elementSearchKey == null || elements == null) {
			return false;
		}
		List styleGroups = getStyleGroups(elementSearchKey, elements);
		if (styleGroups == null) {
			return false;
		}
		
		Element style = null;
		String styleGroupName = null;
		String selectionLimit = null;
		for (int i = 0; i < styleGroups.size(); i++) {
			style = (Element) styleGroups.get(i);
			
			styleGroupName = getValueFromNextElement(ThemesConstants.RW_GROUP_NAME, style);
			theme.addStyleGroupName(styleGroupName);
			
			selectionLimit = getValueFromNextElement(ThemesConstants.RW_SELECTION_LIMIT, style);
			
			if (!extractStyleVariations(theme, styleGroupName, getStyleGroupElements(style), LIMITED_SELECTION.equals(selectionLimit))) {
				return false;
			}

		}
		return true;
	}
	
	private List getStyleGroups(String elementSearchKey, List children) {
		Element styleBaseElement = getNextElement(elementSearchKey, children);
		if (styleBaseElement == null) {
			return null;
		}
		Element styleGroupsBase = styleBaseElement.getChild(ThemesConstants.TAG_ARRAY);
		if (styleGroupsBase == null) {
			return null;
		}
		return styleGroupsBase.getChildren(ThemesConstants.TAG_DICT);
	}
	
	private List getStyleGroupElements(Element style) {
		if (style == null) {
			return null;
		}
		
		Element styleElements = getNextElement(ThemesConstants.RW_GROUP_MEMBERS, style.getChildren());
		
		if (styleElements == null) {
			return null;
		}
		
		return styleElements.getChildren();
	}
	
	private boolean extractStyleVariationFiles(ThemeStyleGroupMember member, Element styleFiles) {
		if (styleFiles == null) {
			return false;
		}
		List files = styleFiles.getChildren();
		String file = null;
		for (int k = 0; k < files.size(); k++) {
			file = ((Element)files.get(k)).getText();
			if (!file.endsWith(CSS_EXTENSION)) { // In Theme.plist sometimes occurs errors, e.g. css file with .png extension
				file = helper.getFileName(file) + CSS_EXTENSION;
			}
			member.addStyleFile(file);
		}
		return true;
	}
	
	private boolean extractStyleVariations(Theme theme, String styleGroupName, List styleVariations, boolean limitedSelection) {
		if (styleVariations == null) {
			return false;
		}
		
		ThemeStyleGroupMember member = null; 
		for (int i = 0; i < styleVariations.size(); i++) {
			Element styleMember = (Element) styleVariations.get(i);
			
			member = new ThemeStyleGroupMember();
			member.setName(getValueFromNextElement(ThemesConstants.TAG_NAME, styleMember));
			member.setType(getValueFromNextElement(ThemesConstants.TAG_TYPE, styleMember));
			member.setGroupName(styleGroupName);
			
			Element enabledValue = getNextElement(ThemesConstants.TAG_ENABLED, styleMember.getChildren());
			if (enabledValue == null) {
				return false;
			}
			if (ThemesConstants.TAG_TRUE.equals(enabledValue.getName())) {
				member.setEnabled(true);
			}
			
			if (!extractStyleVariationFiles(member, getNextElement(ThemesConstants.TAG_FILES, styleMember.getChildren()))) {
				return false;
			}
			
			member.setLimitedSelection(limitedSelection);
			
			theme.addStyleGroupMember(styleGroupName + ThemesConstants.AT + i, member);
		}
		return true;
	}
	
	private int getNextElementIndex(String parentElementValue, List parentElementChildren) {
		if (parentElementChildren == null) {
			return -1;
		}
		int i = 0;
		boolean foundParentElement = false;
		for (i = 0; (i < parentElementChildren.size() && !foundParentElement); i++) {
			if (((Element) parentElementChildren.get(i)).getText().equals(parentElementValue)) {
				foundParentElement = true;
			}
		}
		if (foundParentElement && i < parentElementChildren.size()) {
			return i;
		}
		return -1;
	}
	
	private Element getNextElement(String searchKey, List elements) {
		int index = getNextElementIndex(searchKey, elements);
		if (index == -1) {
			return null;
		}
		return (Element) elements.get(index);
	}
	
	private String getValueFromNextElement(String parentElementValue, Element baseElement) {
		String value = ThemesConstants.EMPTY;
		if (baseElement == null) {
			return value;
		}
		List children = baseElement.getChildren();
		int index = getNextElementIndex(parentElementValue, children);
		if (index == -1) {
			return value;
		}
		return ((Element) children.get(index)).getTextNormalize();
	}

}
