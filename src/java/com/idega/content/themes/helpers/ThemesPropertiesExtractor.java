package com.idega.content.themes.helpers;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import org.jdom.Document;
import org.jdom.Element;

public class ThemesPropertiesExtractor {
	
	private ThemesHelper helper = ThemesHelper.getInstance();
	
	private static final String LIMITED_SELECTION = "1";
	
	private static final int BIG_WIDTH = 800;
	private static final int BIG_HEIGHT = 600;
	
//	private static final int SMALL_WIDTH = 115;
//	private static final int SMALL_HEIGHT = 125;
	
	public synchronized void proceedFileExtractor(ThemeInfo theme) {
		List files = helper.getFiles(theme.getLinkToBaseAsItIs());
		String webRoot = helper.getFullWebRoot();
		String url = helper.getWebRootWithoutContent(webRoot);
		if (!theme.isPropertiesExtracted() && files != null) {
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
			
			if (theme.getLinkToPreview() == null) {
				searchForPreviews(theme, files);
			}
			
			if (theme.getLinkToPreview() == null) { // Big preview
				if (helper.getPreviewGenerator().generatePreview(webRoot + theme.getLinkToSkeleton(), ThemesConstants.PREVIEW_IMAGE, theme.getLinkToBaseAsItIs(), BIG_WIDTH, BIG_HEIGHT)) {
					theme.setLinkToPreview(ThemesConstants.PREVIEW_IMAGE + ThemesConstants.DOT + helper.getPreviewGenerator().getFileType());
				}
			}
			
			if (theme.getLinkToSmallPreview() == null) { // Small preview
//				if (generatePreview(webRoot, theme, ThemesConstants.SMALL_PREVIEW_IMAGE, SMALL_WIDTH, SMALL_HEIGHT)) {
//					theme.setLinkToSmallPreview(ThemesConstants.SMALL_PREVIEW_IMAGE + ThemesConstants.DOT + helper.getPreviewGenerator().getFileType());
//				}
			}
			
			if (theme.getName() == null) {
				theme.setName(helper.getFileName(theme.getLinkToSkeleton()));
			}
			theme.setNewTheme(false);
			
			theme.setPropertiesExtracted(true);
		}
	}
	
	private void searchForPreviews(ThemeInfo theme, List files) {
		if (theme == null || files == null) {
			return;
		}
		String uri = null;
		for (int i = 0; i < files.size(); i++) {
			uri = files.get(i).toString();
			if (ThemesConstants.PREVIEW_IMAGE.equals(helper.getFileName(uri))) {
				theme.setLinkToPreview(helper.getFileNameWithExtension(uri));
				return;
			}
		}
	}
	
	public synchronized void proceedFileExtractor() {
		System.out.println("started file extractor: " + new Date());
		List <ThemeInfo> themes = new ArrayList <ThemeInfo> (helper.getThemesCollection());
		for (int i = 0; i < themes.size(); i++) {
			proceedFileExtractor(themes.get(i));
		}
		System.out.println("finished file extractor: " + new Date());
	}
	
	public void extractProperties(ThemeInfo theme, String link) {
		Document doc = helper.getXMLDocument(link);
		if (doc == null) {
			return;
		}
		Element base = doc.getRootElement().getChild(ThemesConstants.TAG_DICT);
		theme.setName(getValueFromNextElement(ThemesConstants.RW_THEME_NAME, base));
		
		if (extractStyles(theme, ThemesConstants.RW_STYLE_VARIATIONS, base.getChildren())) {
			theme.setPropertiesExtracted(true);
		}
	}
	
	private boolean extractStyles(ThemeInfo theme, String elementSearchKey, List <Element> elements) {
		if (theme == null || elementSearchKey == null || elements == null) {
			return false;
		}
		List <Element> styleGroups = getStyleGroups(elementSearchKey, elements);
		if (styleGroups == null) {
			return false;
		}
		
		Element style = null;
		String styleGroupName = null;
		String selectionLimit = null;
		for (int i = 0; i < styleGroups.size(); i++) {
			style = styleGroups.get(i);
			
			styleGroupName = getValueFromNextElement(ThemesConstants.RW_GROUP_NAME, style);
			theme.addStyleGroupName(styleGroupName);
			
			selectionLimit = getValueFromNextElement(ThemesConstants.RW_SELECTION_LIMIT, style);
			
			if (!extractStyleVariations(theme, styleGroupName, getStyleGroupElements(style), LIMITED_SELECTION.equals(selectionLimit))) {
				return false;
			}

		}
		return true;
	}
	
	private List <Element> getStyleGroups(String elementSearchKey, List <Element> children) {
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
	
	private List <Element> getStyleGroupElements(Element style) {
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
		List <Element> files = styleFiles.getChildren();
		for (int k = 0; k < files.size(); k++) {
			member.addStyleFile(files.get(k).getText());
		}
		return true;
	}
	
	private boolean extractStyleVariations(ThemeInfo theme, String styleGroupName, List <Element> styleVariations, boolean limitedSelection) {
		if (styleVariations == null) {
			return false;
		}
		
		ThemeStyleGroupMember member = null; 
		for (int i = 0; i < styleVariations.size(); i++) {
			Element styleMember = styleVariations.get(i);
			
			member = new ThemeStyleGroupMember();
			member.setName(getValueFromNextElement(ThemesConstants.TAG_NAME, styleMember));
			member.setType(getValueFromNextElement(ThemesConstants.TAG_TYPE, styleMember));
			
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
	
	private int getNextElementIndex(String parentElementValue, List <Element> parentElementChildren) {
		if (parentElementChildren == null) {
			return -1;
		}
		int i = 0;
		boolean foundParentElement = false;
		for (i = 0; (i < parentElementChildren.size() && !foundParentElement); i++) {
			if (parentElementChildren.get(i).getText().equals(parentElementValue)) {
				foundParentElement = true;
			}
		}
		if (foundParentElement && i < parentElementChildren.size()) {
			return i;
		}
		return -1;
	}
	
	private Element getNextElement(String searchKey, List <Element> elements) {
		int index = getNextElementIndex(searchKey, elements);
		if (index == -1) {
			return null;
		}
		return elements.get(index);
	}
	
	private String getValueFromNextElement(String parentElementValue, Element baseElement) {
		String value = ThemesConstants.EMPTY;
		if (baseElement == null) {
			return value;
		}
		List <Element> children = baseElement.getChildren();
		int index = getNextElementIndex(parentElementValue, children);
		if (index == -1) {
			return value;
		}
		return children.get(index).getText();
	}

}
