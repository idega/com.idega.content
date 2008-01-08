package com.idega.content.themes.helpers.business;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.jdom.Document;
import org.jdom.Element;

import com.idega.content.business.ContentConstants;
import com.idega.content.themes.helpers.bean.Theme;
import com.idega.content.themes.helpers.bean.ThemeStyleGroupMember;
import com.idega.util.CoreConstants;
import com.idega.util.StringHandler;

public class ThemesPropertiesExtractorBean implements ThemesPropertiesExtractor {

	private ThemesHelper helper = ThemesHelper.getInstance();
	
	private static final String LIMITED_SELECTION = "1";
	private static final String CSS_EXTENSION = ".css";
	private static final String PNG_EXTENSION = ".png";
	
	private static final Log log = LogFactory.getLog(ThemesPropertiesExtractor.class);
	
	public void prepareThemes(List<String> pLists, List<String> configs, boolean useThread) {
		//	Initializing ImageGenerator
		helper.getImageGenerator(null);
		
		//	Firstly getting not prepared themes
		List<Theme> themesToPrepare = getUnPreparedThemes();
		
		//	Preparing new theme(s)
		Theme theme = null;
		for (int i = 0; i < themesToPrepare.size(); i++) {
			theme = themesToPrepare.get(i);
			if (!prepareTheme(theme, pLists, configs, useThread)) {
				markThemeAsNotPrepared(theme);
			}
		}
	}
	
	private void markThemeAsNotPrepared(Theme theme) {
		if (theme == null) {
			return;
		}
		
		theme.setLoading(false);
		theme.setNewTheme(true);
		theme.setPropertiesExtracted(false);
	}
	
	private synchronized List<Theme> getUnPreparedThemes() {
		List<Theme> newThemes = new ArrayList<Theme>();
		List <Theme> themes = new ArrayList<Theme>(helper.getAllThemes());
		if (themes == null) {
			return newThemes;
		}
		
		Theme theme = null;
		for (int i = 0; i < themes.size(); i++) {
			theme = themes.get(i);
			//	Checking if it is possible to extract properties
			if (!theme.isLoading() && !theme.isPropertiesExtracted()) {
				theme.setLoading(true);
				newThemes.add(theme);
			}
		}
		return newThemes;
	}
	
	private boolean prepareTheme(Theme theme, List<String> pLists, List<String> configs, boolean useThread) {
		if (useThread) {
			ThemePropertiesExtractor extractor = new ThemePropertiesExtractor(theme, helper.getThemesPropertiesExtractor(), pLists, configs);
			extractor.start();
			return true;
		}
		
		return prepareTheme(true, theme, pLists, configs);
	}
	
	private String getPropertiesFile(Theme theme, List<String> pLists) {
		if (pLists == null) {
			return null;
		}
		String pList = null;
		for (int i = 0; i < pLists.size(); i++) {
			pList = pLists.get(i);
			if (pList.indexOf(theme.getLinkToBaseAsItIs()) != -1) {
				return pList;
			}
		}
		return null;
	}
	
	private String getConfigFile(Theme theme, String searchName, List<String> configs) {
		if (configs == null) {
			return null;
		}
		String config = null;
		searchName = new StringBuffer(searchName).append(ThemesConstants.IDEGA_THEME_INFO).toString();
		for (int i = 0; i < configs.size(); i++) {
			config = configs.get(i);
			if (config.indexOf(theme.getLinkToBaseAsItIs()) != -1) {
				if (config.endsWith(searchName)) {
					return config;
				}
			}
		}
		return null;
	}
	
	public boolean prepareTheme(boolean checkConfigFile, Theme theme, List<String> pLists, List<String> configs) {
		if (theme == null) {
			return false;
		}
		
		String webRoot = helper.getFullWebRoot();
		String url = helper.getWebRootWithoutContent(webRoot);
		String linkToProperties = null;
		String searchResult = null;
		boolean foundedPropertiesFile = false;
		
		//	Looking for properties file
		linkToProperties = getPropertiesFile(theme, pLists);
		if (linkToProperties != null) {
			foundedPropertiesFile = true;
		}
		if (!foundedPropertiesFile) {
			for (int i = 0; (i < ThemesConstants.THEME_PROPERTIES_FILES.size() && !foundedPropertiesFile); i++) {
				linkToProperties = new StringBuffer(ThemesConstants.THEME_PROPERTIES_FILES.get(i)).toString();
				searchResult = existsFile(theme, linkToProperties);
				if (searchResult != null) {
					if (helper.isCorrectFile(helper.getFileNameWithExtension(searchResult), ThemesConstants.THEME_PROPERTIES_FILES.get(i))) {
						foundedPropertiesFile = true;
					}
				}
			}
			linkToProperties = searchResult;
			searchResult = null;
		}
		
		// Extracting properties and preparing theme, style files for usage
		if (foundedPropertiesFile) {
			if (linkToProperties.indexOf(CoreConstants.SPACE) != -1) {
				linkToProperties = helper.urlEncode(linkToProperties);
			}
			theme.setLinkToProperties(linkToProperties);
			if (!extractProperties(theme, new StringBuffer(url).append(linkToProperties).toString())) {
				markThemeAsNotPrepared(theme);
				return false;
			}
		}
		
		// Setting default theme if it does not exit
		if (theme.getName() == null) {
			theme.setName(helper.getFileName(theme.getLinkToSkeleton()));
		}
		
		String linkToConfig = null;
		if (checkConfigFile) {
			//	Getting theme name, which will be used to search for configuration file
			String searchName = StringHandler.removeCharacters(theme.getName(), ContentConstants.SPACE, ContentConstants.UNDER);
			String skeletonName = null;
			if (theme.getLinkToSkeleton().indexOf(ThemesConstants.THEME) != -1) {
				skeletonName = helper.decode(helper.getFileNameWithExtension(theme.getLinkToSkeleton()), true);
				searchName = helper.extractValueFromString(skeletonName, 0, skeletonName.indexOf(ThemesConstants.THEME));
			}
			
			//	Trying to get configuration file from list
			linkToConfig = getConfigFile(theme, searchName, configs);
			
			if (linkToConfig == null) {
				theme.setNewTheme(true);	//	Every theme must have configuration file, if don't - treat theme as new theme
			}
		}
		
		if (theme.isNewTheme()) {
			theme.setNewTheme(true);
			ThemeChanger changer = helper.getThemeChanger();
			try {
				if (!(changer.prepareThemeStyleFiles(theme))) {	//	Checking and preparing CSS files
					markThemeAsNotPrepared(theme);
					return false;
				}
				if (!(changer.prepareThemeForUsage(theme))) {	//	Preparing skeleton
					markThemeAsNotPrepared(theme);
					return false;
				}
				if (!(helper.getThemesService().createIBPage(theme))) {
					markThemeAsNotPrepared(theme);
					return false;
				}
			} catch (Exception e) {
				e.printStackTrace();
				markThemeAsNotPrepared(theme);
				return false;
			}
		}
			
		if (checkConfigFile && linkToConfig != null) {
			// Extracting configuration
			if (linkToConfig.indexOf(CoreConstants.SPACE) != -1) {
				linkToConfig = helper.urlEncode(linkToConfig);
			}
			if (!extractConfiguration(theme, new StringBuffer(url).append(linkToConfig).toString())) {
				markThemeAsNotPrepared(theme);
				return false;
			}
		}
		
		//	Checking previews
		if (theme.getLinkToThemePreview() == null || theme.getLinkToSmallPreview() == null) {
			//	And creating if don't exist
			if (!helper.generatePreviewsForTheme(theme, false, ThemesConstants.IS_THEME_PREVIEW_JPG, ThemesConstants.THEME_PREVIEW_QUALITY, false)) {
				markThemeAsNotPrepared(theme);
				return false;
			}
		}
		
		// Creating configuration file
		if (linkToConfig == null && checkConfigFile) {
			if (!helper.createThemeConfig(theme)) {
				markThemeAsNotPrepared(theme);
				return false;
			}
		}
		
		// Finishing theme
		theme.setNewTheme(false);
		theme.setPropertiesExtracted(true);
		theme.setLoading(false);
		helper.addLoadedTheme(theme.getId());
		
		return true;
	}
	
	@SuppressWarnings("unchecked")
	private boolean extractProperties(Theme theme, String link) {
		Document doc = helper.getXMLDocument(link);
		if (doc == null) {
			return false;
		}
		Element base = doc.getRootElement().getChild(ThemesConstants.TAG_DICT);
		theme.setName(getValueFromNextElement(ThemesConstants.RW_THEME_NAME, base));
		
		if (!(extractColourFiles(theme, ThemesConstants.RW_COLOUR_TAG_FILES, base))) {
			return false;
		}
		
		return extractStyles(theme, ThemesConstants.RW_STYLE_VARIATIONS, base.getChildren());
	}
	
	@SuppressWarnings("unchecked")
	private boolean extractColourFiles(Theme theme, String tagValue, Element root) {
		if (theme == null || root == null) {
			return false;
		}
		
		Element coloursTag = getElementByValue(tagValue, root.getChildren());
		if (coloursTag == null) {
			return true;
		}
		
		Element array = getNextElement(coloursTag, root.getChildren());
		if (array == null) {
			return true;
		}
		
		List<Element> arrayChildren = array.getChildren();
		if (arrayChildren == null) {
			return true; 
		}
		
		Element e = null;
		for (int i = 0; i < arrayChildren.size(); i++) {
			e = arrayChildren.get(i);
			
			if (ThemesConstants.TAG_STRING.equals(e.getName())) {
				theme.addColourFile(e.getText());
			}
		}
		
		return true;
	}
	
	private Element getElementByValue(String value, List<Element> elements) {
		if (value == null || elements == null) {
			return null;
		}
		
		Element e = null;
		for (int i = 0; (i < elements.size() && e == null); i++) {
			e = elements.get(i);
			
			if (!(value.equals(e.getText()))) {
				e = null;
			}
		}
		
		return e;
	}
	
	private boolean extractStyles(Theme theme, String elementSearchKey, List<Element> elements) {
		if (theme == null || elementSearchKey == null || elements == null) {
			return false;
		}
		List<Element> styleGroups = getStyleGroups(elementSearchKey, elements);
		if (styleGroups == null) {
			return false;
		}
		
		Element style = null;
		String styleGroupName = null;
		String selectionLimit = null;
		for (int i = 0; i < styleGroups.size(); i++) {
			style = styleGroups.get(i);
			styleGroupName = getValueFromNextElement(ThemesConstants.RW_GROUP_NAME, style);
			selectionLimit = getValueFromNextElement(ThemesConstants.RW_SELECTION_LIMIT, style);
			extractStyleVariations(theme, styleGroupName, getStyleGroupElements(style), LIMITED_SELECTION.equals(selectionLimit));
			if (theme.getStyleGroupsMember(new StringBuffer(styleGroupName).append(ThemesConstants.AT).append(0).toString()) == null) {
				log.info(new StringBuffer("Style group '").append(styleGroupName).append("' has no CSS file! Disabling this group."));
			}
			else {
				theme.addStyleGroupName(styleGroupName);
			}

		}
		return true;
	}
	
	@SuppressWarnings("unchecked")
	private String getValueFromNextElement(String parentElementValue, Element baseElement) {
		String value = ThemesConstants.EMPTY;
		if (baseElement == null) {
			return value;
		}
		List<Element> children = baseElement.getChildren();
		int index = getNextElementIndex(parentElementValue, children);
		if (index == -1) {
			return value;
		}
		return (children.get(index)).getTextNormalize();
	}
	
	private int getNextElementIndex(String parentElementValue, List<Element> parentElementChildren) {
		if (parentElementChildren == null) {
			return -1;
		}
		int i = 0;
		boolean foundParentElement = false;
		for (i = 0; (i < parentElementChildren.size() && !foundParentElement); i++) {
			if ((parentElementChildren.get(i)).getText().equals(parentElementValue)) {
				foundParentElement = true;
			}
		}
		if (foundParentElement && i < parentElementChildren.size()) {
			return i;
		}
		return -1;
	}
	
	private Element getNextElement(String searchKey, List<Element> elements) {
		int index = getNextElementIndex(searchKey, elements);
		if (index == -1) {
			return null;
		}
		return elements.get(index);
	}
	
	private Element getNextElement(Element element, List<Element> elements) {
		if (element == null || elements == null) {
			return null;
		}
		
		Element e = null;
		for (int i = 0; i < elements.size(); i++) {
			e = elements.get(i);
			if (e.equals(element) && ((i + 1) < elements.size())) {
				return elements.get(i + 1);
			}
		}
		
		return null;
	}
	
	@SuppressWarnings("unchecked")
	private boolean extractStyleVariations(Theme theme, String styleGroupName, List<Element> styleVariations, boolean limitedSelection) {
		if (styleVariations == null) {
			return false;
		}
		
		ThemeStyleGroupMember member = null;
		int styleGroupMemberIndex = 0;
		Element styleMember = null;
		String styleVariationType = null;
		boolean canAddVariation = false;
		for (int i = 0; i < styleVariations.size(); i++) {
			styleMember = styleVariations.get(i);
			
			canAddVariation = false;
			styleVariationType = getValueFromNextElement(ThemesConstants.TAG_TYPE, styleMember);
			
			member = new ThemeStyleGroupMember();
			member.setName(getValueFromNextElement(ThemesConstants.TAG_NAME, styleMember));
			member.setType(styleVariationType);
			member.setGroupName(styleGroupName);
			
			if (ThemesConstants.RW_STYLE_VARIATION_TYPE_COLOUR.equals(styleVariationType)) {
				member.setStylesheet(false);
				
				member.setColour(getValueFromNextElement(ThemesConstants.TAG_DEFAULT_COLOUR, styleMember));
				String variable = getValueFromNextElement(ThemesConstants.TAG_TAG, styleMember);
				if (variable != null) {
					variable = variable.replace(CoreConstants.PERCENT, CoreConstants.EMPTY);
					
					member.setVariable(variable);
					member.setEnabled(true);
					
					theme.addStyleVariable(variable, member.getColour());
					
					canAddVariation = true;
				}
			}
			else {
				Element enabledValue = getNextElement(ThemesConstants.TAG_ENABLED, styleMember.getChildren());
				if (enabledValue != null) {
					if (ThemesConstants.TAG_TRUE.equals(enabledValue.getName())) {
						member.setEnabled(true);
					}
				}
				
				canAddVariation = extractStyleVariationFiles(member, getNextElement(ThemesConstants.TAG_FILES, styleMember.getChildren()), theme.getLinkToBase());
			}
			
			if (canAddVariation) {
				member.setLimitedSelection(limitedSelection);
				theme.addStyleGroupMember(new StringBuffer(styleGroupName).append(ThemesConstants.AT).append(styleGroupMemberIndex).toString(), member);
				styleGroupMemberIndex++;
			}
		}
		return true;
	}
	
	@SuppressWarnings("unchecked")
	private List<Element> getStyleGroups(String elementSearchKey, List children) {
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
	
	@SuppressWarnings("unchecked")
	private List<Element> getStyleGroupElements(Element style) {
		if (style == null) {
			return null;
		}
		
		Element styleElements = getNextElement(ThemesConstants.RW_GROUP_MEMBERS, style.getChildren());
		
		if (styleElements == null) {
			return null;
		}
		
		return styleElements.getChildren();
	}
	
	@SuppressWarnings("unchecked")
	private boolean extractStyleVariationFiles(ThemeStyleGroupMember member, Element styleFiles, String linkToBase) {
		if (styleFiles == null || linkToBase == null) {
			return false;
		}
		List<Element> files = styleFiles.getChildren();
		if (files == null) {
			return false;
		}
		String file = null;
		String stylePath = null;
		for (int i = 0; i < files.size(); i++) {
			file = (files.get(i)).getText();
			file = StringHandler.removeCharacters(file, ContentConstants.SPACE, ContentConstants.UNDER);
			file = StringHandler.removeCharacters(file, ContentConstants.BRACKET_OPENING, ContentConstants.EMPTY);
			file = StringHandler.removeCharacters(file, ContentConstants.BRACKET_CLOSING, ContentConstants.EMPTY);
			
			//	In Theme.plist sometimes occurs errors, e.g. css file with .png extension
			if (file.endsWith(PNG_EXTENSION)) {
				stylePath = file.substring(0, file.lastIndexOf(ContentConstants.DOT));
				file = new StringBuffer(stylePath).append(CSS_EXTENSION).toString();
			}
			
			member.addStyleFile(file);
		}
		return true;
	}
	
	@SuppressWarnings("unchecked")
	private boolean extractConfiguration(Theme theme, String link) {
		Document doc = helper.getXMLDocument(link);
		if (doc == null || theme == null) {
			return false;
		}
		disableAllStyles(theme);
		
		Element root = doc.getRootElement();
		if (root == null) {
			return false;
		}
		Element name = root.getChild(ThemesConstants.CON_NAME);
		if (name == null) {
			return false;
		}
		theme.setName(name.getTextNormalize());
		
		List<Element> styles = root.getChild(ThemesConstants.CON_STYLES).getChildren();
		if (styles == null) {
			return false;
		}
		for (int i = 0; i < styles.size(); i++) {
			setEnabledStyles(theme, styles.get(i));
		}
		
		Element preview = root.getChild(ThemesConstants.CON_PREVIEW);
		theme.setLinkToThemePreview(preview.getTextNormalize());
		
		Element smallPreview = root.getChild(ThemesConstants.CON_SMALL_PREVIEW);
		theme.setLinkToSmallPreview(smallPreview.getTextNormalize());
		
		Element pageId = root.getChild(ThemesConstants.CON_PAGE_ID);
		try {
			theme.setIBPageID(Integer.valueOf(pageId.getTextNormalize()));
		} catch (NumberFormatException e) {
			e.printStackTrace();
			return false;
		}
		
		Element extraRegions = root.getChild(ThemesConstants.CON_EXTRA_REGIONS);
		loadExtraRegions(theme, extraRegions);
		
		return true;
	}
	
	@SuppressWarnings("unchecked")
	private void loadExtraRegions(Theme theme, Element regions) {
		if (theme == null || regions == null) {
			return;
		}
		
		List<Element> allRegions = regions.getChildren(ThemesConstants.CON_EXTRA_REGION);
		if (allRegions == null) {
			return;
		}
		
		Element region = null;
		for (int i = 0; i < allRegions.size(); i++) {
			region = allRegions.get(i);
			
			theme.addExtraRegion(region.getAttributeValue(ThemesConstants.CON_ATT_EXTRA_REGION_PARENT), region.getTextNormalize());
		}
	}
	
	private void setEnabledStyles(Theme theme, Element style) {
		String styleGroupName = style.getChildTextNormalize(ThemesConstants.CON_GROUP);
		String variation = style.getChildTextNormalize(ThemesConstants.CON_VARIATION);
		String variable = style.getChildTextNormalize(ThemesConstants.CON_VARIABLE);
		String color = style.getChildTextNormalize(ThemesConstants.CON_COLOR);
		ThemeStyleGroupMember member = null;
		try {
			member = helper.getThemeChanger().getStyleMember(theme, styleGroupName, variation);
		} catch (Exception e) {
			e.printStackTrace();
		}
		
		if (member != null) {
			member.setEnabled(true);
			
			if (variable != null && color != null) {
				member.setVariable(variable);
				member.setColour(color);
			
				theme.addStyleVariable(variable, color);
			}
		}
	}
	
	private void disableAllStyles(Theme theme) {
		ThemeChanger changer = helper.getThemeChanger();
		
		List <String> groupNames = theme.getStyleGroupsNames();
		ThemeStyleGroupMember member = null;
		String styleGroupName = null;
		Map <String, ThemeStyleGroupMember> styleMembers = theme.getStyleGroupsMembers();
		for (int i = 0; i < groupNames.size(); i++) {
			styleGroupName = groupNames.get(i);
			int j = 0;
			member = null;
			try {
				member = changer.getMember(styleMembers, styleGroupName, j);
			} catch (Exception e) {
				e.printStackTrace();
			}
			while (member != null) {
				member.setEnabled(false);
				j++;
				member = null;
				try {
					changer.getMember(styleMembers, styleGroupName, j);
				} catch (Exception e) {
					e.printStackTrace();
				}
			}
		}
	}
	
	private String existsFile(Theme theme, String fileName) {
		if (theme == null || fileName == null) {
			return null;
		}
		String linkToBase = theme.getLinkToBase();
		if (linkToBase == null) {
			return null;
		}
		if (!linkToBase.endsWith(ContentConstants.SLASH)) {
			linkToBase = new StringBuffer(linkToBase).append(ContentConstants.SLASH).toString();
		}
		String searchName = new StringBuffer(CoreConstants.WEBDAV_SERVLET_URI).append(theme.getLinkToBase()).append(fileName).toString();
		if (helper.existFileInSlide(searchName)) {
			return searchName;
		}
		return null;
	}
	
}
