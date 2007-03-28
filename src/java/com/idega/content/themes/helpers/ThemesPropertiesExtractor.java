package com.idega.content.themes.helpers;

import java.io.IOException;
import java.rmi.RemoteException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.apache.commons.httpclient.HttpException;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.jdom.Document;
import org.jdom.Element;

import com.idega.content.business.ContentConstants;
import com.idega.util.StringHandler;

public class ThemesPropertiesExtractor {
	
	private ThemesHelper helper = ThemesHelper.getInstance();
	
	private static final String LIMITED_SELECTION = "1";
	private static final String CSS_EXTENSION = ".css";
	
	private static final Log log = LogFactory.getLog(ThemesPropertiesExtractor.class);
	
	public boolean prepareThemes() {
		boolean result = true;
		List <Theme> themes = null;
		synchronized (ThemesPropertiesExtractor.class) {
			themes = new ArrayList<Theme>(helper.getThemesCollection());
		}
		if (themes == null) {
			return false;
		}
		
		for (int i = 0; (i < themes.size() && result); i++) {
			result = prepareTheme(themes.get(i));
		}
		return result;
	}
	
	private boolean prepareTheme(Theme theme) {
		// Checking if it is possible to extract properties
		synchronized (ThemesPropertiesExtractor.class) {
			if (theme.isLoading()) {
				return true;
			}
			if (theme.isPropertiesExtracted()) {
				return true;
			}
			
			theme.setLoading(true);
		}
		
		List files = helper.getFiles(theme.getLinkToBaseAsItIs());
		if (files == null) {
			return true;
		}
		
		String webRoot = helper.getFullWebRoot();
		String url = helper.getWebRootWithoutContent(webRoot);
		String linkToProperties = null;
		boolean foundedPropertiesFile = false;
		
		// Looking for properties file
		for (int j = 0; (j < files.size() && !foundedPropertiesFile); j++) {
			linkToProperties = files.get(j).toString();
			for (int k = 0; (k < ThemesConstants.PROPERTIES_FILES.size() && !foundedPropertiesFile); k++) {
				if (helper.isCorrectFile(helper.getFileNameWithExtension(linkToProperties), ThemesConstants.PROPERTIES_FILES.get(k))) {
					foundedPropertiesFile = true;
				}
			}
		}
		
		// Extraxting properties and preparing theme, style files for usage
		if (foundedPropertiesFile) {
			if (linkToProperties.indexOf(ThemesConstants.SPACE) != -1) {
				linkToProperties = helper.urlEncode(linkToProperties);
			}
			theme.setLinkToProperties(linkToProperties);
			extractProperties(theme, new StringBuffer(url).append(linkToProperties).toString());
			if (theme.isNewTheme()) {
				helper.getThemeChanger().prepareThemeForUsage(theme);
				helper.getThemeChanger().prepareThemeStyleFiles(theme);
				try {
					helper.getThemesService().createIBPage(theme);
				} catch (RemoteException e) {
					log.error(e);
					return false;
				}
			}
		}
		
		// Setting default theme if it does not exit
		if (theme.getName() == null) {
			theme.setName(helper.getFileName(theme.getLinkToSkeleton()));
		}
		
		// Getting theme name, which will be used to search for configuration file
		String searchName = StringHandler.removeCharacters(theme.getName(), ContentConstants.SPACE, ContentConstants.UNDER);
		String skeletonName = null;
		if (theme.getLinkToSkeleton().indexOf(ThemesConstants.THEME) != -1) {
			skeletonName = helper.decode(helper.getFileNameWithExtension(theme.getLinkToSkeleton()), true);
			searchName = helper.extractValueFromString(skeletonName, 0, skeletonName.indexOf(ThemesConstants.THEME));
		}
		
		// Searching for configuration file
		String linkToConfig = null;
		for (int i = 0; (i < files.size() && linkToConfig == null); i++) {
			if (files.get(i).toString().endsWith(new StringBuffer(searchName).append(ThemesConstants.IDEGA_THEME_INFO).toString())) {
				linkToConfig = files.get(i).toString();
			}
		}
		
		// Extracting configuration
		if (linkToConfig != null) {
			if (linkToConfig.indexOf(ThemesConstants.SPACE) != -1) {
				linkToConfig = helper.urlEncode(linkToConfig);
			}
			extractConfiguration(theme, new StringBuffer(url).append(linkToConfig).toString());
		}
		
		// Searching for previews
		if (theme.getLinkToThemePreview() == null || theme.getLinkToSmallPreview() == null) {
			searchForPreviews(theme, files);
		}
		
		// No previews where found, generating big preview
		if (theme.getLinkToThemePreview() == null) {
			String urlToFile = new StringBuffer(webRoot).append(theme.getLinkToSkeleton()).toString();
			String fileName = new StringBuffer(theme.getName()).append(ThemesConstants.THEME_PREVIEW).toString();
			if (helper.getImageGenerator().generatePreview(urlToFile,  fileName, theme.getLinkToBaseAsItIs(), ThemesConstants.PREVIEW_WIDTH, ThemesConstants.PREVIEW_HEIGHT, true)) {
				theme.setLinkToThemePreview(new StringBuffer(fileName).append(ThemesConstants.DOT).append(helper.getImageGenerator().getFileExtension()).toString());
			}
		}
		
		// If does not exist small preview, we'll get it from big preview, also encoding will be done for both images
		if (theme.getLinkToSmallPreview() == null) {
			helper.createSmallImage(theme, false);
		}
		
		// Creating configuration file
		if (linkToConfig == null) {
			helper.createThemeConfig(theme);
		}
		
		// Finishing theme
		theme.setNewTheme(false);
		theme.setPropertiesExtracted(true);
		theme.setLoading(false);
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
		
		Element smallPreview = root.getChild(ThemesConstants.CON_SMALL_PREVIEW);
		theme.setLinkToSmallPreview(smallPreview.getTextNormalize());
		
		Element pageId = root.getChild(ThemesConstants.CON_PAGE_ID);
		try {
			theme.setIBPageID(Integer.valueOf(pageId.getTextNormalize()));
		} catch (NumberFormatException e) {
			log.error(e);
		}
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
			member = helper.getThemeChanger().getMember(styleMembers, styleGroupName, j);
			while (member != null) {
				member.setEnabled(false);
				j++;
				member = helper.getThemeChanger().getMember(styleMembers, styleGroupName, j);
			}
		}
	}
	
	private void searchForPreviews(Theme theme, List files) {
		if (theme == null || files == null) {
			return;
		}
		String uri = null;
		boolean foundBig = false;
		boolean foundSmall = false;
		for (int i = 0; (i < files.size() && !foundBig && !foundSmall); i++) {
			uri = files.get(i).toString();
			if ((new StringBuffer(theme.getName()).append(ThemesConstants.THEME_PREVIEW).toString()).equals(helper.getFileName(uri))) {
				theme.setLinkToThemePreview(helper.getFileNameWithExtension(uri));
			}
			else {
				if ((new StringBuffer(theme.getName()).append(ThemesConstants.THEME_SMALL_PREVIEW).toString()).equals(helper.getFileName(uri))) {
					theme.setLinkToSmallPreview(helper.getFileNameWithExtension(uri));
				}
			}
		}
	}
	
	private void extractProperties(Theme theme, String link) {
		Document doc = helper.getXMLDocument(link);
		if (doc == null) {
			return;
		}
		Element base = doc.getRootElement().getChild(ThemesConstants.TAG_DICT);
		theme.setName(getValueFromNextElement(ThemesConstants.RW_THEME_NAME, base));
		
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
	
	private boolean extractStyleVariationFiles(ThemeStyleGroupMember member, Element styleFiles, String linkToBase) {
		if (styleFiles == null || linkToBase == null) {
			return false;
		}
		List files = styleFiles.getChildren();
		if (files == null) {
			return false;
		}
		String file = null;
		for (int i = 0; i < files.size(); i++) {
			file = ((Element)files.get(i)).getText();
			file = StringHandler.removeCharacters(file, ContentConstants.SPACE, ContentConstants.UNDER);
			file = StringHandler.removeCharacters(file, ContentConstants.BRACKET_OPENING, ContentConstants.EMPTY);
			file = StringHandler.removeCharacters(file, ContentConstants.BRACKET_CLOSING, ContentConstants.EMPTY);
			if (!file.endsWith(CSS_EXTENSION)) { // In Theme.plist sometimes occurs errors, e.g. css file with .png extension
				if (!existsFile(new StringBuffer(linkToBase).append(file).toString())) {
					file = new StringBuffer(helper.getFileName(file)).append(CSS_EXTENSION).toString();
				}
			}
			if (existsFile(new StringBuffer(linkToBase).append(file).toString())) {
				member.addStyleFile(file);
			}
			else {
				log.info(new StringBuffer("File '").append(file).append("' does not exist!"));
				return false;
			}
		}
		return true;
	}
	
	private boolean extractStyleVariations(Theme theme, String styleGroupName, List styleVariations, boolean limitedSelection) {
		if (styleVariations == null) {
			return false;
		}
		
		ThemeStyleGroupMember member = null;
		int styleGroupMemberIndex = 0;
		for (int i = 0; i < styleVariations.size(); i++) {
			Element styleMember = (Element) styleVariations.get(i);
			
			member = new ThemeStyleGroupMember();
			member.setName(getValueFromNextElement(ThemesConstants.TAG_NAME, styleMember));
			member.setType(getValueFromNextElement(ThemesConstants.TAG_TYPE, styleMember));
			member.setGroupName(styleGroupName);
			
			Element enabledValue = getNextElement(ThemesConstants.TAG_ENABLED, styleMember.getChildren());
			if (enabledValue != null) {
				if (ThemesConstants.TAG_TRUE.equals(enabledValue.getName())) {
					member.setEnabled(true);
				}
			}
			
			if (extractStyleVariationFiles(member, getNextElement(ThemesConstants.TAG_FILES, styleMember.getChildren()), theme.getLinkToBase())) {
				member.setLimitedSelection(limitedSelection);
				theme.addStyleGroupMember(new StringBuffer(styleGroupName).append(ThemesConstants.AT).append(styleGroupMemberIndex).toString(), member);
				styleGroupMemberIndex++;
			}
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
	
	private boolean existsFile(String file) {
		if (file == null) {
			return false;
		}
		try {
			return helper.getSlideService().getExistence(file);
		} catch (HttpException e) {
			e.printStackTrace();
		} catch (RemoteException e) {
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		}
		return false;
	}

}