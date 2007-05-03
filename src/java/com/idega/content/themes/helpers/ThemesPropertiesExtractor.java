package com.idega.content.themes.helpers;

import java.rmi.RemoteException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.jdom.Document;
import org.jdom.Element;

import com.idega.content.business.ContentConstants;
import com.idega.util.StringHandler;

public class ThemesPropertiesExtractor {
	
	private ThemesHelper helper = ThemesHelper.getInstance();
	
	protected static final String LIMITED_SELECTION = "1";
	protected static final String CSS_EXTENSION = ".css";
	
	private static final Log log = LogFactory.getLog(ThemesPropertiesExtractor.class);
	
	public boolean prepareThemes(List<String> pLists, List<String> configs, boolean useThread) {
		//	Initializing ImageGenerator
		helper.getImageGenerator(null);
		
		boolean prepared = true;
		
		//	Firstly getting unprepared themes
		List<Theme> themesToPrepare = getUnPreparedThemes();
		
		//	Preparing new theme(s)
		for (int i = 0; (i < themesToPrepare.size() && prepared); i++) {
			prepared = prepareTheme(themesToPrepare.get(i), pLists, configs, useThread);
		}
		return prepared;
	}
	
	private synchronized List<Theme> getUnPreparedThemes() {
		List<Theme> newThemes = new ArrayList<Theme>();
		List <Theme> themes = new ArrayList<Theme>(helper.getThemesCollection());
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
			ThemePropertiesExtractor extractor = new ThemePropertiesExtractor(theme, this, pLists, configs);
			extractor.start();
			return true;
		}
		
		return prepareTheme(theme, pLists, configs);
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
	
	protected boolean prepareTheme(Theme theme, List<String> pLists, List<String> configs) {
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
					e.printStackTrace();
				}
			}
		}
		
		// Setting default theme if it does not exit
		if (theme.getName() == null) {
			theme.setName(helper.getFileName(theme.getLinkToSkeleton()));
		}
		
		//	Getting theme name, which will be used to search for configuration file
		String searchName = StringHandler.removeCharacters(theme.getName(), ContentConstants.SPACE, ContentConstants.UNDER);
		String skeletonName = null;
		if (theme.getLinkToSkeleton().indexOf(ThemesConstants.THEME) != -1) {
			skeletonName = helper.decode(helper.getFileNameWithExtension(theme.getLinkToSkeleton()), true);
			searchName = helper.extractValueFromString(skeletonName, 0, skeletonName.indexOf(ThemesConstants.THEME));
		}
		
		String linkToConfig = null;
		//	Trying to get config file from list
		linkToConfig = getConfigFile(theme, searchName, configs);
		
		// Extracting configuration
		if (linkToConfig != null) {
			if (linkToConfig.indexOf(ThemesConstants.SPACE) != -1) {
				linkToConfig = helper.urlEncode(linkToConfig);
			}
			extractConfiguration(theme, new StringBuffer(url).append(linkToConfig).toString());
		}
		
		//	Checking previews
		if (theme.getLinkToThemePreview() == null || theme.getLinkToSmallPreview() == null) {
			//	And creating if don't exist
			helper.generatePreviewsForTheme(theme, false, true, 1f);
		}
		
		// Creating configuration file
		if (linkToConfig == null) {
			helper.createThemeConfig(theme);
		}
		
		// Finishing theme
		theme.setNewTheme(false);
		theme.setPropertiesExtracted(true);
		theme.setLoading(false);
		helper.addLoadedTheme(theme.getId());
		
		return true;
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
			extractStyleVariations(theme, styleGroupName, getStyleGroupElements(style), ThemesPropertiesExtractor.LIMITED_SELECTION.equals(selectionLimit));
			if (theme.getStyleGroupsMember(new StringBuffer(styleGroupName).append(ThemesConstants.AT).append(0).toString()) == null) {
				log.info(new StringBuffer("Style group '").append(styleGroupName).append("' has no CSS file! Disabling this group."));
			}
			else {
				theme.addStyleGroupName(styleGroupName);
			}

		}
		return true;
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
	
	private boolean extractStyleVariations(Theme theme, String styleGroupName, List styleVariations, boolean limitedSelection) {
		if (styleVariations == null) {
			return false;
		}
		
		ThemeStyleGroupMember member = null;
		int styleGroupMemberIndex = 0;
		Element styleMember = null;
		for (int i = 0; i < styleVariations.size(); i++) {
			styleMember = (Element) styleVariations.get(i);
			
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
			member.addStyleFile(file);
		}
		return true;
	}
	
	private void extractConfiguration(Theme theme, String link) {
		Document doc = helper.getXMLDocument(link);
		if (doc == null || theme == null) {
			return;
		}
		disableAllStyles(theme);
		
		Element root = doc.getRootElement();
		if (root == null) {
			return;
		}
		Element name = root.getChild(ThemesConstants.CON_NAME);
		if (name == null) {
			return;
		}
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
			e.printStackTrace();
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
		String searchName = new StringBuffer(ContentConstants.CONTENT).append(theme.getLinkToBase()).append(fileName).toString();
		if (helper.existFileInSlide(searchName)) {
			return searchName;
		}
		return null;
	}

}