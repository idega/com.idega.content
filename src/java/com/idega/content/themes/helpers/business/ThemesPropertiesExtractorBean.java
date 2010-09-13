package com.idega.content.themes.helpers.business;

import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.jdom.Document;
import org.jdom.Element;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.context.annotation.Scope;
import org.springframework.stereotype.Service;

import com.idega.content.business.ContentConstants;
import com.idega.content.themes.helpers.bean.BuiltInThemeStyle;
import com.idega.content.themes.helpers.bean.Theme;
import com.idega.content.themes.helpers.bean.ThemeStyleGroupMember;
import com.idega.util.CoreConstants;
import com.idega.util.ListUtil;
import com.idega.util.StringHandler;

@Scope(BeanDefinition.SCOPE_SINGLETON)
@Service(ThemesPropertiesExtractor.SPRING_BEAN_IDENTIFIER)
public class ThemesPropertiesExtractorBean implements ThemesPropertiesExtractor {

	@Autowired
	private ThemesHelper helper;
	@Autowired
	private ThemeChanger themeChanger;
	
	private static final String LIMITED_SELECTION = "1";
	private static final String CSS_EXTENSION = ".css";
	private static final String PNG_EXTENSION = ".png";
	
	private static final Logger LOGGER = Logger.getLogger(ThemesPropertiesExtractorBean.class.getName());
	
	public void prepareThemes(List<String> pLists, List<String> configs, List<String> predefinedStyles, boolean useThread) {
		//	Initializing ImageGenerator
		getHelper().getImageGenerator(null);
		
		//	Firstly getting not prepared themes
		List<Theme> themesToPrepare = getUnPreparedThemes();
		if (ListUtil.isEmpty(themesToPrepare)) {
			return;
		}
		
		//	Preparing new theme(s)
		for (Theme theme: themesToPrepare) {
			if (!prepareTheme(theme, pLists, configs, predefinedStyles, useThread)) {
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
		List<Theme> themes = new ArrayList<Theme>(getHelper().getAllThemes());
		
		for (Theme theme: themes) {
			//	Checking if it is possible to extract properties
			if (!theme.isLoading() && !theme.isPropertiesExtracted() && !newThemes.contains(theme)) {
				theme.setLoading(true);
				newThemes.add(theme);
			}
		}
		return newThemes;
	}
	
	private boolean prepareTheme(Theme theme, List<String> pLists, List<String> configs, List<String> predefinedStyles, boolean useThread) {
		if (useThread) {
			ThemePropertiesExtractor extractor = new ThemePropertiesExtractor(theme, this, pLists, configs, predefinedStyles);
			extractor.start();
			return true;
		}
		
		return prepareTheme(true, theme, pLists, configs, predefinedStyles);
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
	
	private boolean checkFileExistence(String uri) {
		try {
			return helper.getSlideService().getExistence(uri);
		} catch (Exception e) {
			LOGGER.log(Level.WARNING, "Error while checking existence of: " + uri, e);
		}
		return false;
	}
	
	private String findThemeConfiguration(String baseUri, String searchName) {
		if (!baseUri.endsWith(CoreConstants.SLASH)) {
			baseUri = baseUri.concat(CoreConstants.SLASH);
		}
		String configUri = baseUri.concat(searchName);
		if (checkFileExistence(configUri)) {
			if (!configUri.startsWith(CoreConstants.WEBDAV_SERVLET_URI)) {
				configUri = CoreConstants.WEBDAV_SERVLET_URI.concat(configUri);
			}
			return configUri;
		}
		return null;
	}
	
	private String getConfigFile(Theme theme, String searchName, List<String> configs) {
		String baseUri = theme.getLinkToBaseAsItIs();
		searchName = new StringBuffer(searchName).append(ThemesConstants.IDEGA_THEME_INFO).toString();
		if (!ListUtil.isEmpty(configs)) {
			for (String config: configs) {
				try {
					if (config.indexOf(baseUri) != -1 || config.indexOf(URLEncoder.encode(baseUri, CoreConstants.ENCODING_UTF8)) != -1) {
						if (config.endsWith(searchName) || config.endsWith(URLEncoder.encode(searchName, CoreConstants.ENCODING_UTF8))) {
							return config;
						}
					}
				} catch (Exception e) {
					LOGGER.log(Level.WARNING, "Error while encoding: " + searchName, e);
				}
			}
		}
		
		String config = findThemeConfiguration(baseUri, searchName);
		if (config != null) {
			return config;
		}
		try {
			String encodedSearchName = URLEncoder.encode(searchName, CoreConstants.ENCODING_UTF8);
			config = findThemeConfiguration(baseUri, encodedSearchName);
			if (config != null) {
				return config;
			}
			
			String encodedBaseUri = URLEncoder.encode(baseUri, CoreConstants.ENCODING_UTF8);
			config = findThemeConfiguration(encodedBaseUri, searchName);
			if (config != null) {
				return config;
			}
			
			config = findThemeConfiguration(encodedBaseUri, encodedSearchName);
			if (config != null) {
				return config;
			}
		} catch (UnsupportedEncodingException e) {
			LOGGER.log(Level.WARNING, "Error while encoding!", e);
		}
		
		LOGGER.info("Didn't find configuration for theme: " + searchName + " in provided configs: " + configs);
		return null;
	}
	
	public boolean prepareTheme(boolean checkConfigFile, Theme theme, List<String> pLists, List<String> configs, List<String> predefinedStyles) {
		if (theme == null) {
			return false;
		}
		
		String webRoot = getHelper().getFullWebRoot();
		String url = getHelper().getWebRootWithoutContent(webRoot);
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
					if (getHelper().isCorrectFile(getHelper().getFileNameWithExtension(searchResult), ThemesConstants.THEME_PROPERTIES_FILES.get(i))) {
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
				linkToProperties = getHelper().urlEncode(linkToProperties);
			}
			theme.setLinkToProperties(linkToProperties);
			if (!extractProperties(theme, new StringBuffer(url).append(linkToProperties).toString())) {
				markThemeAsNotPrepared(theme);
				return false;
			}
		}
		
		// Setting default theme if it does not exit
		if (theme.getName() == null) {
			theme.setName(getHelper().getFileName(theme.getLinkToSkeleton()));
		}
		
		String linkToConfig = null;
		if (checkConfigFile) {
			//	Getting theme name, which will be used to search for configuration file
			String searchName = getHelper().getPreparedThemeNameToUseInRepository(theme);
			String skeletonName = null;
			if (theme.getLinkToSkeleton().indexOf(ThemesConstants.THEME) != -1) {
				skeletonName = getHelper().decode(getHelper().getFileNameWithExtension(theme.getLinkToSkeleton()), true);
				searchName = getHelper().extractValueFromString(skeletonName, 0, skeletonName.indexOf(ThemesConstants.THEME));
			}
			
			//	Trying to get configuration file from list
			linkToConfig = getConfigFile(theme, searchName, configs);
			
			theme.setNewTheme(linkToConfig == null);	//	Every theme must have configuration file, if don't - treat theme as new theme
		}
		
		if (theme.isNewTheme()) {
			theme.setNewTheme(true);
			ThemeChanger changer = getThemeChanger();
			if (changer == null) {
				markThemeAsNotPrepared(theme);
				return false;
			}
			try {
				if (!(changer.prepareThemeStyleFiles(theme))) {	//	Checking and preparing CSS files
					markThemeAsNotPrepared(theme);
					return false;
				}
				if (!(changer.prepareThemeForUsage(theme))) {	//	Preparing skeleton
					markThemeAsNotPrepared(theme);
					return false;
				}
				if (!(getHelper().getThemesService().createBuilderTemplate(theme))) {
					markThemeAsNotPrepared(theme);
					return false;
				}
			} catch (Exception e) {
				LOGGER.log(Level.WARNING, "Error creating new theme", e);
				markThemeAsNotPrepared(theme);
				return false;
			}
		}
			
		if (checkConfigFile && linkToConfig != null) {
			// Extracting configuration
			if (linkToConfig.indexOf(CoreConstants.SPACE) != -1) {
				linkToConfig = getHelper().urlEncode(linkToConfig);
			}
			if (!extractConfiguration(theme, new StringBuffer(url).append(linkToConfig).toString())) {
				markThemeAsNotPrepared(theme);
				return false;
			}
		}
		
		//	Checking previews
		if (theme.getLinkToSmallPreview() == null) {
			//	And creating if don't exist
			if (!getHelper().generatePreviewsForTheme(theme, false, ThemesConstants.IS_THEME_PREVIEW_JPG, ThemesConstants.THEME_PREVIEW_QUALITY)) {
				markThemeAsNotPrepared(theme);
				return false;
			}
		}
		
		//	Extracting predefined styles (*.rwstyle) for current theme
		if (!extractPredefinedStyles(theme, predefinedStyles)) {
			markThemeAsNotPrepared(theme);
			return false;
		}
		
		// Creating configuration file
		if (linkToConfig == null && checkConfigFile) {
			if (!getHelper().createThemeConfig(theme)) {
				markThemeAsNotPrepared(theme);
				return false;
			}
		}
		
		// Finishing theme
		theme.setNewTheme(false);
		theme.setPropertiesExtracted(true);
		theme.setLoading(false);
		getHelper().addLoadedTheme(theme.getId());
		
		return true;
	}
	
	private boolean extractPredefinedStyles(Theme theme, List<String> predefinedStyles) {
		if (theme == null) {
			return false;
		}
		
		if (predefinedStyles == null) {
			predefinedStyles = new ArrayList<String>(getHelper().getPredefinedThemeStyles());
			if (predefinedStyles.isEmpty()) {
				return true;
			}
		}
		
		String linkToBase = theme.getLinkToBase();
		List<String> thisThemesStyles = new ArrayList<String>();
		for (String style: predefinedStyles) {
			if (style.startsWith(CoreConstants.WEBDAV_SERVLET_URI)) {
				style = style.replaceFirst(CoreConstants.WEBDAV_SERVLET_URI, CoreConstants.EMPTY);
			}
			
			if (style.startsWith(linkToBase)) {
				thisThemesStyles.add(style);
			}
		}
		if (thisThemesStyles.isEmpty()) {
			return true;
		}
		
		List<String> allPredefinedStyles = getHelper().getPredefinedThemeStyles();
		for (String style: thisThemesStyles) {
			if (parsePredefinedStyle(theme, style)) {	
				allPredefinedStyles.remove(style);
			}
		}
		
		return true;
	}
	
	@SuppressWarnings("unchecked")
	private boolean parsePredefinedStyle(Theme theme, String uri) {
		if (theme == null || uri == null) {
			return false;
		}
		
		Document styleConfig = getHelper().getXMLDocument(new StringBuilder(getHelper().getFullWebRoot()).append(uri).toString());
		if (styleConfig == null) {
			return false;
		}
		
		Element root = styleConfig.getRootElement();
		if (root == null) {
			return false;
		}
		
		Element dict = root.getChild(ThemesConstants.TAG_DICT);
		if (dict == null) {
			return false;
		}
		
		List<Element> content = dict.getChildren();
		if (content == null || content.isEmpty()) {
			return true;
		}
		
		String styleName = getValueFromNextElement("Style Name", dict);
		if (styleName == null) {
			return false;
		}
		
		Map<String, String> colours = new HashMap<String, String>();
		Element coloursConfig = getNextElement(getElementByValue("Colours", content), content);
		if (coloursConfig != null) {
			List<Element> colourElements = coloursConfig.getChildren("key");
			if (colourElements != null && !colourElements.isEmpty()) {
				String colourName = null;
				String colourValue = null;
				for (Element colourNameElement: colourElements) {
					colourName = colourNameElement.getTextNormalize();
					colourValue = getValueFromNextElement(colourName, coloursConfig);
					
					if (colourName != null && colourValue != null) {
						colourName = StringHandler.replace(colourName, CoreConstants.PERCENT, CoreConstants.EMPTY);
						colours.put(colourName, colourValue);
					}
				}
			}
		}
		
		Map<String, String> variations = new HashMap<String, String>();
		Element variationsConfig = getNextElement(getElementByValue("Selections", content), content);
		if (variationsConfig != null) {
			List<Element> variationElements = variationsConfig.getChildren();
			if (variationElements != null && !variationElements.isEmpty()) {
				String fullVariation = null;
				String variationGroup = null;
				String variationValue = null;
				int firstDotIndex = -1;
				for (Element variationElement: variationElements) {
					fullVariation = variationElement.getTextNormalize();
					
					firstDotIndex = fullVariation.indexOf(CoreConstants.DOT);
					if (fullVariation != null && firstDotIndex != -1) {
						variationGroup = fullVariation.substring(0, firstDotIndex);
						variationValue = fullVariation.substring(firstDotIndex + 1);
						if (variationGroup != null && variationValue != null) {
							variations.put(variationGroup, variationValue);
						}
					}
				}
			}
		}
		
		BuiltInThemeStyle style = new BuiltInThemeStyle(getHelper().getBuiltInThemeStyleId(theme), styleName, uri);
		style.setColours(colours);
		style.setVariations(variations);
		theme.addBuiltInStyle(style);
		
		return true;
	}
	
	@SuppressWarnings("unchecked")
	private boolean extractProperties(Theme theme, String link) {
		Document doc = getHelper().getXMLDocument(link);
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
				LOGGER.info(new StringBuffer("Style group '").append(styleGroupName).append("' has no CSS file! Disabling this group.").toString());
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
	private List<Element> getStyleGroups(String elementSearchKey, @SuppressWarnings("rawtypes") List children) {
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
			file = getHelper().getFixedSlideFileName((files.get(i)).getText());
			
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
		Document doc = getHelper().getXMLDocument(link);
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
		
		Element smallPreview = root.getChild(ThemesConstants.CON_SMALL_PREVIEW);
		theme.setLinkToSmallPreview(smallPreview.getTextNormalize());
		
		Element pageId = root.getChild(ThemesConstants.CON_PAGE_ID);
		try {
			theme.setIBPageID(Integer.valueOf(pageId.getTextNormalize()));
		} catch (NumberFormatException e) {
			LOGGER.log(Level.WARNING, "Error converting to integer: " + pageId.getTextNormalize(), e);
			return false;
		}
		
		Element builtInStyleUri = root.getChild(ThemesConstants.CON_URI_OF_CURRENT_BUILT_IN_STYLE);
		if (builtInStyleUri != null) {
			theme.setCurrentlyUsedBuiltInStyleUri(builtInStyleUri.getTextNormalize());
		}
		
		Element extraRegions = root.getChild(ThemesConstants.CON_EXTRA_REGIONS);
		loadExtraRegions(theme, extraRegions);
		
		Element originalColourFiles = root.getChild(ThemesConstants.CON_COLOUR_FILES_ORIGINAL);
		Element colourFiles = root.getChild(ThemesConstants.CON_COLOUR_FILES);
		loadColourFiles(theme, originalColourFiles, colourFiles);
		
		return true;
	}
	
	private void loadColourFiles(Theme theme, Element originalFilesElement, Element filesElement) {
		List<String> originalFiles = getParsedColourFiles(originalFilesElement);
		if (originalFiles != null) {
			theme.setOriginalColourFiles(originalFiles);
		}
		
		List<String> files = getParsedColourFiles(filesElement);
		if (files != null) {
			theme.setColourFiles(files);
		}
	}
	
	@SuppressWarnings("unchecked")
	private List<String> getParsedColourFiles(Element parentElement) {
		if (parentElement == null) {
			return null;
		}
		
		List<Element> files = parentElement.getChildren(ThemesConstants.CON_COLOUR_FILE);
		if (files == null || files.isEmpty()) {
			return null;
		}
		
		List<String> parsedFiles = new ArrayList<String>();
		for (Element file: files) {
			parsedFiles.add(file.getTextNormalize());
		}
		
		return parsedFiles;
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
			member = getThemeChanger().getStyleMember(theme, styleGroupName, variation);
		} catch (Exception e) {
			LOGGER.log(Level.WARNING, "Error setting style", e);
		}
		
		if (member == null) {
			return;
		}
		
		member.setEnabled(true);
		
		if (variable != null && color != null) {
			member.setVariable(variable);
			member.setColour(color);
		
			theme.addStyleVariable(variable, color);
		}
	}
	
	private void disableAllStyles(Theme theme) {
		ThemeChanger changer = getThemeChanger();
		if (changer == null) {
			return;
		}
		
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
				LOGGER.log(Level.WARNING, "Error getting style member", e);
			}
			while (member != null) {
				member.setEnabled(false);
				j++;
				member = null;
				try {
					changer.getMember(styleMembers, styleGroupName, j);
				} catch (Exception e) {
					LOGGER.log(Level.WARNING, "Error getting style member", e);
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
		if (getHelper().existFileInSlide(searchName)) {
			return searchName;
		}
		return null;
	}

	public ThemesHelper getHelper() {
		return helper;
	}

	public void setHelper(ThemesHelper helper) {
		this.helper = helper;
	}

	public ThemeChanger getThemeChanger() {
		return themeChanger;
	}

	public void setThemeChanger(ThemeChanger themeChanger) {
		this.themeChanger = themeChanger;
	}
	
}
