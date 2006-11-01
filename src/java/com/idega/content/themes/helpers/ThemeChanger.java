package com.idega.content.themes.helpers;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.rmi.RemoteException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.jaxen.JaxenException;
import org.jaxen.jdom.JDOMXPath;
import org.jdom.Attribute;
import org.jdom.Comment;
import org.jdom.Document;
import org.jdom.Element;
import org.jdom.Namespace;
import org.jdom.Text;
import org.jdom.output.Format;
import org.jdom.output.XMLOutputter;

public class ThemeChanger {
	
	private static final Log log = LogFactory.getLog(ThemeChanger.class);
	
	// These are defaults in RapidWeaver, we are using its to generate good preview
	private static final String TOOLBAR_REPLACE_BEGIN = "<ul><li><a href=\"index.html\" rel=\"self\" id=\"current\">";
	private static final String TOOLBAR_REPLACE_END = "</a></li></ul>";
	
	private static final String SIDEBAR_REPLACE_BEGIN = "<div id=\"blog-categories\">";
	private static final String SIDEBAR_REPLACE_ELEMENT_BEGIN = "<div class=\"blog-category-link-disabled\">";
	private static final String SIDEBAR_REPLACE_ELEMENT_END = "</div>";
	
	private static final String BREADCRUMB_REPLACE_BEGIN = "<ul><li><a href=\"index.html\">";
	private static final String BREADCRUMB_REPLACE_END = "</a></li></ul>";
	
	// Default keywords
	private static final String TOOLBAR = "toolbar";
	private static final String SIDEBAR = "sidebar";
	private static final String BREADCRUMB = "breadcrumb";
	private static final String FOOTER = "footer";
	
	// These are defaults in RapidWeaver style files, we need to change directories to images
	private static final String CSS_IMAGE_URL = "url(";
	private static final String DIRECTORY = "../";
	private static final String IMAGES = "images";
	private static final String CUSTOM_CSS_REPLACE = CSS_IMAGE_URL + DIRECTORY + DIRECTORY + IMAGES;
	private static final String[] CUSTOM_REPLACE = new String[] {CUSTOM_CSS_REPLACE};

	// Incorrect CSS syntax needs to be replaced
	private static final String[] DEFAULT_REPLACE = new String[] {"href^=", "href$="};
	private static final String DEFAULT_CSS_REPLACEMENT = "href~=";
	
	private static final String HTML_HEAD = "head";
	private static final String HTML_BODY = "body";
	
	// Element's attributes and values
	private static final String TAG_ATTRIBUTE_REL = "rel";
	private static final String TAG_ATTRIBUTE_MEDIA = "media";
	private static final String TAG_ATTRIBUTE_VALUE_STYLESHEET = "stylesheet";
	private static final String TAG_ATTRIBUTE_VALUE_CSS = "text/css";
	private static final String TAG_ATTRIBUTE_VALUE_SCREEN = "screen";
	
	private static final String COPY_AND_SPACE = "&copy;&nbsp;";
	
	private ThemesHelper helper = ThemesHelper.getInstance();
	private Namespace namespace = Namespace.getNamespace(ThemesConstants.NAMESPACE);
	private XMLOutputter out = null;
	
	public ThemeChanger() {
		out = new XMLOutputter();
		out.setFormat(Format.getPrettyFormat());
	}
	
	/**
	 * Prepares importing theme for usage (removes needless content, adds regions, extracts properties)
	 * @param theme
	 * @return boolean
	 */
	public boolean prepareThemeForUsage(Theme theme) {
		if (!theme.isNewTheme()) {
			return true; // Theme allready prepared
		}
		
		String skeleton = theme.getLinkToSkeleton();
		if (skeleton.indexOf(ThemesConstants.SPACE) != -1) {
			skeleton = helper.urlEncode(skeleton);
		}
		Document doc = helper.getXMLDocument(helper.getFullWebRoot() + skeleton);
		if (doc == null) {
			return false;
		}
		Element root = doc.getRootElement();
		Element head = root.getChild(HTML_HEAD, namespace);
		
		// Removing needles content (like "%pathto")
		if (!proceedHeadContent(head)) {
			return false;
		}
		
		// Adding IBPage regions
		if (!proceedBodyContent(root.getChild(HTML_BODY, namespace))) {
			return false;
		}
		
		// Finding where to insert element
		int index = getElementIndex(head.getChildren(), ThemesConstants.TAG_ATTRIBUTE_TYPE, TAG_ATTRIBUTE_VALUE_CSS);
		// Adding enabled styles
		List <ThemeStyleGroupMember> members = getEnabledStyles(theme);
		for (int i = 0; i < members.size(); i++) {
			head.addContent(index, getNewStyleElement(members.get(i)));
			index++;
		}
		
		if (!uploadDocument(doc, theme.getLinkToBaseAsItIs(), helper.getFileNameWithExtension(theme.getLinkToSkeleton()), theme, true)) {
			return false;
		}
		
		return true;
	}
	
	/**
	 * 
	 * @param theme
	 * @return
	 */
	public boolean prepareThemeStyleFiles(Theme theme) {
		if (!theme.isNewTheme()) {
			return true; // Theme allready prepared
		}
		
		if (!prepareThemeDefaultStyleFiles(theme)) {
			return false;
		}
		
		Map styles = theme.getStyleGroupsMembers();
		if (styles == null) {
			return false;
		}

		Iterator it = styles.values().iterator();
		ThemeStyleGroupMember member = null;
		List <String> files = null;
		int index = theme.getLinkToBase().indexOf(ThemesConstants.THEMES);
		if (index != -1) {
			index++;
		}
		
		// Constructing correct path to images folder
		String addToCss = helper.extractValueFromString(theme.getLinkToBase(), index, theme.getLinkToBase().length());
		String[] dirLevels = addToCss.split(ThemesConstants.SLASH);
		StringBuffer replacement = new StringBuffer();
		replacement.append(CSS_IMAGE_URL);
		for (int i = 0; i < dirLevels.length; i++) {
			replacement.append(DIRECTORY);
		}
		replacement.append(addToCss);
		replacement.append(IMAGES);
		
		while (it.hasNext()) {
			member = (ThemeStyleGroupMember) it.next();
			files = member.getStyleFiles();
			for (index = 0; index < files.size(); index++) {
				if (!proceedStyleFile(theme.getLinkToBase() + files.get(index), CUSTOM_REPLACE, replacement.toString())) {
					return false;
				}
			}
		}
		return true;
	}
	
	private boolean prepareThemeDefaultStyleFiles(Theme theme) {
		List <String> defaultStyles = ThemesConstants.DEFAULT_STYLE_FILES;
		for (int i = 0; i < defaultStyles.size(); i++) {
			if (!proceedStyleFile(theme.getLinkToBase() + defaultStyles.get(i), DEFAULT_REPLACE, DEFAULT_CSS_REPLACEMENT)) {
				return false;
			}
		}
		return true;
	}
	
	private boolean proceedStyleFile(String linkToStyle, String[] replaces, String replacement) {
		if (linkToStyle == null || replaces == null || replacement == null) {
			return false;
		}
		
		// Getting css file
		InputStream is = helper.getInputStream(helper.getFullWebRoot() + linkToStyle);
		if (is == null) {
			return false;
		}
		InputStreamReader isr = new InputStreamReader(is);
		BufferedReader buf = new BufferedReader(isr);
		StringBuffer sb = new StringBuffer();
		String line;
		try {
			while ((line = buf.readLine()) != null) {
			     sb.append(line);
			}
		} catch (IOException e) {
			log.error(e);
			return false;
		} finally {
			helper.closeInputStream(is);
		}
		
		// Changing content
		String content = sb.toString();
		boolean needToReplace = false;
		for (int i = 0; i < replaces.length; i++) {
			while (content.indexOf(replaces[i]) != -1) {
				content = content.replace(replaces[i], replacement);
				needToReplace = true;
			}
		}

		if (!needToReplace) {
			return true;
		}

		// Uploading modified file
		try {
			if (!helper.getSlideService().uploadFileAndCreateFoldersFromStringAsRoot(helper.getLinkToBase(helper.decodeUrl(linkToStyle)), helper.getFileNameWithExtension(linkToStyle), content, null, true)) {
				return false;
			}
		} catch (RemoteException e) {
			log.error(e);
			return false;
		}
		
		return true;
	}
	
	/**
	 * 
	 * @param elements
	 * @param attributeValue
	 * @return int
	 */
	private int getElementIndex(List elements, String attributeType, String attributeValue) {
		int index = 0;
		if (elements == null) {
			return index;
		}
		Element e = null;
		String value = null;
		for (int i = 0; i < elements.size(); i++) {
			e = (Element) elements.get(i);
			value = e.getAttributeValue(attributeType);
			if (attributeValue.equals(value)) {
					index = i;
			}
		}
		index++;
		return index;
	}
	
	/**
	 * Uploads document to slide
	 * @param doc
	 * @param linkToBase
	 * @param fileName
	 * @return boolean
	 */
	private boolean uploadDocument(Document doc, String linkToBase, String fileName, Theme theme, boolean isTheme) {
		out.setFormat(Format.getCompactFormat());
		String docContent = out.outputString(doc);
		
		if (isTheme) {
			docContent = addRegions(docContent);
			docContent = getFixedDocumentContent(docContent);
		}
		theme.setLocked(true);
		try {
			if (!helper.getSlideService().uploadFileAndCreateFoldersFromStringAsRoot(linkToBase, fileName, docContent, null, true)) {
				return false;
			}
		} catch (RemoteException e) {
			log.error(e);
			return false;
		} finally {
			theme.setLocked(false);
		}
		return true;
	}
	
	/**
	 * Checks if document contains needless info if so, replaces with empty String
	 * @param documentContent
	 * @return String
	 */
	private String getFixedDocumentContent(String documentContent) {
		for (int i = 0; i < ThemesConstants.USELESS_CONTENT.size(); i++) {
			while (documentContent.indexOf(ThemesConstants.USELESS_CONTENT.get(i)) != -1) {
				documentContent = documentContent.replace(ThemesConstants.USELESS_CONTENT.get(i), ThemesConstants.EMPTY);
			}
		}
		return documentContent;
	}
	
	/**
	 * RapidWeaver theme consists inproper data for valid XHTML document, so needs to be fixed
	 * @param head
	 * @return boolean
	 */
	private boolean proceedHeadContent(Element head) {
		if (head == null) {
			return false;
		}
		List headElements = head.getContent();
		if (headElements == null) {
			return false;
		}
		
		Object o = null;
		List <Text> textElements = new ArrayList <Text> ();
		List <Element> elementsNeedsRegions = new ArrayList <Element> ();
		Element e = null;
		Attribute a = null;
		
		for (int i = 0; i < headElements.size(); i++) {
			o = headElements.get(i);
			if (o instanceof Element) {
				e = (Element) o;
				if (!needAddRegion(ThemesConstants.REGIONS, e.getTextNormalize())) {
					e.setText(fixValue(e.getTextNormalize()));
				}
				a = e.getAttribute(ThemesConstants.TAG_ATTRIBUTE_HREF);
				if (a == null) {
					a = e.getAttribute(ThemesConstants.TAG_ATTRIBUTE_SRC);
				}
				if (a != null) {
					a.setValue(fixValue(a.getValue())); // Fixing attribute's value
				}
			}
			else {
				if (o instanceof Text) {
					textElements.add((Text) o);
				}
			}
		}
		
		Iterator <Element> ite = elementsNeedsRegions.iterator();
		while (ite.hasNext()) {
			e = ite.next();
			e.addContent(getCommentsCollection(fixValue(e.getTextNormalize())));
		}

		Iterator <Text> itt = textElements.iterator();
		Text t = null;
		while (itt.hasNext()) {
			t = itt.next();
			if (needAddRegion(ThemesConstants.REGIONS, t.getTextNormalize())) {
				head.addContent(getCommentsCollection(fixValue(t.getTextNormalize())));
			}
			t.detach();
		}
		
		/*head.addContent(0, getCommentsCollection("header"));
		head.addContent(getCommentsCollection("user_javascript"));
		
		Collection <Element> c = new ArrayList<Element>();
		Element meta = new Element("script", namespace);
		meta.setAttribute(new Attribute("type", "text/javascript"));
		meta.setAttribute(new Attribute("src", "noScriptActually.js"));
		c.add(meta);
		head.addContent(c);*/
		
		return true;
	}
	
	/**
	 * Creates collection of Comment objets (opening and closing comment)
	 * @param commentValue
	 * @return Collection
	 */
	private Collection <Comment> getCommentsCollection(String commentValue) {
		Collection <Comment> c = new ArrayList <Comment> ();
		c.add(new Comment(ThemesConstants.TEMPLATE_REGION_BEGIN + commentValue + ThemesConstants.TEMPLATE_REGION_MIDDLE));
		c.add(new Comment(ThemesConstants.TEMPLATE_REGION_END));
		return c;
	}
	
	/**
	 * Checks if need to add a region to tag
	 * @param regions
	 * @param value
	 * @return boolean
	 */
	private boolean needAddRegion(List <String> regions, String value) {
		if (regions == null) {
			return false;
		}
		if (regions.contains(value)) {
			return true;
		}
		return false;
	}
	
	/**
	 * Adding regions to div tags like this: <!-- TemplateBeginEditable name="MyUniqueRegionId1" -->MyUniqueRegionId1<!-- TemplateEndEditable -->
	 * @param root
	 * @return boolean
	 */
	private boolean proceedBodyContent(Element root) {
		if (root == null) {
			return false;
		}
		List nodes = null;
		JDOMXPath xp = null;
		try {
			xp = new JDOMXPath(ThemesConstants.DIV_TAG_INSTRUCTION);
			xp.addNamespace(ThemesConstants.NAMESPACE_ID, ThemesConstants.NAMESPACE);
			nodes = xp.selectNodes(root);
		} catch (JaxenException e) {
			log.error(e);
		}
		if (nodes == null) {
			return false;
		}
		Iterator it = nodes.iterator();
		while (it.hasNext()) {
			addRegion((Element) it.next());
		}
		return true;
	}
	
	private String getBasicReplace(String begin, String defaultValue, String end) {
		String value = ThemesConstants.EMPTY;
		if (begin != null) {
			value += begin;
		}
		value += defaultValue;
		if (end != null) {
			value += end;
		}
		return value;
	}
	
	private String getSidebarReplace(String defaultValue) {
		String[] elements = defaultValue.split(ThemesConstants.COMMA);
		if (elements == null) {
			return ThemesConstants.EMPTY;
		}
		String sidebar = SIDEBAR_REPLACE_BEGIN;
		for (int i = 0; i < elements.length; i++) {
			sidebar += SIDEBAR_REPLACE_ELEMENT_BEGIN + elements[i] + SIDEBAR_REPLACE_ELEMENT_END;
		}
		return sidebar + SIDEBAR_REPLACE_ELEMENT_END;
	}
	
	/**
	 * Creates String, thats represens Builder's region
	 * @param value
	 * @return String
	 */
	private String getRegion(String value) {
		//String region = helper.getRegionBegin();
		String region = ThemesConstants.COMMENT_BEGIN + ThemesConstants.TEMPLATE_REGION_BEGIN + value +
			ThemesConstants.TEMPLATE_REGION_MIDDLE + ThemesConstants.COMMENT_END;
		
		ThemeSettings settings = helper.getSettings().get(value);
		if (settings != null) {
			if (value.equals(TOOLBAR)) {
				return region + getBasicReplace(TOOLBAR_REPLACE_BEGIN, settings.getDefaultValue(), TOOLBAR_REPLACE_END) +
					ThemesConstants.COMMENT_BEGIN +	ThemesConstants.TEMPLATE_REGION_END + ThemesConstants.COMMENT_END;
			}
			if (value.equals(SIDEBAR)) {
				return region + getSidebarReplace(settings.getDefaultValue()) + ThemesConstants.COMMENT_BEGIN +
					ThemesConstants.TEMPLATE_REGION_END + ThemesConstants.COMMENT_END;
			}
			if (value.equals(BREADCRUMB)) {
				return region + getBasicReplace(BREADCRUMB_REPLACE_BEGIN, settings.getDefaultValue(), BREADCRUMB_REPLACE_END) +
					ThemesConstants.COMMENT_BEGIN +	ThemesConstants.TEMPLATE_REGION_END + ThemesConstants.COMMENT_END;
			}
			if (value.equals(FOOTER)) {
				return region + COPY_AND_SPACE + getBasicReplace(null, settings.getDefaultValue(), null) + ThemesConstants.COMMENT_BEGIN +
					ThemesConstants.TEMPLATE_REGION_END + ThemesConstants.COMMENT_END;
			}
			region += settings.getDefaultValue();
		}
		
		return region + ThemesConstants.COMMENT_BEGIN +	ThemesConstants.TEMPLATE_REGION_END + ThemesConstants.COMMENT_END;
	}
	
	/**
	 * Creates Builder's regions in XML () document
	 * @param docContent
	 * @return String
	 */
	private String addRegions(String docContent) {
		String fixedValue = null;
		for (int i = 0; i < ThemesConstants.REGIONS.size(); i++) {
			fixedValue = fixValue(ThemesConstants.REGIONS.get(i));
			while (docContent.indexOf(ThemesConstants.REGIONS.get(i)) != -1) {
				docContent = docContent.replace(ThemesConstants.REGIONS.get(i), getRegion(fixedValue));
			}
		}
		return docContent;
	}
	
	/**
	 * Replacing useless content with empty String
	 * @param value
	 * @return String
	 */
	private String fixValue(String value) {
		for (int i = 0; i < ThemesConstants.USELESS_CONTENT.size(); i++) {
			while (value.indexOf(ThemesConstants.USELESS_CONTENT.get(i)) != -1) {
				value = value.replace(ThemesConstants.USELESS_CONTENT.get(i), ThemesConstants.EMPTY);
			}
		}
		return value;
	}
	
	/**
	 * Adds region to div tag if div tag has id attribute
	 * @param e
	 * @return boolean
	 */
	private boolean addRegion(Element e) {
		if (e == null) {
			return false;
		}
		String regionID = e.getAttributeValue(ThemesConstants.TAG_ATTRIBUTE_ID);
		if (regionID == null) {
			return false;
		}
		
		if (needAddRegion(ThemesConstants.BASIC_IDS_FOR_REGIONS, regionID)) {
			e.addContent(0, getCommentsCollection(regionID));
		}
		return true;
	}
	
	/**
	 * Changes theme with new style variation, creates draft and creates new preview image
	 * @param themeID
	 * @param styleGroupName
	 * @param styleMember
	 * @return String
	 */
	public String changeTheme(String themeID, String styleGroupName, String styleMember, String themeName, boolean radio, boolean checked) {
		if (themeID == null || styleGroupName == null || styleMember == null) {
			return null;
		}
		
		Theme theme = helper.getTheme(themeID);
		
		theme.setChangedName(themeName);
		
		String linkToDoc = theme.getLinkToDraft();
		if (linkToDoc == null) {
			linkToDoc = theme.getLinkToSkeleton();
		}
		
		if (linkToDoc.indexOf(ThemesConstants.SPACE) != -1) {
			linkToDoc = helper.urlEncode(linkToDoc);
		}
		Document doc = helper.getXMLDocument(helper.getFullWebRoot() + linkToDoc);
		if (doc == null) {
			return null;
		}
		
		boolean limitedSelection = true;
		
		ThemeStyleGroupMember oldStyle = null;
		ThemeStyleGroupMember newStyle = null;
		ThemeStyleGroupMember styleChanger = null;
		if (radio) { // Simply changing CSS files
			oldStyle = getEnabledStyleMember(theme, styleGroupName);
			newStyle = getStyleMember(theme, styleGroupName, styleMember);
			styleChanger = oldStyle;
		}
		else { //Need to know either add CSS or remove
			limitedSelection = false;
			if (checked) { // Need to add
				newStyle = getStyleMember(theme, styleGroupName, styleMember);
				styleChanger = newStyle;
			}
			else { //Need to remove
				oldStyle = getStyleMember(theme, styleGroupName, styleMember);
				styleChanger = oldStyle;
			}
		}
		
		Element root = doc.getRootElement();
		if (!changeThemeStyle(root.getChild(HTML_HEAD, namespace), oldStyle, newStyle)) {
			return null;
		}
		if (oldStyle != null) {
			oldStyle.setEnabled(false);
		}
		if (newStyle != null) {
			newStyle.setEnabled(true);
		}
		
		String draft = helper.getFileName(theme.getLinkToSkeleton()) + ThemesConstants.DRAFT;
		theme.setLinkToDraft(theme.getLinkToBase() + draft);
		if (!uploadDocument(doc, theme.getLinkToBaseAsItIs(), helper.decode(draft, true), theme, true)) {
			return null;
		}

		String uploadDir = helper.getFullWebRoot() + theme.getLinkToDraft();
		String fileName = theme.getName() +	ThemesConstants.DRAFT_PREVIEW;
		boolean result = helper.getPreviewGenerator().generatePreview(uploadDir, fileName, theme.getLinkToBaseAsItIs(), 800, 600);
		if (!result) {
			return null;
		}
		addThemeChange(theme, styleChanger, limitedSelection);
		theme.setLinkToDraftPreview(fileName + ThemesConstants.DOT + helper.getPreviewGenerator().getFileType());
		result = helper.createSmallImage(theme, theme.getLinkToDraftPreview());
		
		if (result) {
			return themeID;
		}
		return null;
	}

	private void addThemeChange(Theme theme, ThemeStyleGroupMember style, boolean limitedSelection) {
		ThemeChange change = new ThemeChange();
		change.setLimitedSelection(limitedSelection);
		change.setEnabled(style.isEnabled());
		change.setStyleGroupName(style.getGroupName());
		change.setStyleGroupMember(style.getName());
		theme.addThemeChange(change);
	}
	
	/**
	 * Changes theme's old style with new
	 * @param head
	 * @param oldStyle
	 * @param newStyle
	 * @return boolean
	 */
	private boolean changeThemeStyle(Element head, ThemeStyleGroupMember oldStyle, ThemeStyleGroupMember newStyle) {
		if (head == null) {
			return false;
		}
		
		List styles = head.getChildren(ThemesConstants.ELEMENT_LINK, namespace);
		if (styles == null) {
			return false;
		}
		
		int index = 4;
		List <Element> uselessStyles = new ArrayList <Element> ();
		
		if (oldStyle != null) {
			
			if (oldStyle.getStyleFiles() == null) {
				return false;
			}
			
			Element style = null;
			String attributeValue = null;
			for (int i = 0; i < styles.size(); i++) {
				style = (Element) styles.get(i);
				attributeValue = style.getAttributeValue(ThemesConstants.TAG_ATTRIBUTE_HREF);
				if (oldStyle.getStyleFiles().contains(attributeValue)) {
					uselessStyles.add(style);
					index = getElementIndex(head.getChildren(), ThemesConstants.TAG_ATTRIBUTE_HREF, attributeValue);
				}
			}
			
		}
		
		if (newStyle != null) {
			head.addContent(index, getNewStyleElement(newStyle));
		}
		
		Iterator <Element> it = uselessStyles.iterator();
		while (it.hasNext()) {
			it.next().detach();
		}
		
		return true;
	}
	
	/**
	 * Creates new style element: <link href="..." ... />
	 * @param newStyle
	 * @return Collection
	 */
	private Collection <Element> getNewStyleElement(ThemeStyleGroupMember newStyle) {
		Collection <Element> newStyleElements = new ArrayList <Element> ();
		List <Attribute> attributes = null;
		Element newStyleHref = null;
		for (int i = 0; i < newStyle.getStyleFiles().size(); i++) {
			attributes = getBasicAttributesList();			
			attributes.add(new Attribute(ThemesConstants.TAG_ATTRIBUTE_HREF, newStyle.getStyleFiles().get(i)));

			newStyleHref = new Element(ThemesConstants.ELEMENT_LINK, namespace);
			newStyleHref.setAttributes(attributes);
			newStyleElements.add(newStyleHref);
		}
		return newStyleElements;
	}
	
	/**
	 * List of basic attributes for element
	 * @return List
	 */
	private List <Attribute> getBasicAttributesList() {
		List <Attribute> attributes = new ArrayList <Attribute> ();
		attributes.add(new Attribute(TAG_ATTRIBUTE_REL, TAG_ATTRIBUTE_VALUE_STYLESHEET));
		attributes.add(new Attribute(ThemesConstants.TAG_ATTRIBUTE_TYPE, TAG_ATTRIBUTE_VALUE_CSS));
		attributes.add(new Attribute(TAG_ATTRIBUTE_MEDIA, TAG_ATTRIBUTE_VALUE_SCREEN));
		return attributes;
	}
	
	/**
	 * Gets enabled style group member (theme's variation)
	 * @param theme
	 * @param styleGroupName
	 * @return ThemeStyleGroupMember
	 */
	private ThemeStyleGroupMember getEnabledStyleMember(Theme theme, String styleGroupName) {
		Map <String, ThemeStyleGroupMember> styleMembers = theme.getStyleGroupsMembers();
		int i = 0;
		ThemeStyleGroupMember member = styleMembers.get(styleGroupName + ThemesConstants.AT + i);
		while (member != null) {
			if (member.isEnabled()) {
				return member;
			}
			i++;
			member = styleMembers.get(styleGroupName + ThemesConstants.AT + i);
		}
		
		return null;
	}
	
	/**
	 * Gets enabled style groups members (theme's variations)
	 * @param theme
	 * @return List
	 */
	private List <ThemeStyleGroupMember> getEnabledStyles(Theme theme) {
		List <ThemeStyleGroupMember> members = new ArrayList<ThemeStyleGroupMember>();
		List <String> groupNames = theme.getStyleGroupsNames();
		ThemeStyleGroupMember member = null;
		String styleGroupName = null;
		Map <String, ThemeStyleGroupMember> styleMembers = theme.getStyleGroupsMembers();
		for (int i = 0; i < groupNames.size(); i++) {
			styleGroupName = groupNames.get(i);
			int j = 0;
			member = styleMembers.get(styleGroupName + ThemesConstants.AT + j);
			while (member != null) {
				if (member.isEnabled()) {
					members.add(member);
				}
				j++;
				member = styleMembers.get(styleGroupName + ThemesConstants.AT + j);
			}
		}
		return members;
	}
	
	/**
	 * Searches and returns style group member (theme's variation)
	 * @param theme
	 * @param styleGroupName
	 * @param styleVariation
	 * @return ThemeStyleGroupMember
	 */
	public ThemeStyleGroupMember getStyleMember(Theme theme, String styleGroupName, String styleVariation) {
		Map <String, ThemeStyleGroupMember> styleMembers = theme.getStyleGroupsMembers();
		int i = 0;
		ThemeStyleGroupMember member = styleMembers.get(styleGroupName + ThemesConstants.AT + i);
		while (member != null) {
			if (styleVariation.equals(member.getName())) {
				return member;
			}
			i++;
			member = styleMembers.get(styleGroupName + ThemesConstants.AT + i);
		}
		
		return null;
	}
	
	/**
	 * Saves theme
	 * @param themeID
	 * @param themeName
	 * @return boolean
	 */
	public boolean saveTheme(String themeID, String themeName) {
		if (themeID == null || themeName == null) {
			return false;
		}
		
		Theme theme = helper.getTheme(themeID);
		
		if (!theme.getName().equals(themeName)) {
			return createNewTheme(theme, themeName);
		}
		
		if (theme.getLinkToDraft() == null) {
			log.info("No draft for theme: " + themeID);
			return false;
		}

		InputStream is = null;
		is = helper.getInputStream(helper.getFullWebRoot() + theme.getLinkToDraft());
		if (is == null) {
			return false;
		}
		
		String fileName = helper.decode(helper.getFileNameWithExtension(theme.getLinkToSkeleton()), true);
		theme.setLocked(true);
		try {
			if (!helper.getSlideService().uploadFileAndCreateFoldersFromStringAsRoot(theme.getLinkToBaseAsItIs(), fileName, is, null, true)) {
				return false;
			}
		} catch (RemoteException e) {
			log.error(e);
			return false;
		} finally {
			helper.closeInputStream(is);
		}
		theme.setLocked(false);
		
		theme.setChanges(new ArrayList<ThemeChange>());
		theme.setLinkToDraft(null);
		theme.setLinkToThemePreview(theme.getLinkToDraftPreview());
		theme.setLinkToDraftPreview(null);
		
		return createThemeConfig(theme);
	}
	
	private void restoreTheme(Theme theme) {
		if (theme == null) {
			return;
		}
		List <ThemeChange> changes = theme.getChanges();
		ThemeChange change = null;
		ThemeStyleGroupMember member = null;
		for (int i = 0; i < changes.size(); i++) {
			change = changes.get(i);
			member = getStyleMember(theme, change.getStyleGroupName(), change.getStyleGroupMember());
			if (member != null) {
				if (change.isLimitedSelection()) {
					disableStyle(theme, member.getGroupName());
				}
				member.setEnabled(!change.isEnabled());
			}
		}
	}
	
	private void disableStyle(Theme theme, String styleGroupName) {
		ThemeStyleGroupMember member = null;
		Iterator <ThemeStyleGroupMember> it = theme.getStyleGroupsMembers().values().iterator();
		while (it.hasNext()) {
			member = it.next();
			if (member.getGroupName().equals(styleGroupName)) {
				member.setEnabled(false);
			}
		}
	}
	
	private boolean createNewTheme(Theme parent, String newName) {
		String linkToTheme = parent.getLinkToDraft();
		parent.setLinkToDraft(null);
		if (linkToTheme == null) {
			linkToTheme = parent.getLinkToSkeleton();
		}
		if (linkToTheme == null) {
			return false;
		}
		InputStream is = helper.getInputStream(helper.getFullWebRoot() + linkToTheme);
		if (is == null) {
			return false;
		}
		String linkToBase = helper.getLinkToBase(linkToTheme);
		if (!linkToBase.endsWith(ThemesConstants.SLASH)) {
			linkToBase += ThemesConstants.SLASH;
		}
		String decodedLinkToBase = helper.decodeUrl(linkToBase);
		if (!decodedLinkToBase.endsWith(ThemesConstants.SLASH)) {
			decodedLinkToBase += ThemesConstants.SLASH;
		}
		String themeName = newName + ThemesConstants.THEME;
		try {
			if (!helper.getSlideService().uploadFileAndCreateFoldersFromStringAsRoot(decodedLinkToBase,	themeName, is, null, true)) {
				return false;
			}
		} catch (RemoteException e) {
			log.error(e);
			return false;
		} finally {
			helper.closeInputStream(is);
		}
		
		String themeID = helper.getThemesLoader().createNewTheme(decodedLinkToBase + themeName, linkToBase + helper.encode(themeName,
				true), true);
		if (themeID == null) {
			return false;
		}
		Theme child = helper.getTheme(themeID);
		child.setName(newName);

		copyTheme(parent, child);
		
		String linkToPreview = parent.getLinkToDraftPreview();
		parent.setLinkToDraftPreview(null);
		if (linkToPreview == null) {
			linkToPreview = parent.getLinkToThemePreview();
		}
		linkToBase = child.getLinkToBase();
		if (!linkToBase.endsWith(ThemesConstants.SLASH)) {
			linkToBase += ThemesConstants.SLASH;
		}
		is = helper.getInputStream(helper.getFullWebRoot() + linkToBase +  helper.encode(linkToPreview, true));
		if (is == null) {
			return false;
		}
		String fileName = child.getName() + ThemesConstants.THEME_PREVIEW + ThemesConstants.DOT + helper.getFileExtension(linkToPreview);
		try {
			if (helper.getSlideService().uploadFileAndCreateFoldersFromStringAsRoot(decodedLinkToBase, fileName, is, null, true)) {
				child.setLinkToThemePreview(fileName);
			}
		} catch (RemoteException e) {
			log.error(e);
		} finally {
			helper.closeInputStream(is);
		}
		if (!helper.createSmallImage(child, child.getLinkToThemePreview())) {
			return false;
		}
		
		child.setPropertiesExtracted(true);
		
		restoreTheme(parent);
		
		return createThemeConfig(child);
	}
	
	/**
	 * Copies parent theme's style groups and variations to child theme
	 * @param parent
	 * @param child
	 */
	private void copyTheme(Theme parent, Theme child) {
		List <String> groupNames = parent.getStyleGroupsNames();
		ThemeStyleGroupMember member = null;
		ThemeStyleGroupMember parentMember = null;
		String styleGroupName = null;
		Map <String, ThemeStyleGroupMember> styleMembers = parent.getStyleGroupsMembers();
		for (int i = 0; i < groupNames.size(); i++) {
			styleGroupName = groupNames.get(i);
			child.addStyleGroupName(styleGroupName);
			int j = 0;
			parentMember = styleMembers.get(styleGroupName + ThemesConstants.AT + j);
			while (parentMember != null) {
				member = new ThemeStyleGroupMember(parentMember);
				child.addStyleGroupMember(styleGroupName + ThemesConstants.AT + j, member);
				j++;
				parentMember = styleMembers.get(styleGroupName + ThemesConstants.AT + j);
			}
		}
	}
	
	private boolean createThemeConfig(Theme theme) {
		Document doc = new Document();
		Element root = new Element(ThemesConstants.CON_THEME);
		Collection <Element> rootElements = new ArrayList<Element>();
		
		Element name = new Element(ThemesConstants.CON_NAME);
		name.setText(theme.getName());
		rootElements.add(name);
		
		Element styles = new Element(ThemesConstants.CON_STYLES);
		Collection <Element> stylesElements = new ArrayList<Element>();
		
		List <ThemeStyleGroupMember> enabled = getEnabledStyles(theme);
		ThemeStyleGroupMember member = null;
		
		Element style = null;
		Collection <Element> styleElements = null;
		Element groupName = null;
		Element variation = null;
		for (int i = 0; i < enabled.size(); i++) {
			member = enabled.get(i);
			style = new Element(ThemesConstants.CON_STYLE);
			styleElements = new ArrayList<Element>();

			groupName = new Element(ThemesConstants.CON_GROUP);
			groupName.setText(member.getGroupName());
			styleElements.add(groupName);
			
			variation = new Element(ThemesConstants.CON_VARIATION);
			variation.setText(member.getName());
			styleElements.add(variation);

			style.setContent(styleElements);
			stylesElements.add(style);
		}
		styles.setContent(stylesElements);
		rootElements.add(styles);
		
		Element preview = new Element(ThemesConstants.CON_PREVIEW);
		preview.setText(theme.getLinkToThemePreview());
		rootElements.add(preview);
		
		Element smallPreview = new Element(ThemesConstants.CON_SMALL_PREVIEW);
		smallPreview.setText(theme.getLinkToSmallPreview());
		rootElements.add(smallPreview);
		
		root.setContent(rootElements);
		doc.setRootElement(root);
		return uploadDocument(doc, theme.getLinkToBaseAsItIs(), theme.getName() + ThemesConstants.IDEGA_THEME_INFO, theme, false);
	}

}
