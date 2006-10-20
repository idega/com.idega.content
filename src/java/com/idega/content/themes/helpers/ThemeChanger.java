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
import org.jdom.output.XMLOutputter;

public class ThemeChanger {
	
	private static final Log log = LogFactory.getLog(ThemeChanger.class);
	
	// These are defaults in RapidWeaver, we are using its to generate good preview
	private static final String TITLE_REPLACE = "My company";
	private static final String SLOGAN_REPLACE = "Changing the world, one site at a time...";
	private static final String TOOLBAR_REPLACE = "<ul><li><a href=\"index.html\" rel=\"self\" id=\"current\">Untitled Page 1</a></li></ul>";
	private static final String SIDEBAR_REPLACE = "<div id=\"blog-categories\"><div class=\"blog-category-link-disabled\">Personal</div><div class=\"blog-category-link-disabled\">Work</div><div class=\"blog-category-link-disabled\">Humor</div><div class=\"blog-category-link-disabled\">Apple</div></div><div id=\"blog-archives\"></div>";
	private static final String BREADCRUMB_REPLACE = "<ul><li><a href=\"index.html\">Untitled Page 1</a>&nbsp;>&nbsp;</li></ul>";
	private static final String FOOTER_REPLACE = "&copy; My company";
	
	// Default keywords
	private static final String TITLE = "site_title";
	private static final String SLOGAN = "site_slogan";
	private static final String TOOLBAR = "toolbar";
	private static final String SIDEBAR = "sidebar";
	private static final String BREADCRUMB = "breadcrumb";
	private static final String FOOTER = "footer";
	
	// These are defaults in RapidWeaver style files, we need to change directories to images
	private static final String CSS_IMAGE_URL = "url(";
	private static final String DIRECTORIES = "../../";
	private static final String IMAGES = "images";
	private static final String CSS_REPLACE = CSS_IMAGE_URL + DIRECTORIES + IMAGES;
	
	private ThemesHelper helper = ThemesHelper.getInstance();
	private Namespace namespace = Namespace.getNamespace(ThemesConstants.NAMESPACE);
	
	/**
	 * Prepares importing theme for usage (removes needless content, adds regions, extracts properties)
	 * @param theme
	 * @return boolean
	 */
	public boolean prepareThemeForUsage(ThemeInfo theme) {
		if (!theme.isNewTheme()) {
			return true; // Theme allready prepared
		}
		
		Document doc = helper.getXMLDocument(helper.getFullWebRoot() + theme.getLinkToSkeleton());
		if (doc == null) {
			return false;
		}
		Element root = doc.getRootElement();
		Element head = root.getChild("head", namespace);
		
		// Removing needles content (like "%pathto")
		if (!proceedHeadContent(head)) {
			return false;
		}
		
		// Adding IBPage regions
		if (!proceedBodyContent(root.getChild("body", namespace))) {
			return false;
		}
		
		// Finding where to insert element
		int index = getElementIndex(head.getChildren(), ThemesConstants.TAG_ATTRIBUTE_TYPE, "text/css");
		// Adding enabled styles
		List <ThemeStyleGroupMember> members = getEnabledStyles(theme);
		for (int i = 0; i < members.size(); i++) {
			head.addContent(index, getNewStyleElement(members.get(i)));
			index++;
		}
		
		if (!uploadDocument(doc, theme.getLinkToBase(), helper.getFileNameWithExtension(theme.getLinkToSkeleton()), theme)) {
			return false;
		}
		
		return true;
	}
	
	/**
	 * 
	 * @param theme
	 * @return
	 */
	public boolean prepareThemeStyleFiles(ThemeInfo theme) {
		if (!theme.isNewTheme()) {
			return true; // Theme allready prepared
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
		String addToCss = helper.extractValueFromString(theme.getLinkToBase(), index, theme.getLinkToBase().length());
		while (it.hasNext()) {
			member = (ThemeStyleGroupMember) it.next();
			files = member.getStyleFiles();
			for (index = 0; index < files.size(); index++) {
				if (!proceedStyleFile(theme.getLinkToBase() + files.get(index), addToCss)) {
					return false;
				}
			}
		}
		return true;
	}
	
	private boolean proceedStyleFile(String linkToStyle, String addToCss) {
		// Getting css file
		InputStream is = helper.getInputStream(helper.getFullWebRoot() + linkToStyle);
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
		if (content.indexOf(CSS_REPLACE) == -1) {
			return true;
		}

		while (content.indexOf(CSS_REPLACE) != -1) {
			content = content.replace(CSS_REPLACE, CSS_IMAGE_URL + DIRECTORIES + addToCss + IMAGES);
		}

		try {
			if (!helper.getSlideService().uploadFileAndCreateFoldersFromStringAsRoot(helper.getLinkToBase(linkToStyle), helper.getFileNameWithExtension(linkToStyle), content, null, true)) {
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
	private boolean uploadDocument(Document doc, String linkToBase, String fileName, ThemeInfo theme) {
		XMLOutputter out = new XMLOutputter();
		String docContent = addRegions(out.outputString(doc));
		docContent = getFixedDocumentContent(docContent);
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
			if (documentContent.indexOf(ThemesConstants.USELESS_CONTENT.get(i)) != -1) {
				documentContent = documentContent.replaceAll(ThemesConstants.USELESS_CONTENT.get(i), ThemesConstants.EMPTY);
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
			log.info("head is null");
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
				if (needAddRegion(ThemesConstants.REGIONS, e.getTextNormalize())) {
					//elementsNeedsRegions.add(e);
				}
				else {
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
			e.setText(ThemesConstants.EMPTY);
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
			log.info("root is null");
			return false;
		}
		List nodes = null;
		JDOMXPath xp = null;
		try {
			xp = new JDOMXPath(ThemesConstants.DIV_TAG_INSTRUCTION);
			xp.addNamespace(ThemesConstants.NAMESPACE_ID, ThemesConstants.NAMESPACE);
			nodes = xp.selectNodes(root);
		} catch (JaxenException e1) {
			log.error(e1);
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
	
	/**
	 * Creates String, thats represens Builder's region
	 * @param value
	 * @return String
	 */
	private String getRegion(String value) {
		String region = ThemesConstants.COMMENT_BEGIN + ThemesConstants.TEMPLATE_REGION_BEGIN + value +
			ThemesConstants.TEMPLATE_REGION_MIDDLE + ThemesConstants.COMMENT_END + ThemesConstants.COMMENT_BEGIN +
			ThemesConstants.TEMPLATE_REGION_END + ThemesConstants.COMMENT_END;
		
		if (value.equals(TITLE)) {
			return region + TITLE_REPLACE;
		}
		if (value.equals(SLOGAN)) {
			return region + SLOGAN_REPLACE;
		}
		if (value.equals(TOOLBAR)) {
			return region + TOOLBAR_REPLACE;
		}
		if (value.equals(SIDEBAR)) {
			return region + SIDEBAR_REPLACE;
		}
		if (value.equals(BREADCRUMB)) {
			return region + BREADCRUMB_REPLACE;
		}
		if (value.equals(FOOTER)) {
			return region + FOOTER_REPLACE;
		}
		
		return region;
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
			log.info("Element is null");
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
	public String changeTheme(String themeID, String styleGroupName, String styleMember, boolean radio, boolean checked) {
		if (themeID == null || styleGroupName == null || styleMember == null) {
			return null;
		}
		
		ThemeInfo theme = helper.getThemeInfo(themeID);
		
		String linkToDoc = theme.getLinkToDraft();
		if (linkToDoc == null) {
			linkToDoc = theme.getLinkToSkeleton();
		}
		
		Document doc = helper.getXMLDocument(helper.getFullWebRoot() + linkToDoc);
		if (doc == null) {
			return null;
		}
		
		ThemeStyleGroupMember oldStyle = null;
		ThemeStyleGroupMember newStyle = null;
		if (radio) {
			oldStyle = getEnabledStyleMember(theme, styleGroupName);
			newStyle = getStyleMember(theme, styleGroupName, styleMember);
		}
		else {
			if (checked) {
				newStyle = getStyleMember(theme, styleGroupName, styleMember);
			}
			else {
				oldStyle = getStyleMember(theme, styleGroupName, styleMember);
			}
		}

		if (oldStyle == null) {
			return null;
		}
		
		Element root = doc.getRootElement();
		if (!changeThemeStyle(root.getChild("head", namespace), oldStyle, newStyle)) {
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
		if (!uploadDocument(doc, theme.getLinkToBase(), draft, theme)) {
			return null;
		}

		if (helper.getPreviewGenerator().generatePreview(helper.getFullWebRoot() + theme.getLinkToDraft(), ThemesConstants.PREVIEW_IMAGE, theme.getLinkToBase(), 800, 600)) {
			theme.setLinkToPreview(ThemesConstants.PREVIEW_IMAGE + ThemesConstants.DOT + helper.getPreviewGenerator().getFileType());
			return themeID;
		}
		return null;
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
		attributes.add(new Attribute("rel", "stylesheet"));
		attributes.add(new Attribute(ThemesConstants.TAG_ATTRIBUTE_TYPE, "text/css"));
		attributes.add(new Attribute("media", "screen"));
		return attributes;
	}
	
	/**
	 * Gets enabled style group member (theme's variation)
	 * @param theme
	 * @param styleGroupName
	 * @return ThemeStyleGroupMember
	 */
	private ThemeStyleGroupMember getEnabledStyleMember(ThemeInfo theme, String styleGroupName) {
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
	private List <ThemeStyleGroupMember> getEnabledStyles(ThemeInfo theme) {
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
	 * @param newStyleMember
	 * @return ThemeStyleGroupMember
	 */
	private ThemeStyleGroupMember getStyleMember(ThemeInfo theme, String styleGroupName, String newStyleMember) {
		Map <String, ThemeStyleGroupMember> styleMembers = theme.getStyleGroupsMembers();
		int i = 0;
		ThemeStyleGroupMember member = styleMembers.get(styleGroupName + ThemesConstants.AT + i);
		while (member != null) {
			if (newStyleMember.equals(member.getName())) {
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
	 * @return boolean
	 */
	public boolean saveTheme(String themeID, String themeName) {
		if (themeID == null) {
			return false;
		}
		
		ThemeInfo theme = helper.getThemeInfo(themeID);
		
		if (!theme.getName().equals(themeName)) {
			log.info("Create a new theme");
			// TODO: new theme under old
			return true;
		}
		
		if (theme.getLinkToDraft() == null) {
			log.info("No draft for theme: " + themeID);
			return false;
		}

		InputStream is = null;
		is = helper.getInputStream(helper.getFullWebRoot() + theme.getLinkToDraft());
		
		String fileName = helper.getFileNameWithExtension(theme.getLinkToSkeleton());
		theme.setLocked(true);
		try {
			if (!helper.getSlideService().uploadFileAndCreateFoldersFromStringAsRoot(theme.getLinkToBase(), fileName, is, null, true)) {
				return false;
			}
		} catch (RemoteException e) {
			log.error(e);
			return false;
		} finally {
			helper.closeInputStream(is);
		}
		theme.setLocked(false);
		
		theme.setLinkToDraft(null);

		return true;
	}

}
