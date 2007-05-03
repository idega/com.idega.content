package com.idega.content.themes.helpers;

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.rmi.RemoteException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
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

import com.idega.content.business.ContentConstants;
import com.idega.idegaweb.IWMainApplication;
import com.idega.idegaweb.IWMainApplicationSettings;
import com.idega.util.StringHandler;

public class ThemeChanger {
	
	private static final Log log = LogFactory.getLog(ThemeChanger.class);
	
	// These are defaults in RapidWeaver, we are using its to generate good preview
	private static final String TOOLBAR_REPLACE_BEGIN = "<ul><li><a href=\"index.html\" rel=\"self\" id=\"current\">";
	private static final String TOOLBAR_REPLACE_END = "</a></li></ul>";
	
	private static final String IMAGE_START = "<img src=";
	private static final String IMAGE_POSITION = "style=\"margin: 2px; float: ";
	private static final String IMAGE_END = " />";
	private static final String CONTENT_PARAGRAPH_TITLE = "<div class=\"blog-entry\"><div class=\"blog-entry-title\">";
	private static final String CONTENT_PARAGRAPH_DATE = "</div><div class=\"blog-entry-date\">";
	private static final String CONTENT_PARAGRAPH_LINK = "<span class=\"blog-entry-permalink\"> | <a>Permalink</a></span></div><div class=\"blog-entry-body\">";
	private static final String CONTENT_PARAGRAPH_START = "<p style=\"font-family: Verdana,Arial,Helvetica,sans-serif\">";
	private static final String CONTENT_PARAGRAPH_END = "</p></div></div>";
	private static final String CONTENT_BEGIN = "<div class=\"contentSpacer\"></div>";
	private static final String CONTENT_END = "<div class=\"clear\"></div>\n<div class=\"clearer\"></div>";
	
	// Default keywords
	private static final String FOOTER = "footer";
	private static final String CONTENT = "content";
	
	// These are defaults in RapidWeaver style files, we need to change directories to images
	private static final String CSS_IMAGE_URL = "url(";
	private static final String DIRECTORY = "../";
	private static final String IMAGES = "images";
	private static final String CUSTOM_CSS_REPLACE = CSS_IMAGE_URL + DIRECTORY + DIRECTORY + IMAGES;
	private static final String[] CUSTOM_REPLACE = new String[] {CUSTOM_CSS_REPLACE};

	// Incorrect CSS syntax needs to be replaced
	private static final String[] HREF_REPLACE = new String[] {"href^=", "href$="};
	private static final String HREF_REPLACEMENT = "href~=";
	private static final String[] IMAGE_URL_REPLACE = new String[] {"url(images"};
	private static final String[] HTML_REPLACE = new String[] {">html"};
	
	private static final String HTML_HEAD = "head";
	private static final String HTML_BODY = "body";
	
	// Element's attributes and values
	private static final String TAG_ATTRIBUTE_REL = "rel";
	private static final String TAG_ATTRIBUTE_MEDIA = "media";
	private static final String TAG_ATTRIBUTE_VALUE_STYLESHEET = "stylesheet";
	private static final String TAG_ATTRIBUTE_VALUE_CSS = "text/css";
	private static final String TAG_ATTRIBUTE_VALUE_SCREEN = "screen";
	
	private static final String COPY_AND_SPACE = ContentConstants.EMPTY;//"&copy;&nbsp;";
	
	private static final String ELEMENT_SCRIPT_NAME = "script";
	private static final String ELEMENT_LINK_NAME = "link";
	
	private static final int THEME_HEIGHT = 350;
	
	private static final String REGION_TO_EXPAND = "contentContainer";
	
	private static final String IDEGA_COMMENT = "idega";
	
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
	protected boolean prepareThemeForUsage(Theme theme) {
		if (theme == null) {
			return false;
		}
		
		if (!theme.isNewTheme()) {
			return true; // Theme allready prepared
		}
		
		String skeleton = theme.getLinkToSkeleton();
		if (skeleton.indexOf(ThemesConstants.SPACE) != -1) {
			skeleton = helper.urlEncode(skeleton);
		}
		
		Document doc = helper.getXMLDocument(new StringBuffer(helper.getFullWebRoot()).append(skeleton).toString());
		if (doc == null) {
			return false;
		}
		Element root = doc.getRootElement();
		Element head = root.getChild(HTML_HEAD, namespace);
		
		String path = new StringBuffer(ContentConstants.CONTENT).append(theme.getLinkToBase()).toString();
		
		// Removing needles content (like "%pathto")
		if (!proceedHeadContent(path, head)) {
			return false;
		}
		
		// Adding IBPage regions
		if (!proceedBodyContent(path, root.getChild(HTML_BODY, namespace))) {
			return false;
		}
		
		// Adding enabled styles
		List <ThemeStyleGroupMember> members = getEnabledStyles(theme);
		// Finding where to insert element
		int index = getElementIndex(head.getContent(), ThemesConstants.TAG_ATTRIBUTE_TYPE, TAG_ATTRIBUTE_VALUE_CSS);
		for (int i = 0; i < members.size(); i++) {
			addContentToElement(head, getNewStyleElement(path, members.get(i)), index);
			index++;
		}
		
		if (uploadDocument(doc, theme.getLinkToBaseAsItIs(), helper.getFileNameWithExtension(theme.getLinkToSkeleton()), theme,	true)) {
			return true;
		}
		
		return false;
	}
	
	private boolean addContentToElement(Element parent, Collection<Element> content, int index) {
		if (parent == null || content == null) {
			return false;
		}
		if (index < 0) {
			return false;
		}
		if (index > parent.getContentSize()) {
			parent.addContent(content);
		}
		else {
			parent.addContent(index, content);
		}
		return true;
	}
	
	/**
	 * 
	 * @param theme
	 * @return
	 */
	protected boolean prepareThemeStyleFiles(Theme theme) {
		if (theme == null) {
			return false;
		}
		
		if (!theme.isNewTheme()) {
			return true; // Theme allready prepared
		}
		
		prepareThemeDefaultStyleFiles(theme);
		
		Map styles = theme.getStyleGroupsMembers();
		if (styles == null) {
			return false;
		}

		ThemeStyleGroupMember member = null;
		List <String> files = null;
		int index = theme.getLinkToBase().indexOf(ThemesConstants.THEMES);
		if (index != -1) {
			index++;
		}
		
		// Constructing correct path to images folder
		StringBuffer replacement = new StringBuffer();
		replacement.append(CSS_IMAGE_URL);
		replacement.append(ContentConstants.CONTENT);
		replacement.append(theme.getLinkToBase());
		replacement.append(IMAGES);
		
		Replaces[] r = new Replaces[]{getReplace(CUSTOM_REPLACE, replacement.toString()), getReplace(IMAGE_URL_REPLACE,
				replacement.toString()), getReplace(HTML_REPLACE, ThemesConstants.EMPTY)};
		
		for (Iterator it = styles.values().iterator(); it.hasNext(); ) {
			member = (ThemeStyleGroupMember) it.next();
			files = member.getStyleFiles();
			for (index = 0; index < files.size(); index++) {
				proceedStyleFile(new StringBuffer(theme.getLinkToBase()).append(files.get(index)).toString(), r);
			}
		}
		return true;
	}
	
	private Replaces getReplace(String[] whatToReplace, String replacement) {
		if (whatToReplace == null || replacement == null) {
			return null;
		}
		Replaces replace = new Replaces();
		replace.setReplaces(whatToReplace);
		replace.setReplacement(replacement);
		return replace;
	}
	
	private boolean prepareThemeDefaultStyleFiles(Theme theme) {
		List <String> defaultStyles = ThemesConstants.DEFAULT_STYLE_FILES;
		StringBuffer replacement = new StringBuffer();
		replacement.append(CSS_IMAGE_URL).append(ContentConstants.CONTENT).append(theme.getLinkToBase()).append(IMAGES);
		Replaces[] r = new Replaces[]{getReplace(HREF_REPLACE, HREF_REPLACEMENT), getReplace(IMAGE_URL_REPLACE,
				replacement.toString()), getReplace(HTML_REPLACE, ThemesConstants.EMPTY)};	
		for (int i = 0; i < defaultStyles.size(); i++) {
			if (!proceedStyleFile(new StringBuffer(theme.getLinkToBase()).append(defaultStyles.get(i)).toString(), r)) {
				return false;
			}
		}
		return true;
	}
	
	private boolean proceedStyleFile(String linkToStyle, Replaces[] replaces) {
		if (linkToStyle == null || replaces == null) {
			return false;
		}
		
		// Getting css file
		InputStream is = helper.getInputStream(new StringBuffer(helper.getFullWebRoot()).append(linkToStyle).toString());
		if (is == null) {
			log.error(new StringBuilder("Cann't get CSS file: '").append(linkToStyle).append("' from Theme pack!"));
			return false;
		}
		InputStreamReader isr = new InputStreamReader(is);
		BufferedReader buf = new BufferedReader(isr);
		CssScanner scanner = new CssScanner(buf);
		
		helper.closeInputStream(is);
		helper.closeInputStreamReader(isr);
		helper.closeBufferedReader(buf);
		
		StringBuffer result = scanner.getResultBuffer();
		if (result == null) {
			return true;
		}
		boolean needToReplace = scanner.isNeedToReplace();
		
		// Changing content
		String content = result.toString();
		String[] whatToReplace = null;
		String replacement = null;
		for (int i = 0; i < replaces.length; i++) {
			whatToReplace = replaces[i].getReplaces();
			replacement = replaces[i].getReplacement();
			if (whatToReplace != null && replacement != null) {
				for (int j = 0; j < whatToReplace.length; j++) {
					while (content.indexOf(whatToReplace[j]) != -1) {
						content = content.replace(whatToReplace[j], replacement);
						needToReplace = true;
					}
				}
			}
		}

		if (!needToReplace) {
			return true;
		}

		// Uploading modified file
		try {
			if (helper.getSlideService().uploadFileAndCreateFoldersFromStringAsRoot(helper.getLinkToBase(helper.decodeUrl(linkToStyle)), helper.getFileNameWithExtension(linkToStyle), content, null, true)) {
				return true;
			}
		} catch (RemoteException e) {
			log.error(e);
			return false;
		}
		
		return false;
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
		Object o = null;
		for (int i = 0; i < elements.size(); i++) {
			o = elements.get(i);
			if (o instanceof Element) {
				e = (Element) o;
				value = e.getAttributeValue(attributeType);
				if (attributeValue.equals(value)) {
					index = i;
				}
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
	protected boolean uploadDocument(Document doc, String linkToBase, String fileName, Theme theme, boolean isTheme) {
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
	private boolean proceedHeadContent(String linkToBase, Element head) {
		if (linkToBase == null || head == null) {
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
		for (int i = 0; i < headElements.size(); i++) {
			o = headElements.get(i);
			if (o instanceof Element) {
				e = (Element) o;
				fixDocumentElement(e, linkToBase);
			}
			else {
				if (o instanceof Text) {
					textElements.add((Text) o);
				}
			}
		}
		
		for (Iterator<Element> ite = elementsNeedsRegions.iterator(); ite.hasNext(); ) {
			e = ite.next();
			e.addContent(getCommentsCollection(fixValue(e.getTextNormalize())));
		}

		Text t = null;
		for (Iterator <Text> itt = textElements.iterator(); itt.hasNext(); ) {
			t = itt.next();
			if (needAddRegion(ThemesConstants.REGIONS, t.getTextNormalize())) {
				head.addContent(getCommentsCollection(fixValue(t.getTextNormalize())));
			}
			t.detach();
		}
	
		// Adding fake (comment) element to <script> - to get <script ...></script> in HTML code
		List elements = head.getContent();
		Object element = null;
		Element script = null;
		if (elements != null) {
			for (int i = 0; i < elements.size(); i++) {
				element = elements.get(i);
				if (element instanceof Element) {
					script = (Element) element;
					if (ELEMENT_SCRIPT_NAME.equals(script.getName())) {
						script.addContent(getComment(IDEGA_COMMENT));
					}
				}
			}

		}
		
		return true;
	}
	
	private void fixDocumentElement(Element e, String linkToBase) {
		if (e == null || linkToBase == null) {
			return;
		}
		Attribute a = null;
		if (!needAddRegion(ThemesConstants.REGIONS, e.getTextNormalize())) {
			e.setText(fixValue(e.getTextNormalize()));
		}
		a = e.getAttribute(ThemesConstants.TAG_ATTRIBUTE_HREF);
		if (a == null) {
			a = e.getAttribute(ThemesConstants.TAG_ATTRIBUTE_SRC);
		}
		if (a != null) {
			a.setValue(new StringBuffer(linkToBase).append(fixValue(a.getValue())).toString()); // Fixing attribute's value
		}
	}
	
	/**
	 * Creates collection of Comment objets (opening and closing comment)
	 * @param commentValue
	 * @return Collection
	 */
	private Collection <Comment> getCommentsCollection(String commentValue) {
		Collection <Comment> c = new ArrayList <Comment> ();
		c.add(new Comment(new StringBuffer(ThemesConstants.TEMPLATE_REGION_BEGIN).append(commentValue).append(ThemesConstants.TEMPLATE_REGION_MIDDLE).toString()));
		c.add(new Comment(ThemesConstants.TEMPLATE_REGION_END));
		return c;
	}
	
	private Collection <Element> getElement(String type, String text, String attribute, String attributeValue) {
		Collection <Element> c = new ArrayList <Element> ();
		Element e = new Element(type, namespace);
		e.setText(text);
		if (attribute != null) {
			e.setAttribute(attribute, attributeValue);
		}
		c.add(e);
		return c;
	}
	
	private Collection<Comment> getComment(String comment) {
		Collection<Comment> comments = new ArrayList<Comment>();
		comments.add(new Comment(comment));
		return comments;
	}
	
	/**
	 * Checks if need to add a region to tag
	 * @param regions
	 * @param value
	 * @return boolean
	 */
	private boolean needAddRegion(List <String> regions, String value) {
		if (regions == null || value == null) {
			return false;
		}
		if (regions.contains(value)) {
			return true;
		}
		return false;
	}
	
	/**
	 * Adding regions to div tags like this: <!-- TemplateBeginEditable name="MyUniqueRegionId1" --><!-- TemplateEndEditable -->
	 * @param body
	 * @return boolean
	 */
	private boolean proceedBodyContent(String linkToBase, Element body) {
		if (body == null) {
			return false;
		}
		
		List nodes = null;
		JDOMXPath xp = null;
		try {
			xp = new JDOMXPath(ThemesConstants.DIV_TAG_INSTRUCTION);
			xp.addNamespace(ThemesConstants.NAMESPACE_ID, ThemesConstants.NAMESPACE);
			nodes = xp.selectNodes(body);
		} catch (JaxenException e) {
			log.error(e);
		}
		if (nodes == null) {
			return false;
		}
		for (Iterator it = nodes.iterator(); it.hasNext(); ) {
			addRegion((Element) it.next());
		}
		
		List<Text> needlessText = new ArrayList<Text>();
		List allElements = body.getContent();
		Object o = null;
		Element e = null;
		for (int i = 0; i < allElements.size(); i++) {
			o = allElements.get(i);
			if (o instanceof Text) {	// Finding Text elements - they are needless
				needlessText.add((Text) o);
			}
			else {
				if (o instanceof Element) {	// Fixing <link> and/or <script> attributes values
					e = (Element) o;
					if (ELEMENT_LINK_NAME.equals(e.getName())) {
						fixDocumentElement(e, linkToBase);
					}
					if (ELEMENT_SCRIPT_NAME.equals(e.getName())) {	// <script> tags needs advanced handling
						fixDocumentElement(e, linkToBase);
						if (!hasElementChildren(e)) {
							e.addContent(getComment(IDEGA_COMMENT));
						}
					}
				}
			}
		}
		for (int i = 0; i < needlessText.size(); i++) {	// Removing needless Text elements
			needlessText.get(i).detach();
		}
		
		return true;
	}
	
	private String getBasicReplace(String begin, String defaultValue, String end) {
		if (defaultValue == null) {
			return ThemesConstants.EMPTY;
		}
		if (ThemesConstants.EMPTY.equals(defaultValue)) {
			return ThemesConstants.EMPTY;
		}
		
		StringBuffer value = new StringBuffer();
		if (begin != null) {
			value.append(begin);
		}
		if (defaultValue != null) {
			value.append(defaultValue);
		}
		if (end != null) {
			value.append(end);
		}
		return value.toString();
	}
	
	private Collection <Element> getNavigatorContent(String propertyKey, boolean addID) {
		if (propertyKey == null) {
			return new ArrayList<Element>();
		}
		IWMainApplicationSettings settings  = IWMainApplication.getDefaultIWMainApplication().getSettings();
		if (settings == null) {
			return new ArrayList<Element>();
		}
		String propertyValue = settings.getProperty(new StringBuffer(ThemesConstants.THEMES_PROPERTY_START).append(propertyKey).append(ThemesConstants.THEMES_PROPERTY_END).toString());
		if (propertyValue == null) {
			return new ArrayList<Element>();
		}
		if (ThemesConstants.EMPTY.equals(propertyValue)) {
			return new ArrayList<Element>();
		}
		
		Collection <Element> container = new ArrayList<Element>();
		Collection <Element> pages = new ArrayList<Element>();
		Collection <Element> linkToPage = null;
		
		String[] elements = propertyValue.split(ThemesConstants.COMMA);
		
		Element listContainer = new Element("ul");
		Element listElement = null;
		Element link = null;
		
		String LI = "li";
		String A = "a";
		String ID = "id";
		String CURRENT = "current";
		
		for (int i = 0; i < elements.length; i++) {
			listElement = new Element(LI);
			link = new Element(A);
			if (addID) {
				if (i == 0) {
					link.setAttribute(ID, CURRENT);
				}
			}
			link.setText(elements[i]);
			linkToPage = new ArrayList<Element>();
			linkToPage.add(link);
			listElement.setContent(linkToPage);
			pages.add(listElement);
		}
		listContainer.setContent(pages);
		container.add(listContainer);
		return container;
	}
	
	private String getContentReplace(String defaultValue) {
		StringBuffer content = new StringBuffer();
		if (defaultValue != null && !ThemesConstants.EMPTY.equals(defaultValue)) {
			content.append(defaultValue).append(ThemesConstants.NEW_LINE);
		}
		Date d = new Date();
		content.append(CONTENT_BEGIN).append(ThemesConstants.NEW_LINE);
		for (int i = 0; i < ThemesConstants.DUMMY_ARTICLES.size(); i++) {
			content.append(CONTENT_PARAGRAPH_TITLE).append(ThemesConstants.ARTICLE_TITLE);
			if (ThemesConstants.DUMMY_ARTICLES.size() > 1) {
				content.append(ThemesConstants.SPACE).append(i + 1);
			}
			content.append(CONTENT_PARAGRAPH_DATE);
			content.append(d);
			content.append(CONTENT_PARAGRAPH_LINK).append(CONTENT_PARAGRAPH_START);
			content.append(IMAGE_START).append(ThemesConstants.SINGLE_QUOTE);
			content.append(ThemesConstants.BASE_THEME_IMAGES);
			content.append(ThemesConstants.THEME_IMAGES.get(helper.getRandomNumber(ThemesConstants.THEME_IMAGES.size())));
			content.append(ThemesConstants.SINGLE_QUOTE).append(ThemesConstants.SPACE).append(IMAGE_POSITION);
			content.append(ThemesConstants.IMAGE_POSITIONS.get(helper.getRandomNumber(ThemesConstants.IMAGE_POSITIONS.size())));
			content.append(ThemesConstants.SINGLE_QUOTE).append(IMAGE_END);
			content.append(ThemesConstants.DUMMY_ARTICLES.get(i)).append(CONTENT_PARAGRAPH_END);
		}
		content.append(CONTENT_END).append(ThemesConstants.NEW_LINE);
		return content.toString();
	}
	
	/**
	 * Creates String, thats represents Builder's region
	 * @param value
	 * @return String
	 */
	private String getRegion(String value) {
		StringBuffer region = new StringBuffer(ThemesConstants.COMMENT_BEGIN).append(ThemesConstants.TEMPLATE_REGION_BEGIN);
		region.append(value).append(ThemesConstants.TEMPLATE_REGION_MIDDLE).append(ThemesConstants.COMMENT_END);
		
		IWMainApplicationSettings settings  = IWMainApplication.getDefaultIWMainApplication().getSettings();
		StringBuffer key = new StringBuffer(ThemesConstants.THEMES_PROPERTY_START).append(value);
		key.append(ThemesConstants.THEMES_PROPERTY_END);
		String propertyValue = settings.getProperty(key.toString());
		if (propertyValue != null) {
			if (value.equals(ThemesConstants.TOOLBAR)) {
				region.append(getBasicReplace(TOOLBAR_REPLACE_BEGIN, propertyValue, TOOLBAR_REPLACE_END));
				region.append(ThemesConstants.COMMENT_BEGIN).append(ThemesConstants.TEMPLATE_REGION_END);
				region.append(ThemesConstants.COMMENT_END);
				return region.toString();
			}
			if (value.equals(FOOTER)) {
				region.append(COPY_AND_SPACE).append(getBasicReplace(null, propertyValue, null));
				region.append(ThemesConstants.COMMENT_BEGIN).append(ThemesConstants.TEMPLATE_REGION_END);
				region.append(ThemesConstants.COMMENT_END);
				return region.toString();
			}
			if (value.equals(CONTENT)) {
				region.append(getContentReplace(propertyValue)).append(ThemesConstants.COMMENT_BEGIN);
				region.append(ThemesConstants.TEMPLATE_REGION_END).append(ThemesConstants.COMMENT_END);
				return region.toString();
			}
			region.append(propertyValue);
		}
		region.append(ThemesConstants.COMMENT_BEGIN).append(ThemesConstants.TEMPLATE_REGION_END).append(ThemesConstants.COMMENT_END);
		return region.toString();
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
			if (docContent.indexOf(ThemesConstants.REGIONS.get(i)) != -1) {
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
	 * Adds region to div tag if div tag has proper id attribute
	 * @param e
	 * @return boolean
	 */
	private boolean addRegion(Element e) {
		if (e == null) {
			return false;
		}
		
		String regionID = e.getAttributeValue(ThemesConstants.TAG_ATTRIBUTE_ID);
		
		if (needAddRegion(ThemesConstants.BASIC_IDS_FOR_REGIONS, regionID)) {
			e.addContent(0, getCommentsCollection(regionID));
			if (ThemesConstants.NAVIGATION.equals(regionID)) {
				e.addContent(1, getNavigatorContent(regionID, true));
			}
			if (ThemesConstants.BREADCRUMB.equals(regionID)) {
				e.addContent(1, getNavigatorContent(regionID, false));
			}
		}
		
		fixSiteRegion(e, "h1", ThemesConstants.SITE_TITLE);
		fixSiteRegion(e, "h2", ThemesConstants.SITE_SLOGAN);
		
		if (regionID != null) {
			if (regionID.equals(REGION_TO_EXPAND)) {
				e.addContent(getElement("div", "idega_theme", "style", new StringBuffer("height:").append(THEME_HEIGHT).append("px;visibility:hidden").toString())); // Expanding theme
			}
		}
		
		if (!hasElementChildren(e)) {
			e.addContent(getComment(IDEGA_COMMENT));
		}
		
		return true;
	}
	
	private boolean hasElementChildren(Element e) {
		if (e == null) {
			return false;
		}
		List children = e.getChildren();
		if (children == null) {
			return false;
		}
		if (children.size() == 0) {
			return false;
		}
		return true;
	}
	
	private boolean fixSiteRegion(Element e, String heading, String headingKeyword) {
		if (e == null || heading == null) {
			return false;
		}
		if (detachElement(e, heading)) {
			e.addContent(getCommentsCollection(headingKeyword));
			addElementToRegion(e, e.getContentSize() - 1, heading, headingKeyword);	
			return true;
		}
		return false;
	}
	
	private boolean addElementToRegion(Element e, int index, String elementName, String applicationPropertyKey) {
		if (e == null || elementName == null || applicationPropertyKey == null) {
			return false;
		}
		if (index < 0) {
			index = 0;
		}
		e.addContent(index, getSimpleTextElement(elementName, applicationPropertyKey));
		return true;
	}
	
	private boolean detachElement(Element parent, String elementName) {
		if (parent == null || elementName == null) {
			return false;
		}
		Element useless = parent.getChild(elementName, namespace);
		if (useless != null) {
			useless.detach();
			return true;
		}
		return false;
	}
	
	private Collection <Element> getSimpleTextElement(String containerName, String propertyKey) {
		if (propertyKey == null) {
			return new ArrayList<Element>();
		}
		IWMainApplicationSettings settings  = IWMainApplication.getDefaultIWMainApplication().getSettings();
		if (settings == null) {
			return new ArrayList<Element>();
		}
		String propertyValue = settings.getProperty(new StringBuffer(ThemesConstants.THEMES_PROPERTY_START).append(propertyKey).append(ThemesConstants.THEMES_PROPERTY_END).toString());
		if (propertyValue == null) {
			return new ArrayList<Element>();
		}
		if (ThemesConstants.EMPTY.equals(propertyValue)) {
			return new ArrayList<Element>();
		}
		Collection <Element> mainContainer = new ArrayList<Element>();
		Element parent = new Element(containerName, namespace);
		
		Collection <Text> textContainer = new ArrayList<Text>();
		Text text = new Text(propertyValue);
		
		textContainer.add(text);
		parent.addContent(textContainer);
		mainContainer.add(parent);
		
		return mainContainer;
	}
	
	private boolean clearThemeVariationsFromCache(String themeID) {
		return helper.clearVariationFromCache(themeID);
	}
	
	/**
	 * Changes theme with new style variation, creates draft and creates new preview image
	 * @param themeID
	 * @param styleGroupName
	 * @param variation
	 * @return String
	 */
	public String changeTheme(String themeID, String styleGroupName, String variation, String themeName, boolean radio, boolean checked) {		
		if (themeID == null) {
			return null;
		}
		Theme theme = helper.getTheme(themeID);
		if (theme == null) {
			return null;
		}
		Document doc = getThemeDocument(theme.getId());
		
		String changed = changeTheme(doc, theme, styleGroupName, variation, themeName, radio, checked);
		if (changed == null) {
			return null;
		}
		
		if (finishThemeChange(theme, doc)) {
			return themeID;
		}
		return null;
	}
	
	private boolean finishThemeChange(Theme theme, Document doc) {
		String draft = new StringBuffer(helper.getFileName(theme.getLinkToSkeleton())).append(ThemesConstants.DRAFT).toString();
		theme.setLinkToDraft(new StringBuffer(theme.getLinkToBase()).append(draft).toString());
		if (!uploadDocument(doc, theme.getLinkToBaseAsItIs(), helper.decode(draft, true), theme, true)) {
			return false;
		}

		if (!helper.generatePreviewsForTheme(theme, true, true, 1f)) {
			return false;
		}
		
		clearThemeVariationsFromCache(theme.getId());
		
		return true;
	}
	
	private String changeTheme(Document doc, Theme theme, String styleGroupName, String variation, String themeName, boolean radio, boolean checked) {
		if (doc == null) {
			return null;
		}
		if (theme == null || styleGroupName == null || variation == null) {
			return null;
		}
		
		theme.setChangedName(themeName);
		
		boolean limitedSelection = true;
		
		ThemeStyleGroupMember oldStyle = null;
		ThemeStyleGroupMember newStyle = null;
		ThemeStyleGroupMember styleChanger = null;
		if (radio) { // Simply changing CSS files
			oldStyle = getEnabledStyleMember(theme, styleGroupName);
			newStyle = getStyleMember(theme, styleGroupName, variation);
			styleChanger = oldStyle;
		}
		else { //Need to know either add CSS or remove
			limitedSelection = false;
			if (checked) { // Need to add
				newStyle = getStyleMember(theme, styleGroupName, variation);
				styleChanger = newStyle;
			}
			else { //Need to remove
				oldStyle = getStyleMember(theme, styleGroupName, variation);
				styleChanger = oldStyle;
			}
		}
		
		Element root = doc.getRootElement();
		if (root == null) {
			return null;
		}
		String linkToBase = new StringBuffer(ContentConstants.CONTENT).append(theme.getLinkToBase()).toString();
		/*if (!availableStyleMember(linkToBase, newStyle)){
			return null;
		}*/
		if (!changeThemeStyle(linkToBase, root.getChild(HTML_HEAD, namespace), oldStyle,
				newStyle)) {
			return null;
		}
		if (oldStyle != null) {
			oldStyle.setEnabled(false);
		}
		if (newStyle != null) {
			newStyle.setEnabled(true);
		}
		
		addThemeChange(theme, styleChanger, limitedSelection);
		
		return theme.getId();
	}

	private void addThemeChange(Theme theme, ThemeStyleGroupMember style, boolean limitedSelection) {
		if (theme == null || style == null) {
			return;
		}
		ThemeChange change = new ThemeChange();
		change.setLimitedSelection(limitedSelection);
		change.setEnabled(style.isEnabled());
		change.setStyleGroupName(style.getGroupName());
		change.setVariation(style.getName());
		theme.addThemeChange(change);
	}
	
	/**
	 * Changes theme's old style with new
	 * @param head
	 * @param oldStyle
	 * @param newStyle
	 * @return boolean
	 */
	private boolean changeThemeStyle(String linkToBase, Element head, ThemeStyleGroupMember oldStyle,
			ThemeStyleGroupMember newStyle) {
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
			List <String> files = null;
			boolean foundStyle = false;
			for (int i = 0; i < styles.size(); i++) {
				style = (Element) styles.get(i);
				attributeValue = style.getAttributeValue(ThemesConstants.TAG_ATTRIBUTE_HREF);
				files = oldStyle.getStyleFiles();
				foundStyle = false;
				if (files != null) {
					for (int j = 0; (j < files.size() && !foundStyle); j++) {
						if (attributeValue.indexOf(files.get(j)) != -1) {
							foundStyle = true;
							uselessStyles.add(style);
							index = getElementIndex(head.getContent(), ThemesConstants.TAG_ATTRIBUTE_HREF, attributeValue);
						}
					}
				}
			}
			
		}
		
		if (newStyle != null) {
			addContentToElement(head, getNewStyleElement(linkToBase, newStyle), index);
		}
		
		for (Iterator <Element> it = uselessStyles.iterator(); it.hasNext(); ) {
			it.next().detach();
		}
		
		return true;
	}
	
	/**
	 * Creates new style element: <link href="..." ... />
	 * @param newStyle
	 * @return Collection
	 */
	private Collection <Element> getNewStyleElement(String linkToBase, ThemeStyleGroupMember newStyle) {
		Collection <Element> newStyleElements = new ArrayList <Element> ();
		if (linkToBase == null || newStyle == null) {
			return newStyleElements;
		}
		List <Attribute> attributes = null;
		Element newStyleHref = null;
		for (int i = 0; i < newStyle.getStyleFiles().size(); i++) {
			attributes = getBasicAttributesList();			
			attributes.add(new Attribute(ThemesConstants.TAG_ATTRIBUTE_HREF, new StringBuffer(linkToBase).append(newStyle.getStyleFiles().get(i)).toString()));

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
		ThemeStyleGroupMember member = getMember(styleMembers, styleGroupName, i);
		while (member != null) {
			if (member.isEnabled()) {
				return member;
			}
			i++;
			member = getMember(styleMembers, styleGroupName, i);
		}
		
		return null;
	}
	
	/**
	 * Gets enabled style groups members (theme's variations)
	 * @param theme
	 * @return List
	 */
	protected List <ThemeStyleGroupMember> getEnabledStyles(Theme theme) {
		List <ThemeStyleGroupMember> members = new ArrayList<ThemeStyleGroupMember>();
		List <String> groupNames = theme.getStyleGroupsNames();
		ThemeStyleGroupMember member = null;
		String styleGroupName = null;
		Map <String, ThemeStyleGroupMember> styleMembers = theme.getStyleGroupsMembers();
		for (int i = 0; i < groupNames.size(); i++) {
			styleGroupName = groupNames.get(i);
			int j = 0;
			member = getMember(styleMembers, styleGroupName, j);
			while (member != null) {
				if (member.isEnabled()) {
					members.add(member);
				}
				j++;
				member = getMember(styleMembers, styleGroupName, j);
			}
		}
		return members;
	}
	
	protected ThemeStyleGroupMember getMember(Map <String, ThemeStyleGroupMember> styleMembers, String styleGroupName, int index) {
		if (styleMembers == null || styleGroupName == null) {
			return null;
		}
		return styleMembers.get(new StringBuffer(styleGroupName).append(ThemesConstants.AT).append(index).toString());
	}
	
	/**
	 * Searches and returns style group member (theme's variation)
	 * @param theme
	 * @param styleGroupName
	 * @param styleVariation
	 * @return ThemeStyleGroupMember
	 */
	protected ThemeStyleGroupMember getStyleMember(Theme theme, String styleGroupName, String styleVariation) {
		Map <String, ThemeStyleGroupMember> styleMembers = theme.getStyleGroupsMembers();
		int i = 0;
		ThemeStyleGroupMember member = getMember(styleMembers, styleGroupName, i);
		while (member != null) {
			if (styleVariation.equals(member.getName())) {
				return member;
			}
			i++;
			member = getMember(styleMembers, styleGroupName, i);
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
		if (theme == null) {
			return false;
		}
		
		if (!theme.getName().equals(themeName)) {
			return createNewTheme(theme, themeName);
		}
		
		if (theme.getLinkToDraft() == null) {
			return false;
		}

		InputStream is = null;
		is = helper.getInputStream(new StringBuffer(helper.getFullWebRoot()).append(theme.getLinkToDraft()).toString());
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
		
		theme.setLinkToDraft(null);
		theme.setLinkToThemePreview(theme.getLinkToDraftPreview());
		theme.setLinkToDraftPreview(null);
		
		try {
			helper.getThemesService().createIBPage(theme);
		} catch (RemoteException e) {
			log.error(e);
			return false;
		}
		
		return helper.createThemeConfig(theme);
	}
	
	private boolean restoreTheme(Theme theme) {
		if (theme == null) {
			return false;
		}
		List <ThemeChange> changes = theme.getChanges();
		if (changes.size() == 0) {
			return true;
		}
		ThemeChange change = null;
		ThemeStyleGroupMember member = null;
		for (int i = 0; i < changes.size(); i++) {
			change = changes.get(i);
			member = getStyleMember(theme, change.getStyleGroupName(), change.getVariation());
			if (member != null) {
				if (change.isLimitedSelection()) {
					disableStyle(theme, member.getGroupName());
				}
				member.setEnabled(!change.isEnabled());
			}
		}
		theme.setChangedName(null);
		theme.setLinkToDraftPreview(null);
		theme.setLinkToDraft(null);
		theme.setChanges(new ArrayList<ThemeChange>());
		
		helper.createSmallImage(theme, false);
		
		clearThemeVariationsFromCache(theme.getId());
		
		return true;
	}
	
	public boolean restoreTheme(String themeID) {
		if (themeID == null) {
			return false;
		}
		Theme theme = helper.getTheme(themeID);
		if (theme == null) {
			return false;
		}
		return restoreTheme(theme);
	}
	
	private void disableStyle(Theme theme, String styleGroupName) {
		ThemeStyleGroupMember member = null;
		Collection<ThemeStyleGroupMember> styleMembers = theme.getStyleGroupsMembers().values();
		for (Iterator <ThemeStyleGroupMember> it = styleMembers.iterator(); it.hasNext(); ) {
			member = it.next();
			if (member.getGroupName().equals(styleGroupName)) {
				member.setEnabled(false);
			}
		}
	}
	
	private boolean createNewTheme(Theme parent, String newName) {
		// Copying Theme skeleton
		String linkToTheme = parent.getLinkToDraft();
		if (linkToTheme == null) {
			linkToTheme = parent.getLinkToSkeleton();
		}
		if (linkToTheme == null) {
			return false;
		}
		InputStream is = helper.getInputStream(new StringBuffer(helper.getFullWebRoot()).append(linkToTheme).toString());
		if (is == null) {
			return false;
		}
		String linkToBase = helper.getLinkToBase(linkToTheme);
		if (!linkToBase.endsWith(ContentConstants.SLASH)) {
			linkToBase = new StringBuffer(linkToBase).append(ContentConstants.SLASH).toString();
		}
		String decodedLinkToBase = helper.decodeUrl(linkToBase);
		if (!decodedLinkToBase.endsWith(ContentConstants.SLASH)) {
			decodedLinkToBase = new StringBuffer(decodedLinkToBase).append(ContentConstants.SLASH).toString();
		}
		String themeName = StringHandler.removeCharacters(new StringBuilder(newName).append(ThemesConstants.THEME).toString(), ContentConstants.SPACE, ContentConstants.UNDER);
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
		
		String themeID = helper.getThemesLoader().createNewTheme(new StringBuffer(decodedLinkToBase).append(themeName).toString(), new StringBuffer(linkToBase).append(helper.encode(themeName, true)).toString(), true, true);
		if (themeID == null) {
			return false;
		}
		Theme child = helper.getTheme(themeID);
		if (child == null) {
			return false;
		}
		child.setName(newName);

		copyTheme(parent, child);
		
		// Copying Theme preview image
		String linkToPreview = parent.getLinkToDraftPreview();
		if (linkToPreview == null) {
			linkToPreview = parent.getLinkToThemePreview();
		}
		String endodedLinkToPreview = helper.encode(linkToPreview, true);
		linkToBase = child.getLinkToBase();
		if (!linkToBase.endsWith(ContentConstants.SLASH)) {
			linkToBase = new StringBuffer(linkToBase).append(ContentConstants.SLASH).toString();
		}
		is = helper.getInputStream(new StringBuffer(helper.getFullWebRoot()).append(linkToBase).append(endodedLinkToPreview).toString());
		if (is == null) {
			return false;
		}
		String extension = helper.getFileExtension(linkToPreview);
		String fileName = new StringBuffer(child.getName()).append(ThemesConstants.THEME_PREVIEW).append(ThemesConstants.DOT).append(extension).toString();
		try {
			if (helper.getSlideService().uploadFileAndCreateFoldersFromStringAsRoot(decodedLinkToBase, fileName, is, null, true)) {
				child.setLinkToThemePreview(fileName);
			}
		} catch (RemoteException e) {
			log.error(e);
		} finally {
			helper.closeInputStream(is);
		}
		
		// Setting Theme small preview
		String url = new StringBuffer(helper.getFullWebRoot()).append(linkToBase).append(endodedLinkToPreview).toString();
		helper.createSmallImage(child, url);
		
		child.setPropertiesExtracted(true);
		restoreTheme(parent);
		
		// Creating new template
		try {
			helper.getThemesService().createIBPage(child);
		} catch (RemoteException e) {
			log.error(e);
			return false;
		}
		
		return helper.createThemeConfig(child);
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
			parentMember = getMember(styleMembers, styleGroupName, j);
			while (parentMember != null) {
				member = new ThemeStyleGroupMember(parentMember);
				child.addStyleGroupMember(new StringBuffer(styleGroupName).append(ThemesConstants.AT).append(j).toString(), member);
				j++;
				parentMember = getMember(styleMembers, styleGroupName, j);
			}
		}
	}
	
	protected XMLOutputter getXMLOutputter() {
		return out;
	}
	
	private Document getThemeDocument(String themeID) {
		if (themeID == null) {
			return null;
		}
		
		Theme theme = helper.getTheme(themeID);
		if (theme == null) {
			return null;
		}
		
		String linkToDoc = theme.getLinkToDraft();
		if (linkToDoc == null) {
			linkToDoc = theme.getLinkToSkeleton();
		}
		
		if (linkToDoc.indexOf(ThemesConstants.SPACE) != -1) {
			linkToDoc = helper.urlEncode(linkToDoc);
		}
		return  helper.getXMLDocument(new StringBuffer(helper.getFullWebRoot()).append(linkToDoc).toString());
	}
	
	public String applyMultipleChangesToTheme(String themeID, List<ThemeChange> changes, String themeName) {
		if (themeID == null || changes == null) {
			return null;
		}
		
		Theme theme = helper.getTheme(themeID);
		if (theme == null) {
			return null;
		}
		Document doc = getThemeDocument(themeID);
		if (doc == null) {
			return null;
		}
		
		String changed = themeID;
		ThemeChange change = null;
		for (int i = 0; (i < changes.size() && changed != null); i++) {
			change = changes.get(i);
			changed = changeTheme(doc, theme, change.getStyleGroupName(), change.getVariation(), themeName, change.isRadio(), change.isEnabled());
		}
		
		if (changed == null) {
			return null;
		}
		
		if (finishThemeChange(theme, doc)) {
			return themeID;
		}
		return null;
	}

}