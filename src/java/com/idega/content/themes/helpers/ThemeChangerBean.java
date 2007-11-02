package com.idega.content.themes.helpers;

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.rmi.RemoteException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.jaxen.JaxenException;
import org.jaxen.jdom.JDOMXPath;
import org.jdom.Attribute;
import org.jdom.Comment;
import org.jdom.Content;
import org.jdom.Document;
import org.jdom.Element;
import org.jdom.Namespace;
import org.jdom.Text;
import org.jdom.output.Format;
import org.jdom.output.XMLOutputter;

import com.idega.content.business.ContentConstants;
import com.idega.idegaweb.IWMainApplication;
import com.idega.idegaweb.IWMainApplicationSettings;
import com.idega.util.CoreConstants;
import com.idega.util.StringHandler;

public class ThemeChangerBean implements ThemeChanger {

	private static final Log log = LogFactory.getLog(ThemeChangerBean.class);
	
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
	
	public ThemeChangerBean() {
		out = new XMLOutputter();
		out.setFormat(Format.getPrettyFormat());
	}
	
	/**
	 * Prepares importing theme for usage (removes needless content, adds regions, extracts properties)
	 * @param theme
	 * @return boolean
	 */
	public boolean prepareThemeForUsage(Theme theme) {
		if (theme == null) {
			return false;
		}
		
		if (!theme.isNewTheme()) {
			return true; // Theme already prepared
		}
		
		String skeleton = theme.getLinkToSkeleton();
		if (skeleton.indexOf(CoreConstants.SPACE) != -1) {
			skeleton = helper.urlEncode(skeleton);
		}
		
		Document doc = helper.getXMLDocument(new StringBuffer(helper.getFullWebRoot()).append(skeleton).toString(), true);
		if (doc == null) {
			return false;
		}
		
		Element root = doc.getRootElement();
		Element head = root.getChild(HTML_HEAD, namespace);
		
		String path = new StringBuffer(CoreConstants.WEBDAV_SERVLET_URI).append(theme.getLinkToBase()).toString();
		
		// Removing needles content (like "%pathto")
		if (!proceedHeadContent(path, head)) {
			return false;
		}
		
		// Adding IBPage regions
		if (!proceedBodyContent(path, root.getChild(HTML_BODY, namespace))) {
			return false;
		}
		
		// Adding enabled styles
		if (!addDefaultEnabledStyles(theme, head, path, doc, null)) {
			return false;
		}
		
		return uploadTheme(doc, theme);
	}
	
	private boolean uploadTheme(Document doc, Theme theme) {
		return uploadDocument(doc, theme.getLinkToBaseAsItIs(), helper.getFileNameWithExtension(theme.getLinkToSkeleton()), theme,	true);
	}
	
	private boolean addDefaultEnabledStyles(Theme theme, Element head, String basePath, Document doc, List<ThemeStyleGroupMember> members) {
		if (theme == null || head == null || basePath == null || doc == null) {
			return false;
		}
		
		if (members == null) {
			members = getEnabledStyles(theme);
		}
		
		//	Finding where to insert element
		int index = getElementIndex(head.getContent(), ThemesConstants.TAG_ATTRIBUTE_TYPE, TAG_ATTRIBUTE_VALUE_CSS);
		for (int i = 0; i < members.size(); i++) {
			addContentToElement(head, getNewStyleElement(basePath, members.get(i)), index);
			index++;
		}
		
		return true;
	}
	
	private boolean addContentToElement(Element parent, Collection<Element> content, int index) {
		if (parent == null || content == null || index < 0) {
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
	public boolean prepareThemeStyleFiles(Theme theme) {
		if (theme == null) {
			return false;
		}
		
		if (!theme.isNewTheme()) {
			return true; // Theme already prepared
		}
		
		prepareThemeDefaultStyleFiles(theme);
		
		Collection<ThemeStyleGroupMember> styles = theme.getStyleGroupsMembers().values();
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
		replacement.append(CoreConstants.WEBDAV_SERVLET_URI);
		replacement.append(theme.getLinkToBase());
		replacement.append(IMAGES);
		
		Replaces[] r = new Replaces[]{getReplace(CUSTOM_REPLACE, replacement.toString()), getReplace(IMAGE_URL_REPLACE, replacement.toString()), 
				getReplace(HTML_REPLACE, ThemesConstants.EMPTY)};
		
		
		List<String> invalidFiles = new ArrayList<String>();
		int i = 0;
		for (Iterator<ThemeStyleGroupMember> it = styles.iterator(); it.hasNext(); ) {
			member = it.next();
			files = member.getStyleFiles();
			for (index = 0; index < files.size(); index++) {
				if (!proceedStyleFile(new StringBuffer(theme.getLinkToBase()).append(files.get(index)).toString(), r)) {
					invalidFiles.add(files.get(index));	//	Invalid CSS file, disabling variation
				}
			}
			i++;
		}
		removeVariationsFromTheme(theme, invalidFiles);
		
		return true;
	}
	
	private void removeVariationsFromTheme(Theme theme, List<String> invalidFiles) {
		if (theme == null || invalidFiles == null) {
			return;
		}

		Collection<ThemeStyleGroupMember> variations = theme.getStyleGroupsMembers().values();
		
		List<ThemeStyleGroupMember> variationsToRemove = new ArrayList<ThemeStyleGroupMember>();
		ThemeStyleGroupMember variation = null;
		for (Iterator<ThemeStyleGroupMember> it = variations.iterator(); it.hasNext();) {
			variation = it.next();
			for (int i = 0; i < invalidFiles.size(); i++) {
				if (variation.getStyleFiles().contains(invalidFiles.get(i))) {
					variationsToRemove.add(variation);
				}
			}
		}
		
		//	Removing disabled variations
		for (int i = 0; i < variationsToRemove.size(); i++) {
			try {
				variations.remove(variationsToRemove.get(i));
			} catch(Exception e) {
				e.printStackTrace();
			}
		}
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
		replacement.append(CSS_IMAGE_URL).append(CoreConstants.WEBDAV_SERVLET_URI).append(theme.getLinkToBase()).append(IMAGES);
		Replaces[] r = new Replaces[]{getReplace(HREF_REPLACE, HREF_REPLACEMENT), getReplace(IMAGE_URL_REPLACE, replacement.toString()),
				getReplace(HTML_REPLACE, ThemesConstants.EMPTY)};	
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
		
		// Getting CSS file
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
			if (helper.getSlideService().uploadFileAndCreateFoldersFromStringAsRoot(helper.getLinkToBase(helper.decodeUrl(linkToStyle)),
					helper.getFileNameWithExtension(linkToStyle), content, null, true)) {
				return true;
			}
		} catch (RemoteException e) {
			e.printStackTrace();
			return false;
		}
		
		return false;
	}
	
	/**
	 * 
	 * @param elements
	 * @param attributeValue
	 * @return index
	 */
	@SuppressWarnings("unchecked")
	private int getElementIndex(List contents, String attributeType, String attributeValue) {
		int index = 0;
		if (contents == null) {
			return index;
		}
		Element e = null;
		String value = null;
		Object o = null;
		for (int i = 0; i < contents.size(); i++) {
			o = contents.get(i);
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
	public boolean uploadDocument(Document doc, String linkToBase, String fileName, Theme theme, boolean isTheme) {
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
			e.printStackTrace();
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
	 * RapidWeaver theme consists in-proper data for valid XHTML document, so needs to be fixed
	 * @param head
	 * @return boolean
	 */
	@SuppressWarnings("unchecked")
	private boolean proceedHeadContent(String linkToBase, Element head) {
		if (linkToBase == null || head == null) {
			return false;
		}
		List headElements = head.getContent();
		if (headElements == null) {
			return false;
		}
		
		Object o = null;
		List<Text> textElements = new ArrayList<Text>();
		List<Element> elementsNeedsRegions = new ArrayList<Element>();
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
			String fixedValue = fixValue(a.getValue());
			if (!fixedValue.startsWith(linkToBase)) {
				a.setValue(new StringBuffer(linkToBase).append(fixedValue).toString()); // Fixing attribute's value
			}
		}
	}
	
	/**
	 * Creates collection of Comment objects (opening and closing comment)
	 * @param commentValue
	 * @return Collection
	 */
	private Collection <Comment> getCommentsCollection(String commentValue) {
		Collection <Comment> c = new ArrayList <Comment> ();
		c.add(new Comment(new StringBuffer(ThemesConstants.TEMPLATE_REGION_BEGIN).append(commentValue).append(ThemesConstants.TEMPLATE_REGION_MIDDLE).toString()));
		c.add(new Comment(ThemesConstants.TEMPLATE_REGION_END));
		return c;
	}
	
	private Collection <Element> getExpandRegion() {
		Collection <Element> c = new ArrayList <Element> ();
		
		//	Region
		Element region = new Element("div", namespace);
		String regionName = "idegaThemeExpandRegion";
		region.setAttribute("id", regionName);
		Collection<Content> regionContent = new ArrayList<Content>();
		regionContent.addAll(getCommentsCollection(regionName));

		//	Expander
		Element expander = new Element("div", namespace);
		expander.setText("idegaTheme");
		expander.setAttribute("style", new StringBuffer("height:").append(THEME_HEIGHT).append("px;visibility:hidden").toString());
		regionContent.add(expander);
		
		region.addContent(regionContent);
		c.add(region);
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
	 * Adding regions to DIV tags like this: <!-- TemplateBeginEditable name="MyUniqueRegionId1" --><!-- TemplateEndEditable -->
	 * @param body
	 * @return boolean
	 */
	@SuppressWarnings("unchecked")
	private boolean proceedBodyContent(String linkToBase, Element body) {
		if (body == null) {
			return false;
		}
		
		Object o = null;
		Element e = null;
		
		//	DIVs
		List divs = getNodesByXpath(body, ThemesConstants.DIV_TAG_INSTRUCTION);
		if (divs == null) {
			return false;
		}
		for (Iterator it = divs.iterator(); it.hasNext(); ) {
			o = it.next();
			if (o instanceof Element) {
				addRegion((Element) o);
			}
		}
		
		//	OBJECTs
		o = null;
		List objects = getNodesByXpath(body, ThemesConstants.OBJECT_TAG_INSTRUCTION);
		if (objects != null) {
			for (int i = 0; i < objects.size(); i++) {
				o = objects.get(i);
				if (o instanceof Element) {
					fixTag((Element) o, "data", linkToBase);
				}
			}
			
		}
		
		//	PARAMs
		o = null;
		List params = getNodesByXpath(body, ThemesConstants.PARAM_TAG_INSTRUCTION);
		if (params != null) {
			for (int i = 0; i < params.size(); i++) {
				o = params.get(i);
				if (o instanceof Element) {
					fixTag((Element) o, "value", linkToBase);
				}
			}
		}
		
		List<Text> needlessText = new ArrayList<Text>();
		List content = body.getContent();
		if (content == null) {
			return true;
		}
		
		o = null;
		e = null;
		for (int i = 0; i < content.size(); i++) {
			o = content.get(i);
			if (o instanceof Text) {	// Finding Text elements - they are needless
				needlessText.add((Text) o);
			}
			else {
				if (o instanceof Element) {
					e = (Element) o;
					if (ELEMENT_LINK_NAME.equals(e.getName())) {	//	Fixing <link>
						fixDocumentElement(e, linkToBase);
					}
					if (ELEMENT_SCRIPT_NAME.equals(e.getName())) {	//	<script> tags needs advanced handling
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
	
	@SuppressWarnings("unchecked")
	private List getNodesByXpath(Element container, String expression) {
		if (container == null || expression == null) {
			return null;
		}
		
		JDOMXPath xp = null;
		try {
			xp = new JDOMXPath(expression);
			xp.addNamespace(ThemesConstants.NAMESPACE_ID, ThemesConstants.NAMESPACE);
			return xp.selectNodes(container);
		} catch (JaxenException e) {
			e.printStackTrace();
		}
		
		return null;
	}
	
	private boolean fixTag(Element e, String attributeName, String linkToBase) {
		if (e == null || attributeName == null || linkToBase == null) {
			return false;
		}
		
		Attribute a = e.getAttribute(attributeName, namespace);
		if (a == null) {
			a = e.getAttribute(attributeName);
		}
		if (a == null) {
			return false;
		}
		
		String dataValue = a.getValue();
		if (dataValue == null) {
			return true;	//	Nothing to be fixed
		}
				
		dataValue = getFixedAttributeValue(dataValue, linkToBase);
		a.setValue(dataValue);
		
		return true;
	}
	
	private String getFixedAttributeValue(String value, String linkToBase) {
		if (value == null) {
			return null;
		}
		
		boolean madeChanges = false;
		
		while (value.indexOf(ThemesConstants.USELESS_PATHTO_ELEMENT) != -1) {
			value = value.replace(ThemesConstants.USELESS_PATHTO_ELEMENT, CoreConstants.EMPTY);
			
			madeChanges = true;
		}
		
		int index = value.indexOf(")");
		if (index != -1) {
			value = value.substring(0, index);
			
			madeChanges = true;
		}
		
		index = value.indexOf("?");
		if (index != -1) {
			value = value.substring(0, index);
			
			madeChanges = true;
		}
		
		if (madeChanges) {
			if (!value.startsWith(linkToBase)) {
				value = new StringBuffer(linkToBase).append(value).toString();
			}
		}
		
		return value;
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
		StringBuffer prop = new StringBuffer(ThemesConstants.THEMES_PROPERTY_START).append(propertyKey).append(ThemesConstants.THEMES_PROPERTY_END);
		String propertyValue = settings.getProperty(prop.toString());
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
				content.append(CoreConstants.SPACE).append(i + 1);
			}
			content.append(CONTENT_PARAGRAPH_DATE);
			content.append(d);
			content.append(CONTENT_PARAGRAPH_LINK).append(CONTENT_PARAGRAPH_START);
			content.append(IMAGE_START).append(ThemesConstants.SINGLE_QUOTE);
			content.append(ThemesConstants.BASE_THEME_IMAGES);
			content.append(ThemesConstants.THEME_IMAGES.get(helper.getRandomNumber(ThemesConstants.THEME_IMAGES.size())));
			content.append(ThemesConstants.SINGLE_QUOTE).append(CoreConstants.SPACE).append(IMAGE_POSITION);
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
		String regionContent = null;
		for (int i = 0; i < ThemesConstants.REGIONS.size(); i++) {
			fixedValue = fixValue(ThemesConstants.REGIONS.get(i));
			if (docContent.indexOf(ThemesConstants.REGIONS.get(i)) != -1) {
				regionContent = getRegion(fixedValue);
				if (ThemesConstants.REGIONS_NEEDED_TO_CREATE.contains(fixedValue)) {
					regionContent = getRegionDiv(fixedValue, regionContent);
				}
				docContent = docContent.replace(ThemesConstants.REGIONS.get(i), regionContent);
			}
		}
		return docContent;
	}
	
	private String getRegionDiv(String id, String content) {
		return new StringBuffer("<div id=\"").append(id).append("\">").append(content).append("</div>").toString();
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
	 * Adds region to DIV tag if DIV tag has proper id attribute
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
				e.addContent(getExpandRegion()); // Expanding theme
			}
		}
		
		if (!hasElementChildren(e)) {
			e.addContent(getComment(IDEGA_COMMENT));
		}
		
		return true;
	}
	
	@SuppressWarnings("unchecked")
	private boolean hasElementChildren(Element e) {
		if (e == null) {
			return false;
		}
		List<Element> children = e.getChildren();
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
		StringBuffer prop = new StringBuffer(ThemesConstants.THEMES_PROPERTY_START).append(propertyKey).append(ThemesConstants.THEMES_PROPERTY_END);
		String propertyValue = settings.getProperty(prop.toString());
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
		
		if (finishThemeChange(theme, doc, true)) {
			return themeID;
		}
		return null;
	}
	
	private boolean finishThemeChange(Theme theme, Document doc, boolean generateOnlyBigPreview) {
		String draft = new StringBuffer(helper.getFileName(theme.getLinkToSkeleton())).append(ThemesConstants.DRAFT).toString();
		if (!uploadDocument(doc, theme.getLinkToBaseAsItIs(), helper.decode(draft, true), theme, true)) {
			return false;
		}
		
		theme.setLinkToDraft(new StringBuffer(theme.getLinkToBase()).append(draft).toString());

		if (!helper.generatePreviewsForTheme(theme, true, ThemesConstants.IS_THEME_PREVIEW_JPG, ThemesConstants.THEME_PREVIEW_QUALITY, generateOnlyBigPreview)) {
			theme.setLinkToDraft(null);
			return false;
		}
		
		clearThemeVariationsFromCache(theme.getId());
		
		return true;
	}
	
	private String changeTheme(Document doc, Theme theme, String styleGroupName, String variation, String themeName, boolean radio, boolean checked) {
		if (doc == null || theme == null || styleGroupName == null || variation == null) {
			return null;
		}
		
		theme.setChangedName(themeName);
		
		boolean limitedSelection = true;
		
		ThemeStyleGroupMember oldStyle = null;
		ThemeStyleGroupMember newStyle = null;
		ThemeStyleGroupMember styleChanger = null;
		if (radio) {
			//	Simply changing CSS files
			oldStyle = getEnabledStyleMember(theme, styleGroupName);
			newStyle = getStyleMember(theme, styleGroupName, variation);
			styleChanger = oldStyle;
		}
		else {
			//	Need to know either add CSS or remove
			limitedSelection = false;
			if (checked) {
				//	Need to add
				newStyle = getStyleMember(theme, styleGroupName, variation);
				styleChanger = newStyle;
			}
			else {
				//	Need to remove
				oldStyle = getStyleMember(theme, styleGroupName, variation);
				styleChanger = oldStyle;
			}
		}
		
		Element root = doc.getRootElement();
		if (root == null) {
			return null;
		}
		String linkToBase = new StringBuffer(CoreConstants.WEBDAV_SERVLET_URI).append(theme.getLinkToBase()).toString();
		if (!changeThemeStyle(linkToBase, root.getChild(HTML_HEAD, namespace), oldStyle, newStyle)) {
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
	@SuppressWarnings("unchecked")
	private boolean changeThemeStyle(String linkToBase, Element head, ThemeStyleGroupMember oldStyle, ThemeStyleGroupMember newStyle) {
		if (head == null) {
			return false;
		}
		
		List<Element> styles = head.getChildren(ThemesConstants.ELEMENT_LINK, namespace);
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
			for (int i = 0; i < styles.size(); i++) {
				style = styles.get(i);
				attributeValue = style.getAttributeValue(ThemesConstants.TAG_ATTRIBUTE_HREF);
				files = oldStyle.getStyleFiles();
				if (files != null) {
					for (int j = 0; j < files.size(); j++) {
						if (attributeValue.endsWith(files.get(j))) {
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
		
		for (Iterator<Element> it = uselessStyles.iterator(); it.hasNext(); ) {
			it.next().detach();
		}
		
		return true;
	}
	
	/**
	 * Creates new style element: <link ... />
	 * @param newStyle
	 * @return Collection
	 */
	private Collection<Element> getNewStyleElement(String linkToBase, ThemeStyleGroupMember newStyle) {
		Collection<Element> newStyleElements = new ArrayList <Element> ();
		if (linkToBase == null || newStyle == null) {
			return newStyleElements;
		}
		List<Attribute> attributes = null;
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
	public List<ThemeStyleGroupMember> getEnabledStyles(Theme theme) {
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
	
	public ThemeStyleGroupMember getMember(Map <String, ThemeStyleGroupMember> styleMembers, String styleGroupName, int index) {
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
	public ThemeStyleGroupMember getStyleMember(Theme theme, String styleGroupName, String styleVariation) {
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
		
		if (isNewName(themeName, theme)) {
			log.info("Creating new theme: " + themeName);
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
			e.printStackTrace();
			return false;
		} finally {
			helper.closeInputStream(is);
		}
		theme.setLocked(false);
		
		theme.setLinkToDraft(null);
		theme.setLinkToThemePreview(theme.getLinkToDraftPreview());
		theme.setLinkToDraftPreview(null);
		
		helper.createSmallImage(theme, false);
		
		try {
			helper.getThemesService().createIBPage(theme);
		} catch (RemoteException e) {
			e.printStackTrace();
			return false;
		}
		
		boolean createdConfig = helper.createThemeConfig(theme);
		if (!createdConfig) {
			return false;
		}
		
		try {
			helper.getThemesService().getBuilderService().clearAllCaches();
		} catch(Exception e) {
			e.printStackTrace();
			return false;
		}
		
		return true;
	}
	
	public boolean setSelectedStyles(Theme theme, List<ThemeChange> enabledStyles) {
		if (theme == null) {
			return false;
		}
		
		ThemeChange change = null;
		ThemeStyleGroupMember member = null;
		for (int i = 0; i < enabledStyles.size(); i++) {
			change = enabledStyles.get(i);
			member = getStyleMember(theme, change.getStyleGroupName(), change.getVariation());
			if (member != null) {
				member.setEnabled(change.isEnabled());
			}
		}
		
		return true;
	}
	
	private boolean restoreTheme(Theme theme) {
		if (theme == null) {
			return false;
		}
		
		clearThemeVariationsFromCache(theme.getId());
		
		List <ThemeChange> changes = theme.getChanges();
		ThemeChange change = null;
		ThemeStyleGroupMember member = null;
		for (int i = 0; i < changes.size(); i++) {
			change = changes.get(i);
			member = getStyleMember(theme, change.getStyleGroupName(), change.getVariation());
			if (member != null) {
				if (change.isLimitedSelection()) {
					setEnabledStyles(theme, member.getGroupName(), false);
				}
				member.setEnabled(!change.isEnabled());
			}
		}
		
		if (!theme.getLinkToSkeleton().endsWith(ThemesConstants.THEME)) {
			//	Original theme, will set variations from properties list
			if (reloadThemeProperties(theme.getId(), false)) {
				if (!setDefaultStylesToTheme(theme)) {
					return false;
				}
			}
			else {
				return false;
			}
		}
		
		if (helper.generatePreviewsForTheme(theme, false, ThemesConstants.IS_THEME_PREVIEW_JPG, ThemesConstants.THEME_PREVIEW_QUALITY, false)) {
			theme.setChangedName(null);
			theme.setLinkToDraftPreview(null);
			theme.setLinkToDraft(null);
			theme.setChanges(new ArrayList<ThemeChange>());
		
			helper.createThemeConfig(theme);
			
			return true;
		}
		
		return false;
	}
	
	@SuppressWarnings("unchecked")
	private boolean setDefaultStylesToTheme(Theme theme) {
		if (theme == null) {
			return false;
		}
		
		List<ThemeStyleGroupMember> styles = getEnabledStyles(theme);
		if (styles.size() == 0) {
			return true;
		}
		
		Document doc = getThemeDocument(theme.getId());
		if (doc == null) {
			return false;
		}
		
		Element root = doc.getRootElement();
		if (root == null) {
			return false;
		}
		
		Element header = root.getChild(HTML_HEAD, namespace);
		if (header == null) {
			return false;
		}
		
		List<Element> children = header.getChildren();
		List<ThemeStyleGroupMember> styleMembers = new ArrayList<ThemeStyleGroupMember>();
		List<Element> stylesToRemove = new ArrayList<Element>();
		Map<String, Integer> stylesIndexes = new HashMap<String, Integer>();
		if (children != null) {
			Element child = null;
			for (int j = 0; j < children.size(); j++) {
				child = children.get(j);	
				if (ELEMENT_LINK_NAME.equals(child.getName())) {
					Attribute type = child.getAttribute(ThemesConstants.TAG_ATTRIBUTE_TYPE);
					if (type != null && TAG_ATTRIBUTE_VALUE_CSS.equals(type.getValue())) {
						Attribute styleLink = child.getAttribute(ThemesConstants.TAG_ATTRIBUTE_HREF);
						if (styleLink != null) {
							String styleHref = styleLink.getValue();
							if (styleHref != null && !isDefaultStyle(styleHref)) {
								ThemeStyleGroupMember member = getExistingDefaultStyle(styleHref, styles);
								if (member == null) {
									stylesToRemove.add(child);	//	Style is needless: it's not declared as default in properties list
									stylesIndexes.put(getStyleGroupNameByHref(theme, styleHref), getElementIndex(header.getContent(), ThemesConstants.TAG_ATTRIBUTE_HREF, child.getAttributeValue(ThemesConstants.TAG_ATTRIBUTE_HREF)));
								}
								else {
									styleMembers.add(member);
								}
							}
						}
					}
				
				}
			}
		}
		
		ThemeStyleGroupMember member = null;
		for (int i = 0; i < styleMembers.size(); i++) {
			member = styleMembers.get(i);
			
			if (styles.contains(member)) {
				styles.remove(member);	//	Default style already in theme, removing it from list
			}
		}
		
		String linkToBase = new StringBuffer(CoreConstants.WEBDAV_SERVLET_URI).append(theme.getLinkToBase()).toString();
		for (int i = 0; i < styles.size(); i++) {
			member = styles.get(i);
			if (!addContentToElement(header, getNewStyleElement(linkToBase, member), stylesIndexes.get(member.getGroupName()))) {
				return false;
			}
		}
		
		for (Iterator<Element> it = stylesToRemove.iterator(); it.hasNext(); ) {
			it.next().detach();
		}
		
		return uploadTheme(doc, theme);
	}
	
	private String getStyleGroupNameByHref(Theme theme, String href) {
		if (theme == null || href == null) {
			return null;
		}
		
		Collection<ThemeStyleGroupMember> members = theme.getStyleGroupsMembers().values();
		if (members == null) {
			return null;
		}
		
		ThemeStyleGroupMember member = getExistingDefaultStyle(href, new ArrayList<ThemeStyleGroupMember>(members));
		if (member == null) {
			return null;
		}
		
		return member.getGroupName();
	}
	
	/**
	 * Checks if CSS link already is in HEADER
	 * @param cssHref
	 * @param styles
	 * @return
	 */
	private ThemeStyleGroupMember getExistingDefaultStyle(String cssHref, List<ThemeStyleGroupMember> styles) {
		if (cssHref == null || styles == null) {
			return null;
		}
		
		ThemeStyleGroupMember styleMember = null;
		for (int i = 0; i < styles.size(); i++) {
			styleMember = styles.get(i);
			List<String> style = styleMember.getStyleFiles();
			for (int j = 0; j < style.size(); j++) {
				if (cssHref.endsWith(style.get(j))) {
					return styleMember;
				}
			}
		}
		
		return null;
	}
	
	private boolean isDefaultStyle(String cssHref) {
		if (cssHref == null) {
			return false;
		}
		
		for (int i = 0; i < ThemesConstants.DEFAULT_STYLE_FILES.size(); i++) {
			if (cssHref.endsWith(ThemesConstants.DEFAULT_STYLE_FILES.get(i))) {
				return true;
			}
		}
		
		return false;
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
	
	private void setEnabledStyles(Theme theme, String styleGroupName, boolean enable) {
		ThemeStyleGroupMember member = null;
		Collection<ThemeStyleGroupMember> styleMembers = theme.getStyleGroupsMembers().values();
		for (Iterator <ThemeStyleGroupMember> it = styleMembers.iterator(); it.hasNext(); ) {
			member = it.next();
			if (member.getGroupName().equals(styleGroupName)) {
				member.setEnabled(enable);
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
		
		String tempName = new StringBuilder(newName).append(ThemesConstants.THEME).toString();
		String themeName = StringHandler.removeCharacters(tempName, ContentConstants.SPACE, ContentConstants.UNDER);
		try {
			if (!helper.getSlideService().uploadFileAndCreateFoldersFromStringAsRoot(decodedLinkToBase,	themeName, is, null, true)) {
				return false;
			}
		} catch (RemoteException e) {
			e.printStackTrace();
			return false;
		} finally {
			helper.closeInputStream(is);
		}
		
		//	Adding theme to system
		String tempLink = new StringBuffer(decodedLinkToBase).append(themeName).toString();
		String themeID = null;
		try {
			themeID = helper.getThemesLoader().createNewTheme(tempLink, new StringBuffer(linkToBase).append(helper.encode(themeName, true)).toString(), true, true);
		} catch (Exception e) {
			e.printStackTrace();
		}
		if (themeID == null) {
			return false;
		}
		Theme child = helper.getTheme(themeID);
		if (child == null) {
			return false;
		}
		child.setName(newName);

		//	Copying settings from parent theme
		copyTheme(parent, child);
		
		//	Generating previews
		 if (!helper.generatePreviewsForTheme(child, false, ThemesConstants.IS_THEME_PREVIEW_JPG, ThemesConstants.THEME_PREVIEW_QUALITY, false)) {
			 return false;
		 }
		
		child.setPropertiesExtracted(true);
		
		// Creating new template
		try {
			helper.getThemesService().createIBPage(child);
		} catch (RemoteException e) {
			e.printStackTrace();
			return false;
		}
		
		helper.createThemeConfig(child);
		
		restoreTheme(parent);
		
		return true;
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
	
	public XMLOutputter getXMLOutputter() {
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
		
		if (linkToDoc.indexOf(CoreConstants.SPACE) != -1) {
			linkToDoc = helper.urlEncode(linkToDoc);
		}
		return helper.getXMLDocument(new StringBuffer(helper.getFullWebRoot()).append(linkToDoc).toString());
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
		
		if (finishThemeChange(theme, doc, true)) {
			return themeID;
		}
		
		return null;
	}
	
	private boolean isNewName(String name, Theme theme) {
		if (theme == null) {
			return false;
		}
		
		String changedName = theme.getChangedName();
		if (changedName == null && name == null) {
			return false;	//	No new name for theme
		}
		
		if (changedName == null) {
			changedName = name;	//	Assigning current name
		}
		else {
			changedName = name;
		}
				
		return !theme.getName().equals(changedName);
	}
	
	public boolean reloadThemeProperties(String themeId, boolean checkConfig) {
		if (themeId == null) {
			return false;
		}
		Theme theme = helper.getTheme(themeId);
		if (theme == null) {
			return false;
		}

		List<ThemeChange> enabledStyles = getEnabledStylesAsThemeChanges(theme);				//	Getting current state
		helper.clearVariationFromCache(themeId);												//	Clearing cache
		theme.reloadProperties();																//	Clearing properties
		try {
			helper.getThemesPropertiesExtractor().prepareTheme(checkConfig, theme, null, null);	//	Extracting new properties (also setting default state)
		} catch (Exception e) {
			e.printStackTrace();
			return false;
		}
		setSelectedStyles(theme, enabledStyles);												//	Restoring current state
		
		return true;
	}
	
	private List<ThemeChange> getEnabledStylesAsThemeChanges(Theme theme) {
		List<ThemeChange> enabled = new ArrayList<ThemeChange>();
		
		if (theme == null) {
			return enabled;
		}
		
		ThemeChange change = null;
		ThemeStyleGroupMember member = null;
		for (Iterator<ThemeStyleGroupMember> it = theme.getStyleGroupsMembers().values().iterator(); it.hasNext(); ) {
			member = it.next();
			change = new ThemeChange();
			change.setStyleGroupName(member.getGroupName());
			change.setVariation(member.getName());
			change.setEnabled(member.isEnabled());
			change.setLimitedSelection(member.isLimitedSelection());
			enabled.add(change);
		}
		
		return enabled;
	}

}