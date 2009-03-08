package com.idega.content.themes.helpers.business;

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.rmi.RemoteException;
import java.text.DateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

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
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Scope;
import org.springframework.stereotype.Service;

import com.idega.builder.bean.AdvancedProperty;
import com.idega.content.business.ContentConstants;
import com.idega.content.business.ContentUtil;
import com.idega.content.themes.helpers.bean.BuiltInThemeStyle;
import com.idega.content.themes.helpers.bean.Theme;
import com.idega.content.themes.helpers.bean.ThemeChange;
import com.idega.content.themes.helpers.bean.ThemeStyleGroupMember;
import com.idega.idegaweb.IWMainApplication;
import com.idega.idegaweb.IWMainApplicationSettings;
import com.idega.presentation.IWContext;
import com.idega.slide.business.IWSlideService;
import com.idega.util.CoreConstants;
import com.idega.util.CoreUtil;
import com.idega.util.IOUtil;
import com.idega.util.IWTimestamp;
import com.idega.util.ListUtil;
import com.idega.util.StringHandler;
import com.idega.util.StringUtil;
import com.idega.util.expression.ELUtil;
import com.idega.util.resources.ResourceScanner;

@Scope("singleton")
@Service(ThemeChanger.SPRING_BEAN_IDENTIFIER)
public class ThemeChangerBean implements ThemeChanger {

	private static final Logger logger = Logger.getLogger(ThemeChangerBean.class.getName());
	
	// These are defaults in RapidWeaver, we are using its to generate good preview
	private static final String IMAGE_START = "<img class=\"imageStyle\" src=";
	private static final String IMAGE_END = " />";
	private static final String CONTENT_PARAGRAPH_TITLE = "<div class=\"blog-entry\"><h1 class=\"blog-entry-title\">";
	private static final String CONTENT_PARAGRAPH_DATE = "</h1><div class=\"blog-entry-date\">";
	private static final String CONTENT_PARAGRAPH_LINK = "</div><div class=\"blog-entry-body\">";
	private static final String CONTENT_PARAGRAPH_END = "</div>";
	
	private static final String HREF = "href";
	
	// Default keywords
	private static final String FOOTER = "footer";
	private static final String CONTENT = "content";
	
	private static final String HTML_HEAD = "head";
	private static final String HTML_BODY = "body";
	
	// Element's attributes and values
	private static final String TAG_ATTRIBUTE_REL = "rel";
	private static final String TAG_ATTRIBUTE_MEDIA = "media";
	private static final String TAG_ATTRIBUTE_VALUE_STYLESHEET = "stylesheet";
	private static final String TAG_ATTRIBUTE_VALUE_CSS = CoreConstants.CONTENT_TYPE_TEXT_CSS;
	private static final String TAG_ATTRIBUTE_VALUE_SCREEN = "screen";
	
	//	HTML codes
	private static final String NO_BREAK_STRING = "&nbsp;";
	private static final String COPY_AND_SPACE = "&copy;" + NO_BREAK_STRING;
	private static final String LESS_CODE = "&lt;";
	private static final String LESS_CODE_REPLACEMENT = "\n<";
	private static final String MORE_CODE = "&gt;";
	private static final String MORE_CODE_REPLACEMENT = ">\n";
	
	private static final String ELEMENT_SCRIPT_NAME = "script";
	private static final String ELEMENT_LINK_NAME = "link";
	
	private static final int THEME_HEIGHT = 400;
	
	private static final String REGION_TO_EXPAND = "contentContainer";
	
	private static final String IDEGA_COMMENT = "idega";
	
	private static final String[] TOOLBAR_NAV_MENU = new String[] {"Frontpage", "Products", "Customers", "The Company"};
	private static final String[] TOOLBAR_CHILDREN_NAV_MENU = new String[] {"News", "Services"};
	private static final String[] DUMMY_CATEGORIES = new String[] {"Personal", "Work", "Humor", "Idega"};
	
	private static final String[] _VALID_LINK_TAG_ATTRIBUTES = new String[] {"charset", HREF, "hreflang", "media", "rel", "rev", "target", "type", "title"};
	private static final List<String> VALID_LINK_TAG_ATTRIBUTES = Collections.unmodifiableList(Arrays.asList(_VALID_LINK_TAG_ATTRIBUTES));
	
	private static final String[] _UN_CHANGED_CASE_ATTRIBUTES = new String[] {HREF, "src"};
	private static final List<String> UN_CHANGED_CASE_ATTRIBUTES = Collections.unmodifiableList(Arrays.asList(_UN_CHANGED_CASE_ATTRIBUTES));
	
	private static final String[] _THEME_HEAD_VARIABLES = new String[] {"%header%", "%style_variations%", "%user_styles%", "%user_javascript%",
														"%plugin_header%", "%user_header%"};
	private static final List<String> THEME_HEAD_VARIABLES = Collections.unmodifiableList(Arrays.asList(_THEME_HEAD_VARIABLES));
	
	private static final String[] _REGULAR_EXORESSIONS_FOR_NEEDLESS_STUFF = new String[] {/*" xmlns.+\"", */"..<?doc.+?>"};
	private static final List<String> REGULAR_EXORESSIONS_FOR_NEEDLESS_STUFF = Collections.unmodifiableList(Arrays.asList(_REGULAR_EXORESSIONS_FOR_NEEDLESS_STUFF));
	
	private ThemesHelper helper = null;
	private Namespace namespace = Namespace.getNamespace(ThemesConstants.NAMESPACE);
	private XMLOutputter out = null;
	private ColourExpressionCalculator colourCalculator = null;
	
	public ThemeChangerBean() {
		out = new XMLOutputter();
		out.setFormat(Format.getPrettyFormat());
		
		helper = ThemesHelper.getInstance();
	}
	
	public ColourExpressionCalculator getColourCalculator() {
		return colourCalculator;
	}

	@Autowired
	public void setColourCalculator(ColourExpressionCalculator colourCalculator) {
		this.colourCalculator = colourCalculator;
	}

	private boolean prepareHeadContent(Theme theme, String skeleton) {
		if (theme == null || skeleton == null) {
			return false;
		}
		
		String content = null;
		try {
			content = StringHandler.getContentFromInputStream(helper.getInputStream(new StringBuffer(helper.getFullWebRoot()).append(skeleton).toString()));
		} catch (Exception e) {
			e.printStackTrace();
			return false;
		}
		if (StringUtil.isEmpty(content)) {
			return false;
		}
		
		String variable = null;
		String value = null;
		for (int i = 0; i < THEME_HEAD_VARIABLES.size(); i++) {
			variable = THEME_HEAD_VARIABLES.get(i);
			value = getThemeValueByVariable(variable);

			content = StringHandler.replace(content, variable, value);
		}
		
		return uploadTheme(content, theme, false);
	}
	
	private String getThemeValueByVariable(String variable) {
		return CoreConstants.EMPTY;	//	TODO: make logic to get values for theme's variables
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
		
		if (!prepareHeadContent(theme, skeleton)) {
			return false;
		}
		
		Document doc = helper.getXMLDocument(new StringBuffer(helper.getFullWebRoot()).append(skeleton).toString(), true, false);
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
		
		// Adding Builder page regions
		if (!proceedBodyContent(path, root.getChild(HTML_BODY, namespace))) {
			return false;
		}
		
		// Adding enabled styles
		if (!addDefaultEnabledStyles(theme, head, path, doc, null)) {
			return false;
		}
		
		return uploadTheme(doc, theme);
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
			e.addContent(getCommentsCollection(fixValue(e.getTextNormalize(), linkToBase)));
		}

		Text t = null;
		for (Iterator<Text> itt = textElements.iterator(); itt.hasNext(); ) {
			t = itt.next();
			if (needAddRegion(ThemesConstants.REGIONS, t.getTextNormalize())) {
				head.addContent(getCommentsCollection(fixValue(t.getTextNormalize(), linkToBase)));
			}
			t.detach();
		}
	
		//	Adding fake (comment) element to <script> - to get <script ...></script> in HTML code
		//	Checking <link> tags in head
		List elements = head.getContent();
		o = null;
		Element element = null;
		if (elements != null) {
			for (int i = 0; i < elements.size(); i++) {
				o = elements.get(i);
				if (o instanceof Element) {
					element = (Element) o;
					if (ELEMENT_SCRIPT_NAME.equals(element.getName().toLowerCase())) {
						//	<script>
						element.addContent(getComment(IDEGA_COMMENT));
					}
					
					if (ELEMENT_LINK_NAME.equals(element.getName().toLowerCase())) {
						//	<link>
						checkElementAttributes(element, VALID_LINK_TAG_ATTRIBUTES);
					}
				}
			}

		}
		
		return true;
	}
	
	private boolean uploadTheme(String content, Theme theme, boolean addRegions) {
		return uploadDocument(content, theme.getLinkToBaseAsItIs(), helper.getFileNameWithExtension(theme.getLinkToSkeleton()), theme,	true, addRegions);
	}
	
	private boolean uploadTheme(Document doc, Theme theme) {
		checkCssFiles(doc, theme.getLinkToBase());
		checkScriptTags(doc);
		
		return uploadTheme(out.outputString(doc), theme, true);
	}
	
	@SuppressWarnings("unchecked")
	private boolean checkCssFiles(Document doc, String linkToTheme) {
		if (doc == null) {
			return false;
		}
		
		Element root = doc.getRootElement();
		if (root == null) {
			return false;
		}
			
		List links = getNodesByXpath(root, ThemesConstants.LINK_TAG_INSTRUCTION);
		if (links == null) {
			return true;
		}
		
		Object o = null;
		Element link = null;
		String cssLink = null;
		for (int i = 0; i < links.size(); i++) {
			o = links.get(i);
			if (o instanceof Element) {
				link = (Element) o;
				
				cssLink = link.getAttributeValue(ThemesConstants.TAG_ATTRIBUTE_HREF);
				
				if (cssLink != null) {
					proceedStyleFile(linkToTheme, getFixedDocumentContent(cssLink), false);
				}
			}
		}
		
		return true;
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
		
		if (!createCopiesForColourFiles(theme, null)) {
			return false;
		}
		
		if (!setValuesToColourFiles(theme)) {
			return false;
		}
		
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
		
		List<String> invalidFiles = new ArrayList<String>();
		int i = 0;
		for (Iterator<ThemeStyleGroupMember> it = styles.iterator(); it.hasNext(); ) {
			member = it.next();
			files = member.getStyleFiles();
			for (index = 0; index < files.size(); index++) {
				if (!proceedStyleFile(theme.getLinkToBase(), new StringBuffer(theme.getLinkToBase()).append(files.get(index)).toString(), false)) {
					invalidFiles.add(files.get(index));	//	Invalid CSS file, disabling variation
				}
			}
			i++;
		}
		removeVariationsFromTheme(theme, invalidFiles);
		
		return true;
	}
	
	private boolean setValuesToColourFiles(Theme theme) {
		return getColourCalculator().setValuesToColourFiles(theme);
	}
	
	private boolean createCopiesForColourFiles(Theme theme, String oldName) {
		return createCopiesForColourFiles(theme, null, oldName, true);
	}
	
	private boolean createCopiesForColourFiles(Theme theme, List<String> files, String oldName, boolean setAsOriginalFiles) {
		if (theme == null) {
			return false;
		}
		
		if (files == null) {
			files = theme.getColourFiles();
		}
		if (files == null || files.size() == 0) {
			return true;	//	No color files - it's ok
		}
		
		IWSlideService slide = helper.getSlideService();
		if (slide == null) {
			return false;
		}
		
		List<String> newColourFiles = new ArrayList<String>();
		InputStream stream = null;
		StringBuffer link = null;
		String uploadName = null;
		String file = null;
		String webRoot = helper.getFullWebRoot();
		for (int i = 0; i < files.size(); i++) {
			file = files.get(i);
			
			link = new StringBuffer(webRoot).append(theme.getLinkToBase());
			if (oldName == null) {
				link.append(file);
			}
			else {
				link.append(helper.getThemeColourFileName(theme, oldName, file, setAsOriginalFiles));
			}
			stream = helper.getInputStream(link.toString());
			if (stream == null) {
				return false;
			}
			
			uploadName = helper.getThemeColourFileName(theme, null, file, setAsOriginalFiles);
			try {
				if (slide.uploadFileAndCreateFoldersFromStringAsRoot(theme.getLinkToBase(), uploadName, stream, CoreConstants.CONTENT_TYPE_TEXT_CSS, true)) {
					newColourFiles.add(uploadName);
				}
				else {
					return false;
				}
			} catch(Exception e) {
				e.printStackTrace();
				return false;
			} finally {
				IOUtil.closeInputStream(stream);
			}
		}
		
		if (newColourFiles.size() > 0) {
			if (setAsOriginalFiles) {
				theme.setOriginalColourFiles(newColourFiles);
			}
			else {
				theme.setColourFiles(newColourFiles);
			}
		}
		
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
	
	private boolean prepareThemeDefaultStyleFiles(Theme theme) {
		List <String> defaultStyles = ThemesConstants.DEFAULT_STYLE_FILES;
		for (int i = 0; i < defaultStyles.size(); i++) {
			proceedStyleFile(theme.getLinkToBase(), new StringBuffer(theme.getLinkToBase()).append(defaultStyles.get(i)).toString(), true);
		}
		return true;
	}
	
	private boolean proceedStyleFile(String linkToTheme, String linkToStyle, boolean standardFiles) {
		if (linkToStyle == null) {
			return false;
		}
		
		// Getting CSS file
		String fullLink = null;
		if (linkToStyle.startsWith(CoreConstants.WEBDAV_SERVLET_URI)) {
			linkToStyle = linkToStyle.substring(linkToStyle.indexOf(CoreConstants.WEBDAV_SERVLET_URI) + CoreConstants.WEBDAV_SERVLET_URI.length());
		}
		fullLink = new StringBuffer(helper.getFullWebRoot()).append(linkToStyle).toString();
		InputStream is = helper.getInputStream(fullLink);
		if (is == null) {
			if (!standardFiles) {
				logger.log(Level.WARNING, new StringBuilder("Can't get CSS file: '").append(linkToStyle).append("' from Theme pack!").toString());
			}
			return false;
		}
		InputStreamReader isr = new InputStreamReader(is);
		BufferedReader buf = new BufferedReader(isr);
		ResourceScanner scanner = ELUtil.getInstance().getBean(CssScanner.SPRING_BEAN_IDENTIFIER);
		scanner.setReaderBuffer(buf);
		scanner.setLinkToTheme(linkToTheme);
		scanner.scanFile();
		
		IOUtil.closeInputStream(is);
		helper.closeInputStreamReader(isr);
		helper.closeBufferedReader(buf);
		
		StringBuffer result = scanner.getResultBuffer();
		if (result == null) {
			return true;
		}
		boolean needToReplace = scanner.isNeedToReplace();
		
		String content = result.toString();
		if (!needToReplace) {
			return true;
		}

		// Uploading modified file
		try {
			if (helper.getSlideService().uploadFileAndCreateFoldersFromStringAsRoot(helper.getLinkToBase(helper.decodeUrl(linkToStyle)),
					helper.getFileNameWithExtension(linkToStyle), content, CoreConstants.CONTENT_TYPE_TEXT_CSS, true)) {
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
	
	private boolean uploadDocument(String content, String linkToBase, String fileName, Theme theme, boolean isTheme, boolean addRegions) {
		if (isTheme) {
			if (addRegions) {
				content = addRegions(theme, content, linkToBase);
				if (content == null) {
					return false;
				}
			}
			content = StringHandler.replace(content, ThemesConstants.USELESS_PATHTO_ELEMENT,
					new StringBuffer(CoreConstants.WEBDAV_SERVLET_URI).append(linkToBase).toString());
			
			if (addRegions) {
				content = getFixedDocumentContent(content);
				content = removeNeedlessStuffWithRegularExpressions(content);
			}
			
			content = StringHandler.replace(content, LESS_CODE, LESS_CODE_REPLACEMENT);
			content = StringHandler.replace(content, MORE_CODE, MORE_CODE_REPLACEMENT);
			content = StringHandler.replace(content, "xml:space=\"preserve\"", CoreConstants.EMPTY);
		}

		theme.setLocked(true);
		try {
			if (!helper.getSlideService().uploadFileAndCreateFoldersFromStringAsRoot(linkToBase, fileName, content, null, true)) {
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
	
	private String removeNeedlessStuffWithRegularExpressions(String content) {
		if (content == null) {
			return null;
		}
		
		String expression = null;
		Pattern p = null;
		Matcher m = null;
		List<String> stuffToRemove = new ArrayList<String>();
		for (int i = 0; i < REGULAR_EXORESSIONS_FOR_NEEDLESS_STUFF.size(); i++) {
			expression = REGULAR_EXORESSIONS_FOR_NEEDLESS_STUFF.get(i);
			
			p = Pattern.compile(expression);
			m = p.matcher(content);
			while (m.find()) {
				int start = m.start();
				int end = m.end();
				
				stuffToRemove.add(content.substring(start, end));
			}
		}
		
		for (int i = 0; i < stuffToRemove.size(); i++) {
			content = StringHandler.replace(content, stuffToRemove.get(i), CoreConstants.EMPTY);
		}
		
		return content;
	}
	
	/**
	 * Uploads document to slide
	 * @param doc
	 * @param linkToBase
	 * @param fileName
	 * @return boolean
	 */
	public boolean uploadDocument(Document doc, String linkToBase, String fileName, Theme theme, boolean isTheme) {
		checkScriptTags(doc);
		
		return uploadDocument(out.outputString(doc), linkToBase, fileName, theme, isTheme, false);
	}
	
	@SuppressWarnings("unchecked")
	private void checkScriptTags(Document doc) {
		List<Element> scripts = getNodesByXpath(doc.getRootElement().getChild("body", namespace), "//" + ThemesConstants.NAMESPACE_ID + ":script");
		if (ListUtil.isEmpty(scripts)) {
			return;
		}
		
		for (Element script: scripts) {
			if (ListUtil.isEmpty(script.getContent())) {
				script.addContent(getComment("IdegaWeb"));
			}
		}
	}
	
	/**
	 * Checks if document contains needless info if so, replaces with empty String
	 * @param documentContent
	 * @return String
	 */
	private String getFixedDocumentContent(String content) {
		String uselessContent = null;
		for (int i = 0; i < ThemesConstants.USELESS_CONTENT.size(); i++) {
			uselessContent = ThemesConstants.USELESS_CONTENT.get(i);
			
			if (content.indexOf(uselessContent) != -1) {
				content = StringHandler.replace(content, uselessContent, ThemesConstants.EMPTY);
			}
		}
		return content;
	}
	
	@SuppressWarnings("unchecked")
	private void checkElementAttributes(Element e, List<String> validAttributes) {
		if (e == null || validAttributes == null) {
			return;
		}
		
		List attributes = e.getAttributes();
		if (attributes == null) {
			return;
		}
		
		List<Attribute> needless = new ArrayList<Attribute>();
		
		Object o = null;
		Attribute a = null;
		String name = null;
		String value = null;
		for (int i = 0; i < attributes.size(); i++) {
			o = attributes.get(i);
			if (o instanceof Attribute) {
				a = (Attribute) o;
				
				name = a.getName();
				value = a.getValue();
				if (name == null || value == null) {
					needless.add(a);
				}
				else {
					name = name.toLowerCase();
					if (!UN_CHANGED_CASE_ATTRIBUTES.contains(name)) {
						value = value.toLowerCase();
					}
					if (!validAttributes.contains(name)) {
						needless.add(a);
					}
					else {
						a.setName(name);
						a.setValue(value);
					}
				}
			}
		}
		
		for (Iterator<Attribute> it = needless.iterator(); it.hasNext();) {
			it.next().detach();
		}
	}
	
	private void fixDocumentElement(Element e, String linkToBase) {
		if (e == null || linkToBase == null) {
			return;
		}
		Attribute a = null;
		if (!needAddRegion(ThemesConstants.REGIONS, e.getTextNormalize())) {
			e.setText(fixValue(e.getTextNormalize(), linkToBase));
		}
		a = e.getAttribute(ThemesConstants.TAG_ATTRIBUTE_HREF);
		if (a == null) {
			a = e.getAttribute(ThemesConstants.TAG_ATTRIBUTE_SRC);
		}
		if (a != null) {
			String fixedValue = fixValue(a.getValue(), linkToBase);
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
	private Collection<Content> getCommentsCollection(String commentValue) {
		Collection<Content> c = new ArrayList<Content>();
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
	private boolean needAddRegion(List<String> regions, String value) {
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
						if (!hasElementChildren(e, true)) {
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
		
		if (value.indexOf(ThemesConstants.USELESS_PATHTO_ELEMENT) != -1) {
			value = StringHandler.replace(value, ThemesConstants.USELESS_PATHTO_ELEMENT, CoreConstants.EMPTY);
			
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
	
	private String getBreadCrumbContent() {
		return new StringBuffer("<a href=\"javascript:void(0)\">").append(TOOLBAR_NAV_MENU[0]).append("</a>").append(NO_BREAK_STRING).append(NO_BREAK_STRING)
				.toString();
	}
	
	private String getToolBarContent() {
		StringBuffer navMenu = new StringBuffer("<ul>");
		for (int i = 0; i < TOOLBAR_NAV_MENU.length; i++) {
			navMenu.append("<li>");
			navMenu.append("<a");
			if (i == 0) {
				navMenu.append(" class=\"current\" id=\"current\"");
			}
			navMenu.append(" href=\"javascript:void(0)\" rel=\"self\">").append(TOOLBAR_NAV_MENU[i]).append("</a>");
			if (i == 0) {
				navMenu.append("<ul>");
				for (int j = 0; j < TOOLBAR_CHILDREN_NAV_MENU.length; j++) {
					navMenu.append("<li>")
								.append("<a");
					if (j == 0) {
						navMenu.append(" class=\"current\" id=\"current\"");
					}
						navMenu.append(" href=\"javascript:void(0)\" rel=\"self\" >").append(TOOLBAR_CHILDREN_NAV_MENU[j]).append("</a>")
						.append("</li>");
				}
				navMenu.append("</ul>");
			}
			
			navMenu.append("</li>");
		}
		navMenu.append("</ul>");
		return navMenu.toString();
	}
	
	private String getContentReplace(String defaultValue) {
		StringBuffer content = new StringBuffer();
		if (!StringUtil.isEmpty(defaultValue)) {
			content.append(defaultValue).append(ThemesConstants.NEW_LINE);
		}
		Locale l = null;
		IWContext iwc = CoreUtil.getIWContext();
		if (iwc != null) {
			l = iwc.getCurrentLocale();
		}
		if (l == null) {
			l = Locale.ENGLISH;
		}
		
		content.append(ThemesConstants.NEW_LINE);
		
		content.append(getDummyArticle(l, "Breaking news!", "Lorem ipsum dolor sit amet, consectetuer adipiscing elit. Vestibulum bibendum, ligula ut feugiat rutrum, mauris libero ultricies nulla, at hendrerit lectus dui bibendum metus. Phasellus quis nulla nec mauris sollicitudin ornare. Vivamus faucibus. Class aptent taciti sociosqu ad litora torquent per conubia nostra, per inceptos hymenaeos. Cras vulputate condimentum ipsum. Duis urna eros, commodo id, sagittis sed, sodales eu, ante.", true));
		content.append(getDummyArticle(l, "A very successful start", "Etiam ante. Cras risus dolor, porta nec, adipiscing eu, scelerisque at, metus. Mauris nunc eros, porttitor nec, tincidunt ut, rutrum eget, massa. In facilisis nisi. Sed non lorem malesuada quam egestas bibendum. Quisque bibendum ullamcorper purus.", false));
		
		content.append(ThemesConstants.NEW_LINE);
		return content.toString();
	}
	
	private String getDummyArticle(Locale locale, String title, String body, boolean addImage) {
		IWTimestamp timestamp = new IWTimestamp(new Date());
		
		StringBuilder content = new StringBuilder(CONTENT_PARAGRAPH_TITLE).append(title).append(CONTENT_PARAGRAPH_DATE);
		content.append(timestamp.getLocaleDate(locale, DateFormat.MEDIUM)).append(CONTENT_PARAGRAPH_LINK);
		
		if (addImage) {
			content.append("<div class=\"image-right\">").append(IMAGE_START).append(ThemesConstants.SINGLE_QUOTE);
			content.append(ThemesConstants.BASE_THEME_IMAGES);
			content.append(ThemesConstants.THEME_IMAGES.get(helper.getRandomNumber(ThemesConstants.THEME_IMAGES.size())));
			content.append(ThemesConstants.SINGLE_QUOTE).append(CoreConstants.SPACE).append(IMAGE_END).append("</div>");
		}
		
		content.append(body);
		
		content.append("<div class=\"content_item_comments_style\">")
					.append("<div>")
						.append("<a href=\"javascript:void(0)\">Comments(").append(helper.getRandomNumber(20)).append(")</a>")
						.append("&nbsp;")
						.append("<a href=\"javascript:void(0)\">")
							.append("<img title=\"Atom feed\" src=\"").append(ContentUtil.getBundle().getVirtualPathWithFileNameString("images/feed.png"))
								.append("\" name=\"Atom feed\" alt=\"\"/>")
						.append("</a>")
					.append("</div>")
					.append("<div>")
						.append("<a href=\"javascript:void(0)\">Add your comment</a>")
					.append("</div>")
			.append("</div>");
		
		content.append(CONTENT_PARAGRAPH_END).append(CONTENT_PARAGRAPH_END);
		
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
		
		StringBuffer key = new StringBuffer(ThemesConstants.THEMES_PROPERTY_START).append(value);
		key.append(ThemesConstants.THEMES_PROPERTY_END);
		
		if (value.equals(ThemesConstants.TOOLBAR) || value.startsWith(ThemesConstants.TOOLBAR)) {
			//	Outputs the menu links for the site.
			region.append(getToolBarContent());
			
			region.append(ThemesConstants.COMMENT_BEGIN).append(ThemesConstants.TEMPLATE_REGION_END);
			region.append(ThemesConstants.COMMENT_END);
			return region.toString();
		}
		if (value.equals(ThemesConstants.BREADCRUMB)) {
			//	Outputs a list of links from the home page to the current page.
			region.append(getBreadCrumbContent());
			
			region.append(ThemesConstants.COMMENT_BEGIN).append(ThemesConstants.TEMPLATE_REGION_END);
			region.append(ThemesConstants.COMMENT_END);
			return region.toString();
		}
		
		String defaultValue = CoreConstants.EMPTY;
		if (value.equals(ThemesConstants.LOGO)) {
			defaultValue = ContentUtil.getBundle().getVirtualPathWithFileNameString("images/idegalogo.png");
		}
		String propertyValue = getApplicationSettings().getProperty(key.toString(), defaultValue);
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
		
		if (value.equals(ThemesConstants.LOGO) && !(propertyValue.equals(CoreConstants.EMPTY))) {
			region.append("<img src=\"").append(propertyValue).append("\"></img>");
			
			region.append(ThemesConstants.COMMENT_BEGIN).append(ThemesConstants.TEMPLATE_REGION_END);
			region.append(ThemesConstants.COMMENT_END);
			return region.toString();
		}
		if (value.equals("sidebar")) {
			region.append(getDummyCategories());
			
			region.append(ThemesConstants.COMMENT_BEGIN).append(ThemesConstants.TEMPLATE_REGION_END);
			region.append(ThemesConstants.COMMENT_END);
			return region.toString();
		}
		
		region.append(propertyValue);
		
		region.append(ThemesConstants.COMMENT_BEGIN).append(ThemesConstants.TEMPLATE_REGION_END).append(ThemesConstants.COMMENT_END);
		return region.toString();
	}
	
	/**
	 * Creates Builder's regions in HTML document
	 * @param docContent
	 * @return String
	 */
	private String addRegions(Theme theme, String docContent, String linkToBase) {
		if (theme == null || docContent == null || linkToBase == null) {
			return null;
		}
		
		List<String> regions = new ArrayList<String>();
		Pattern regionPattern = Pattern.compile("%.\\w+.%");
		Matcher matcher = regionPattern.matcher(docContent);
		while (matcher.find()) {
			int start = matcher.start();
			int end = matcher.end();
			
			regions.add(docContent.substring(start, end));
		}
		String fixedValue = null;
		String regionContent = null;
		
		String region = null;
		String fixedRegion = null;
		int sameRegionRepeatTime = 0;
		for (int i = 0; i < regions.size(); i++) {
			region = regions.get(i);
			
			fixedValue = fixValue(region, linkToBase);
			fixedValue = StringHandler.replace(fixedValue, CoreConstants.PERCENT, CoreConstants.EMPTY);
			fixedRegion = StringHandler.replace(region, CoreConstants.PERCENT, CoreConstants.EMPTY);
			sameRegionRepeatTime = 0;
			while (docContent.indexOf(region) != -1) {
				if (sameRegionRepeatTime > 0) {
					//	Adding region with number at the end
					fixedValue = new StringBuffer(fixedValue).append(sameRegionRepeatTime).toString();
					
					theme.addExtraRegion(fixedRegion, fixedValue);
				}
				sameRegionRepeatTime++;
				
				if (fixedRegion.equals("title")) {
					regionContent = getApplicationSettings().getProperty(fixedRegion);
					regionContent = regionContent == null ? "Idega template" : regionContent;
				}
				else {
					regionContent = getRegion(fixedValue);
				}
				
				if (ThemesConstants.REGIONS_NEEDED_TO_CREATE.contains(fixedValue)) {
					regionContent = getRegionDiv(fixedValue, regionContent);
				}
				
				docContent = docContent.replaceFirst(region, regionContent);
			}
		}
		return docContent;
	}
	
	private IWMainApplicationSettings getApplicationSettings() {
		return IWMainApplication.getDefaultIWMainApplication().getSettings();
	}
	
	private String getRegionDiv(String id, String content) {
		return new StringBuffer("<div id=\"").append(id).append("\">").append(content).append("</div>").toString();
	}
	
	/**
	 * Replacing useless content with empty String
	 * @param value
	 * @return String
	 */
	private String fixValue(String value, String linkToBase) {
		value = getFixedDocumentContent(value);
		
		String replace = CoreConstants.EMPTY;
		if (value.indexOf(ThemesConstants.USELESS_DATA_ELEMENT) != -1) {
			replace = linkToBase;
		}
		value = StringHandler.replace(value, ThemesConstants.USELESS_PATHTO_ELEMENT, replace);
		
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
		}
		
		fixSiteRegion(e, "h1", ThemesConstants.SITE_TITLE);
		fixSiteRegion(e, "h2", ThemesConstants.SITE_SLOGAN);
		
		if (regionID != null) {
			if (regionID.equals(REGION_TO_EXPAND)) {
				e.addContent(getExpandRegion()); // Expanding theme
			}
		}
		
		if (!hasElementChildren(e, false)) {
			e.addContent(getComment(IDEGA_COMMENT));
		}
		
		return true;
	}
	
	private String getDummyCategories() {
		StringBuilder content = new StringBuilder();		
		content.append("<div id=\"blog-categories\">");
		
		int index = 0;
		for (String category: DUMMY_CATEGORIES) {
			if (index % 2 == 0) {
				content.append("<div class=\"blog-category-link-disabled\">").append(category).append("</div>");
			}
			else {
				content.append("<a href=\"javascript:void(0)\" class=\"blog-category-link-enabled\">").append(category).append("</a>");
				content.append("<br />");
			}
			
			index++;
		}
		
		content.append("</div>");
		return content.toString();
	}
	
	@SuppressWarnings("unchecked")
	private boolean hasElementChildren(Element e, boolean checkContentOnly) {
		if (e == null) {
			return false;
		}
		
		if (checkContentOnly) {
			List content = e.getContent();
			if (ListUtil.isEmpty(content)) {
				return false;
			}
			return true;
		}
		
		List children = e.getChildren();
		if (ListUtil.isEmpty(children)) {
			return false;
		}
		
		return true;
	}
	
	@SuppressWarnings("unchecked")
	private boolean fixSiteRegion(Element e, String heading, String headingKeyword) {
		if (e == null || heading == null) {
			return false;
		}
		
		List<Element> headings = e.getChildren(heading, namespace);
		if (ListUtil.isEmpty(headings)) {
			return false;
		}
		
		String propKey = new StringBuilder(ThemesConstants.THEMES_PROPERTY_START).append(headingKeyword).append(ThemesConstants.THEMES_PROPERTY_END).toString();
		Collection<Object> contents = null;
		List<Text> oldTexts = null;
		for (Element headingElement: headings) {
			String text = headingElement.getTextNormalize();
			if (!StringUtil.isEmpty(text) && text.indexOf(headingKeyword) != -1) {
				Attribute id = headingElement.getAttribute("id");
				if (id == null) {
					id = new Attribute("id", headingKeyword);
					headingElement.setAttribute(id);
				}
				
				contents = headingElement.getContent();
				if (!ListUtil.isEmpty(contents)) {
					for (Object o: contents) {
						if (o instanceof Text) {
							if (ListUtil.isEmpty(oldTexts)) {
								oldTexts = new ArrayList<Text>();
							}
							oldTexts.add((Text) o);
						}
					}
				}
				
				//	Adding region definition
				headingElement.addContent(getCommentsCollection(headingKeyword));
				
				//	Adding text
				String newText = getApplicationSettings().getProperty(propKey);
				headingElement.addContent(headingElement.getContentSize() - 1, new Text(StringUtil.isEmpty(newText) ? CoreConstants.EMPTY : newText));
				
				if (!ListUtil.isEmpty(oldTexts)) {
					for (Iterator<Text> oldTextsIter = oldTexts.iterator(); oldTextsIter.hasNext();) {
						oldTextsIter.next().detach();
					}
				}
			}
		}
		
		return true;
	}
	
	private boolean clearThemeVariationsFromCache(String themeID) {
		return helper.clearVariationFromCache(themeID);
	}
	
	/**
	 * Changes theme with new style variation, creates draft and creates new preview image
	 * @param themeKey
	 * @param styleGroupName
	 * @param variation
	 * @return String
	 */
	public String changeTheme(String themeKey, String themeName, ThemeChange change, boolean lastChange) {		
		if (themeKey == null || change == null) {
			return null;
		}
		Theme theme = helper.getTheme(themeKey);
		if (theme == null) {
			return null;
		}
		Document doc = getThemeDocument(theme.getId());
		if (doc == null) {
			return null;
		}
		
		String changed = changeTheme(doc, theme, themeName, change, lastChange);
		if (changed == null) {
			return null;
		}
		
		if (lastChange) {
			if (finishThemeChange(theme, doc, true)) {
				return themeKey;
			}
		}
		else {
			return themeKey;
		}
		
		return null;
	}
	
	private boolean finishThemeChange(Theme theme, Document doc, boolean generateOnlyBigPreview) {
		String draft = new StringBuffer(helper.getFileName(theme.getLinkToSkeleton())).append(ThemesConstants.DRAFT).toString();
		if (!uploadDocument(doc, theme.getLinkToBaseAsItIs(), helper.decode(draft, true), theme, true)) {
			return false;
		}
		
		theme.setLinkToDraft(new StringBuffer(theme.getLinkToBase()).append(draft).toString());
		
		clearThemeVariationsFromCache(theme.getId());
		
		return true;
	}
	
	private String changeTheme(Document doc, Theme theme, String themeName, ThemeChange change, boolean lastChange) {
		if (doc == null || theme == null || change == null) {
			return null;
		}
		
		String styleGroupName = change.getStyleGroupName();
		String variation = change.getVariation();
		if (styleGroupName == null || variation == null) {
			return null;
		}
		boolean checked = change.isEnabled();
		
		theme.setChangedName(themeName);
		boolean limitedSelection = true;
		
		ThemeStyleGroupMember oldStyle = null;
		ThemeStyleGroupMember newStyle = null;
		ThemeStyleGroupMember styleChanger = null;
		boolean needUpload = true;
		if (change.isRadio()) {
			//	Simply changing CSS files
			oldStyle = getEnabledStyleMember(theme, styleGroupName);
			newStyle = getStyleMember(theme, styleGroupName, variation);
			styleChanger = oldStyle;
		} else if (change.isColor()) {
			//	Changing color in CSS file
			needUpload = false;
			if (!changeThemeColourVariation(theme, change, lastChange)) {
				return null;
			}
		} else {
			//	Multiple variations, need to know either add CSS or remove
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
		
		if (needUpload) {
			Element root = doc.getRootElement();
			if (root == null) {
				return null;
			}
			String linkToBase = new StringBuffer(CoreConstants.WEBDAV_SERVLET_URI).append(theme.getLinkToBase()).toString();
			if (!changeThemeStyle(linkToBase, root.getChild(HTML_HEAD, namespace), oldStyle, newStyle)) {
				return null;
			}
		}
		if (oldStyle != null) {
			oldStyle.setEnabled(false);
		}
		if (newStyle != null) {
			newStyle.setEnabled(true);
		}
		
		boolean addThemeChange = styleChanger == null ? false : true;
		if (oldStyle != null && newStyle != null) {
			addThemeChange = !(oldStyle == newStyle);
		}
		if (addThemeChange) {
			addThemeChange(theme, styleChanger, limitedSelection);
		}
		
		return theme.getId();
	}
	
	private boolean changeThemeColourVariation(Theme theme, ThemeChange change, boolean lastChange) {
		if (theme == null || change == null) {
			return false;
		}
		
		String variable = change.getVariable();
		String value = change.getVariation();
		if (variable == null || value == null) {
			return false;
		}
		
		ThemeStyleGroupMember colourVariation = getColorGroupMember(theme, variable);
		if (colourVariation != null) {
			addThemeChange(theme, colourVariation, true);
			theme.addStyleVariable(variable, value);
		}
		
		if (lastChange) {
			return setValuesToColourFiles(theme);
		}
		
		return true;
	}
	
	public ThemeStyleGroupMember getColorGroupMember(Theme theme, String variable) {
		if (theme == null || variable == null) {
			return null;
		}
		
		Map.Entry<String,ThemeStyleGroupMember> entry = null;
		ThemeStyleGroupMember member = null;
		boolean found = false;
		for (Iterator<Map.Entry<String,ThemeStyleGroupMember>> it = theme.getStyleGroupsMembers().entrySet().iterator(); (it.hasNext() && !found);) {
			entry = it.next();
			
			member = entry.getValue();
			if (!member.isStylesheet() && member.getVariable() != null) {
				if (variable.equals(member.getVariable())) {
					found = true;
				}
			}
		}
		
		return found ? member : null;
	}

	private void addThemeChange(Theme theme, ThemeStyleGroupMember style, boolean limitedSelection) {
		if (theme == null || style == null) {
			return;
		}
		ThemeChange change = new ThemeChange();
		change.setLimitedSelection(limitedSelection);
		change.setEnabled(style.isEnabled());
		change.setColor(!style.isStylesheet());
		
		change.setStyleGroupName(style.getGroupName());
		change.setVariation(style.getName());
		change.setVariable(style.getVariable());
		
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
		if (oldStyle != null && newStyle != null) {
			if (oldStyle == newStyle) {
				return true;
			}
		}
		
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
				if (attributeValue != null) {
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
		List<String> colourFiles = theme.getColourFiles();
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
				else if (!member.isStylesheet()) {
					if (colourFiles != null) {
						//	CSS files for colors variations
						for (int k = 0; k < colourFiles.size(); k++) {
							member.addStyleFile(colourFiles.get(k));
						}
						members.add(member);
					}
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
	 * @param themeKey
	 * @param themeName
	 * @return boolean
	 */
	public synchronized boolean saveTheme(String themeKey, String themeName) {
		if (themeKey == null || themeName == null) {
			return false;
		}
		
		Theme theme = helper.getTheme(themeKey);
		if (theme == null) {
			return false;
		}
		
		if (isNewName(themeName, theme)) {
			logger.log(Level.INFO, "Creating new theme: " + themeName);
			return createNewTheme(theme, themeName);
		}
		
		String linkToTheme = theme.getLinkToDraft();
		if (linkToTheme == null) {
			linkToTheme = theme.getLinkToSkeleton();
		}
		if (linkToTheme == null) {
			return false;
		}

		InputStream is = null;
		is = helper.getInputStream(new StringBuffer(helper.getFullWebRoot()).append(linkToTheme).toString());
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
			IOUtil.closeInputStream(is);
		}
		theme.setLocked(false);
		
		theme.setLinkToDraft(null);
		
		helper.createSmallImage(theme, false);
		
		try {
			helper.getThemesService().createBuilderTemplate(theme);
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
	
	private boolean restoreTheme(Theme theme, boolean fullyRestore) {
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
			
				if (change.isColor()) {
					theme.addStyleVariable(change.getVariable(), member.getColour());	//	Setting back default color value
				}
			}
		}
		
		setValuesToColourFiles(theme);	//	Setting back default colors
		
		if (fullyRestore) {
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
		}
		
		if (helper.generatePreviewsForTheme(theme, false, ThemesConstants.IS_THEME_PREVIEW_JPG, ThemesConstants.THEME_PREVIEW_QUALITY)) {
			theme.setChangedName(null);
			theme.setLinkToDraft(null);
			theme.setChanges(new ArrayList<ThemeChange>());
			theme.setCurrentlyUsedBuiltInStyleUri(null);
			
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
							if (styleHref != null && !isDefaultStyle(styleHref, theme) && !isColorVariation(theme.getColourFiles(), styleHref)) {
								ThemeStyleGroupMember member = getExistingDefaultStyle(styleHref, styles);
								if (member == null) {
									stylesToRemove.add(child);	//	Style is needless: it's not declared as default in properties list
									stylesIndexes.put(getStyleGroupNameByHref(theme, styleHref), getElementIndex(header.getContent(),
													ThemesConstants.TAG_ATTRIBUTE_HREF, child.getAttributeValue(ThemesConstants.TAG_ATTRIBUTE_HREF)));
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
			if (member.isStylesheet()) {
				if (!addContentToElement(header, getNewStyleElement(linkToBase, member), stylesIndexes.get(member.getGroupName()))) {
					return false;
				}
			}
		}
		
		for (Iterator<Element> it = stylesToRemove.iterator(); it.hasNext(); ) {
			it.next().detach();
		}
		
		return uploadTheme(doc, theme);
	}
	
	private boolean isColorVariation(List<String> cssFiles, String styleHref) {
		if (cssFiles == null || styleHref == null) {
			return false;
		}
		
		for (int i = 0; i < cssFiles.size(); i++) {
			if (styleHref.endsWith(cssFiles.get(i))) {
				return true;
			}
		}
		
		return false;
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
	
	private boolean isDefaultStyle(String cssHref, Theme theme) {
		if (cssHref == null) {
			return false;
		}
		
		for (int i = 0; i < ThemesConstants.DEFAULT_STYLE_FILES.size(); i++) {
			if (cssHref.endsWith(ThemesConstants.DEFAULT_STYLE_FILES.get(i))) {
				return true;
			}
		}
		
		//	Didn't find CSS in static list, will check if CSS is defined in any variation. If not - it's 'default' CSS
		Collection<ThemeStyleGroupMember> allVariations = theme.getStyleGroupsMembers().values();
		ThemeStyleGroupMember styleGroup = null;
		for (Iterator<ThemeStyleGroupMember> it = allVariations.iterator(); it.hasNext();) {
			styleGroup = it.next();
			
			List<String> files = styleGroup.getStyleFiles();
			for (int j = 0; j < files.size(); j++) {
				if (cssHref.endsWith(files.get(j))) {
					return false;	//	It's defined in variations
				}
			}
		}
		
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
		
		return restoreTheme(theme, true);
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
	
	private boolean replaceColourFilesInDoc(Document doc, String linkToBase, List<String> oldFiles, List<String> newFiles) {
		if (doc == null || oldFiles == null || newFiles == null) {
			return false;
		}
		
		if (oldFiles.size() != newFiles.size()) {
			return false;
		}
		
		Element html = doc.getRootElement();
		if (html == null) {
			return false;
		}
		
		Element head = html.getChild(HTML_HEAD, namespace);
		if (head == null) {
			return false;
		}
		
		ThemeStyleGroupMember oldStyle = null;
		ThemeStyleGroupMember newStyle = null;
		for (int i = 0; i < oldFiles.size(); i++) {
			oldStyle = new ThemeStyleGroupMember();
			oldStyle.addStyleFile(oldFiles.get(i));
			
			newStyle = new ThemeStyleGroupMember();
			newStyle.addStyleFile(newFiles.get(i));
			
			if (!changeThemeStyle(linkToBase, head, oldStyle, newStyle)) {
				return false;
			}
		}
		
		return true;
	}
	
	private boolean createThemeSkeleton(Theme parent, Theme child, String linkToTheme) {
		if (parent == null || child == null || linkToTheme == null) {
			return false;
		}
		
		Document themeDoc = helper.getXMLDocument(new StringBuffer(helper.getFullWebRoot()).append(linkToTheme).toString(), false, false); 
		if (themeDoc == null) {
			return false;
		}
		
		if (parent.hasColourFiles()) {
			//	Preparing color files
			if (!createCopiesForColourFiles(child, parent.getColourFiles(), parent.getName(), true)) {
				return false;
			}
			
			String linkToBase = new StringBuffer(CoreConstants.WEBDAV_SERVLET_URI).append(child.getLinkToBase()).toString();
			if (!replaceColourFilesInDoc(themeDoc, linkToBase, parent.getColourFiles(), child.getColourFiles())) {
				return false;
			}
		}
		
		uploadTheme(themeDoc, child);
		
		return true;
	}
	
	private synchronized boolean createNewTheme(Theme parent, String newName) {
		String linkToTheme = parent.getLinkToDraft();
		if (linkToTheme == null) {
			linkToTheme = parent.getLinkToSkeleton();
		}
		if (linkToTheme == null) {
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
		String themeName = helper.getPreparedThemeNameToUseInRepository(tempName);
		
		//	Adding theme to system
		String tempLink = new StringBuffer(decodedLinkToBase).append(themeName).toString();
		String themeKey = null;
		try {
			themeKey = helper.getThemesLoader().createNewTheme(tempLink, new StringBuffer(linkToBase).append(helper.encode(themeName, true)).toString(), true, true);
		} catch (Exception e) {
			e.printStackTrace();
			return false;
		}
		if (themeKey == null) {
			return false;
		}
		Theme child = helper.getTheme(themeKey);
		if (child == null) {
			return false;
		}
		child.setName(newName);

		//	Copying settings from parent theme
		if (!copyTheme(parent, child)) {
			return false;
		}
		
		//	Copying Theme skeleton
		if (!createThemeSkeleton(parent, child, linkToTheme)) {
			return false;
		}
		
		//	Restoring original theme
		restoreTheme(parent, false);
		
		//	Generating previews
		 if (!helper.generatePreviewsForTheme(child, false, ThemesConstants.IS_THEME_PREVIEW_JPG, ThemesConstants.THEME_PREVIEW_QUALITY)) {
			 return false;
		 }
		
		//	Marking properties extracted
		child.setPropertiesExtracted(true);
		
		// Creating new template
		try {
			helper.getThemesService().createBuilderTemplate(child);
		} catch (RemoteException e) {
			e.printStackTrace();
			return false;
		}
		
		//	Creating configuration file
		helper.createThemeConfig(child);
		
		return true;
	}
	
	/**
	 * Copies parent theme's style groups and variations to child theme
	 * @param parent
	 * @param child
	 */
	private boolean copyTheme(Theme parent, Theme child) {
		List <String> groupNames = parent.getStyleGroupsNames();
		ThemeStyleGroupMember member = null;
		ThemeStyleGroupMember parentMember = null;
		String styleGroupName = null;
		String colourValue = null;
		Map <String, ThemeStyleGroupMember> styleMembers = parent.getStyleGroupsMembers();
		for (int i = 0; i < groupNames.size(); i++) {
			styleGroupName = groupNames.get(i);
			child.addStyleGroupName(styleGroupName);
			int j = 0;
			parentMember = getMember(styleMembers, styleGroupName, j);
			while (parentMember != null) {
				member = new ThemeStyleGroupMember(parentMember);
				colourValue = parent.getStyleVariableValue(member.getVariable());
				if (colourValue != null) {
					member.setColour(colourValue);
				}
				child.addStyleGroupMember(new StringBuffer(styleGroupName).append(ThemesConstants.AT).append(j).toString(), member);
				j++;
				parentMember = getMember(styleMembers, styleGroupName, j);
			}
		}
		
		if (!createCopiesForColourFiles(child, parent.getColourFiles(), null, false)) {
			return false;
		}
		
		List<String> styleVariables = parent.getStyleVariablesKeys();
		String variable = null;
		for (int i = 0; i < styleVariables.size(); i++) {
			variable = styleVariables.get(i);
			child.addStyleVariable(variable, parent.getStyleVariableValue(variable));
		}
		
		child.setExtraRegions(new ArrayList<AdvancedProperty>(parent.getExtraRegions()));
		
		child.setCurrentlyUsedBuiltInStyleUri(parent.getCurrentlyUsedBuiltInStyleUri());
		for (BuiltInThemeStyle style: parent.getBuiltInThemeStyles()) {
			child.addBuiltInStyle(new BuiltInThemeStyle(helper.getBuiltInThemeStyleId(child), style));
		}
		
		return true;
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
	
	public String applyMultipleChangesToTheme(String themeKey, List<ThemeChange> changes, String themeName) {
		if (themeKey == null) {
			return null;
		}
		
		Theme theme = helper.getTheme(themeKey);
		if (theme == null) {
			return null;
		}
		Document doc = getThemeDocument(themeKey);
		if (doc == null) {
			return null;
		}
		
		String changed = themeKey;
		if (changes != null) {
			ThemeChange change = null;
			for (int i = 0; (i < changes.size() && changed != null); i++) {
				change = changes.get(i);
				changed = changeTheme(doc, theme, themeName, change, (i + 1) == changes.size());
			}
		}
		
		if (changed == null) {
			return null;
		}
		
		if (finishThemeChange(theme, doc, true)) {
			return themeKey;
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
		theme.clearProperties();																//	Clearing properties
		try {
			//	Extracting new properties (also setting default state)
			helper.getThemesPropertiesExtractor().prepareTheme(checkConfig, theme, null, null, null);
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

	public boolean setBuiltInStyle(String themeId, String builtInStyleId) throws Exception {
		if (themeId == null || builtInStyleId == null) {
			return false;
		}
		
		Theme theme = helper.getTheme(themeId);
		if (theme == null) {
			return false;
		}
		
		if (builtInStyleId.equals(ThemesConstants.DEFAULT_THEME_STYLE_ID)) {
			String currentlyUsedStyledUri = theme.getCurrentlyUsedBuiltInStyleUri();
			if (currentlyUsedStyledUri == null || ThemesConstants.MINUS_ONE.equals(currentlyUsedStyledUri)) {
				return false;
			}
			return restoreTheme(theme, true);
		}
		
		BuiltInThemeStyle style = theme.getBuiltInThemeStyle(builtInStyleId);
		if (style == null) {
			return false;
		}
		
		String currentUri = theme.getCurrentlyUsedBuiltInStyleUri();
		if (currentUri != null && currentUri.equals(style.getUri())) {
			return false;
		}
		
		Map<String, String> colors = style.getColours();
		Map<String, String> variations = style.getVariations();
		
		List<ThemeChange> changes = getMultipleThemeChanges(variations, themeId, false);
		changes.addAll(getMultipleThemeChanges(colors, themeId, true));
		
		if (changes.isEmpty()) {
			return false;
		}
		
		theme.setCurrentlyUsedBuiltInStyleUri(style.getUri());
		if (applyMultipleChangesToTheme(themeId, changes, theme.getName()) == null) {
			theme.setCurrentlyUsedBuiltInStyleUri(null);
			return false;
		}
		
		return true;
	}
	
	private List<ThemeChange> getMultipleThemeChanges(Map<String, String> info, String themeId, boolean colorChanges) {
		List<ThemeChange> changes = new ArrayList<ThemeChange>();
		if (info == null || info.isEmpty()) {
			return changes;
		}
		
		ThemeChange change = null;
		String keyValue = null;
		for (Iterator<String> key = info.keySet().iterator(); key.hasNext();) {
			keyValue = key.next();
			change = new ThemeChange();
			
			change.setThemeId(themeId);
			change.setStyleGroupName(keyValue);
			change.setVariation(info.get(keyValue));
			
			change.setRadio(!colorChanges);
			change.setEnabled(true);
			change.setVariable(keyValue);
			change.setColor(colorChanges);
			change.setPredefinedStyle(false);
			
			changes.add(change);
		}
		
		return changes;
	}

}