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
import java.util.regex.Matcher;
import java.util.regex.Pattern;

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
import org.springframework.context.annotation.Scope;
import org.springframework.stereotype.Service;

import bsh.Interpreter;

import com.idega.builder.bean.AdvancedProperty;
import com.idega.content.business.ContentConstants;
import com.idega.content.themes.helpers.bean.Theme;
import com.idega.content.themes.helpers.bean.ThemeChange;
import com.idega.content.themes.helpers.bean.ThemeStyleGroupMember;
import com.idega.idegaweb.IWMainApplication;
import com.idega.idegaweb.IWMainApplicationSettings;
import com.idega.presentation.IWContext;
import com.idega.slide.business.IWSlideService;
import com.idega.util.CoreConstants;
import com.idega.util.CoreUtil;
import com.idega.util.IWTimestamp;
import com.idega.util.ListUtil;
import com.idega.util.StringHandler;

@Scope("singleton")
@Service(ThemeChanger.SPRING_BEAN_IDENTIFIER)
public class ThemeChangerBean implements ThemeChanger {

	private static final Log log = LogFactory.getLog(ThemeChangerBean.class);
	
	// These are defaults in RapidWeaver, we are using its to generate good preview
	private static final String IMAGE_START = "<img src=";
	private static final String IMAGE_POSITION = "style=\"margin: 2px; float: ";
	private static final String IMAGE_END = " />";
	private static final String CONTENT_PARAGRAPH_TITLE = "<div class=\"blog-entry\"><h1 class=\"blog-entry-title\">";
	private static final String CONTENT_PARAGRAPH_DATE = "</h1><div class=\"blog-entry-date\">";
	private static final String CONTENT_PARAGRAPH_LINK = "</div><div class=\"blog-entry-body\">";
	private static final String CONTENT_PARAGRAPH_END = "</div></div><br /><br />";
	
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
	
	private static final String[] TOOLBAR_NAV_MENU = new String[] {"Frontpage", "Products", "Customers", "Partners", "The Company", "News"};
	
	private String[] _validLinkTagAttributes = new String[] {"charset", HREF, "hreflang", "media", "rel", "rev", "target", "type", "title"};
	private List<String> validLinkTagAttributes = Collections.unmodifiableList(Arrays.asList(_validLinkTagAttributes));
	
	private String[] _unChangedCaseAttributes = new String[] {HREF, "src"};
	private List<String> unChangedCaseAttributes = Collections.unmodifiableList(Arrays.asList(_unChangedCaseAttributes));
	
	private ThemesHelper helper = null;
	private Namespace namespace = Namespace.getNamespace(ThemesConstants.NAMESPACE);
	private XMLOutputter out = null;
	private Interpreter mathInterpreter = null;
	
	private String[] _themeHeadVariables = new String[] {"%header%", "%style_variations%", "%user_styles%", "%user_javascript%",
														"%plugin_header%", "%user_header%"};
	private List<String> themeHeadVariables = Collections.unmodifiableList(Arrays.asList(_themeHeadVariables));
	
	private String[] _regularExpressionsForNeedlessStuff = new String[] {/*" xmlns.+\"", */"..<?doc.+?>"};
	private List<String> regularExpressionsForNeedlessStuff = Collections.unmodifiableList(Arrays.asList(_regularExpressionsForNeedlessStuff));
	
	public ThemeChangerBean() {
		out = new XMLOutputter();
		out.setFormat(Format.getPrettyFormat());
		
		mathInterpreter = new bsh.Interpreter();
		
		helper = ThemesHelper.getInstance();
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
		if (content == null || CoreConstants.EMPTY.equals(content)) {
			return false;
		}
		
		String variable = null;
		String value = null;
		for (int i = 0; i < themeHeadVariables.size(); i++) {
			variable = themeHeadVariables.get(i);
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
	
	/**
	 * RapidWeaver theme consists in-proper data for valid XHTML document, so needs to be fixed
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
		for (Iterator <Text> itt = textElements.iterator(); itt.hasNext(); ) {
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
						checkElementAttributes(element, validLinkTagAttributes);
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
		
		return uploadTheme(out.outputString(doc), theme, true);
	}
	
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
					proceedStyleFile(linkToTheme, getFixedDocumentContent(cssLink));
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
				if (!proceedStyleFile(theme.getLinkToBase(), new StringBuffer(theme.getLinkToBase()).append(files.get(index)).toString())) {
					invalidFiles.add(files.get(index));	//	Invalid CSS file, disabling variation
				}
			}
			i++;
		}
		removeVariationsFromTheme(theme, invalidFiles);
		
		return true;
	}
	
	private boolean setValuesToColourFiles(Theme theme) {
		if (theme == null) {
			return false;
		}
		
		if (!theme.hasColourFiles()) {
			return true;
		}
		
		List<String> colourFiles = theme.getColourFiles();
		IWSlideService slide = helper.getSlideService();
		if (slide == null) {
			return false;
		}
		
		List<String> keys = theme.getStyleVariablesKeys();
		
		String file = null;
		String webRoot = helper.getFullWebRoot();
		String sourceLink = null;
		InputStream stream = null;
		String key = null;
		for (int i = 0; i < colourFiles.size(); i++) {
			file = colourFiles.get(i);
			
			sourceLink = new StringBuffer(webRoot).append(theme.getLinkToBase()).append(helper.getThemeColourFileName(theme, null, file, true)).toString();
			stream = helper.getInputStream(sourceLink);
			if (stream == null) {
				return false;
			}
			
			String content = null;
			try {
				content = StringHandler.getContentFromInputStream(stream);
			} catch (Exception e) {
				e.printStackTrace();
			} finally {
				helper.closeInputStream(stream);
			}
			if (content == null) {
				return false;
			}
			
			for (int j = 0; (j < keys.size() && content != null); j++) {
				key = keys.get(j);
				content = replaceColourFileContent(content, key, theme.getStyleVariableValue(key));
			}
			
			content = checkIfAllVariablesReplaced(content, theme);
			
			if (content == null) {
				return false;
			}
			
			try {
				if (!(slide.uploadFileAndCreateFoldersFromStringAsRoot(theme.getLinkToBase(), file, content, CoreConstants.CONTENT_TYPE_TEXT_CSS, true))) {
					return false;
				}
			} catch (RemoteException e) {
				e.printStackTrace();
				return false;
			}
		}
		
		return true;
	}
	
	private String checkIfAllVariablesReplaced(String content, Theme theme) {
		if (content == null || theme == null) {
			return null;
		}
		
		String defaultColour = "#ffffff";
		int start = -1;
		int end = -1;
		List<String> searchTerm = ListUtil.convertStringArrayToList(new String[] {CoreConstants.PERCENT});
		String cssExpression = null;
		String fixedCssExpression = null;
		String variable = null;
		String styleValue = null;
		ThemeStyleGroupMember colourVariation = null;
		while (content.indexOf(CoreConstants.PERCENT) != -1) {
			cssExpression = null;
			fixedCssExpression = null;
			variable = null;
			styleValue = null;
			colourVariation = null;
			
			start = getStartIndexForCssVariable(content, CoreConstants.PERCENT, CoreConstants.PERCENT);
			end = getEndIndexForCssVariable(content, CoreConstants.PERCENT, searchTerm, true, true);
			
			if (!(canSubstring(content, start, end))) {
				return content;
			}

			cssExpression = content.substring(start, end);
			variable = StringHandler.remove(cssExpression, CoreConstants.PERCENT);
			styleValue = theme.getStyleVariableValue(variable);
			if (styleValue == null) {
				colourVariation = getColorGroupMember(theme, variable);
				if (colourVariation != null) {
					styleValue = colourVariation.getColour();
				}
			}
			if (styleValue == null) {
				styleValue = defaultColour;
			}
			
			fixedCssExpression = computeCssValue(cssExpression, variable, styleValue);
			content = StringHandler.replace(content, cssExpression, fixedCssExpression);
		}
		
		return content;
	}
	
	private String replaceColourFileContent(String content, String variable, String value) {
		if (content == null) {
			return null;
		}
		if (variable == null || value == null) {
			return null;
		}
		
		int start = -1;
		int end = -1;
		String originalValue = null;
		String cssValue = null;
		List<String> searchTerm = new ArrayList<String>();
		searchTerm.add(CoreConstants.PERCENT);
		while (content.indexOf(variable) != -1) {
			start = getStartIndexForCssVariable(content, variable, CoreConstants.PERCENT);
			end = getEndIndexForCssVariable(content, variable, searchTerm, true, true);
			if (!canSubstring(content, start, end)) {
				return value;	//	Error
			}
			
			originalValue = content.substring(start, end);
			cssValue = computeCssValue(originalValue, variable, value);
			if (cssValue == null) {
				return value;	//	Error
			}
			else if (cssValue.length() != 4 && cssValue.length() != 7) {
				return value;
			}
			
			content = StringHandler.replace(content, originalValue, cssValue);
		}
		
		return content;
	}
	
	private String computeCssValue(String originalValue, String variable, String value) {
		if (originalValue.indexOf(CoreConstants.PLUS) == -1 && originalValue.indexOf(CoreConstants.MINUS) == -1 && originalValue.indexOf(CoreConstants.STAR) == -1) {
			return value;
		}
		
		originalValue = originalValue.toLowerCase();
		originalValue = StringHandler.replace(originalValue, CoreConstants.PERCENT, CoreConstants.EMPTY);	//	Removing '%'
		
		if (originalValue.indexOf("r(") == -1 && originalValue.indexOf("g(") == -1 && originalValue.indexOf("b(") == -1) {
			originalValue = StringHandler.replace(originalValue, variable, value);							//	Replacing variable with value
		}
		else {
			originalValue = getComputedHeximalValueByRGB(originalValue, variable, value);					//	Making expression
			if (originalValue == null) {
				return value;
			}
		}
		
		originalValue = getCssExpressionWithDecimalNumbers(originalValue);									//	Hex numbers replaced with decimal
		if (originalValue == null) {
			return value;
		}
		
		Object o = null;
		try {
			o = mathInterpreter.eval(originalValue);
		} catch (Exception e) {
			e.printStackTrace();
			return value;
		}
		if (o == null) {
			return value;
		}
		
		String computedValue = String.valueOf(o);
		int index = computedValue.indexOf(CoreConstants.DOT);
		if (index != -1) {
			computedValue = StringHandler.replace(computedValue, computedValue.substring(index), CoreConstants.EMPTY);
			index = -1;
		}
		
		try {
			return new StringBuffer(CoreConstants.NUMBER_SIGN).append(Integer.toHexString(new Integer(computedValue))).toString();
		} catch(Exception e) {
			e.printStackTrace();
			return value;
		}
	}
	
	private String getComputedHeximalValueByRGB(String originalValue, String variable, String value) {
		//	TODO not finished RGB stuff!
		if (originalValue == null || variable == null || value == null) {
			return null;
		}
		
		if (value.startsWith(CoreConstants.NUMBER_SIGN)) {
			value = value.substring(1);
		}
		
		/*String doubleRegex = "(?i)(?:"+
		"\\b\\d+\\.\\d*(?:e[+-]?\\d+|)[fF]?|"+// ex: 34., 3.E2
		"\\d*\\.\\d+(?:e[+-]?\\d+|)[fF]?|"+ // ex: .2, .994e+4
		"\\b\\d+e[+-]?\\d+[fF]?|"+ // ex: 1e-15
		"\\b\\d+[fF]"+ // ex: 22f
		")[fF]?";
//		String intRegex = "\\d*";
		
		List<String> rgbRegexs = new ArrayList<String>();
//		rgbRegexs.add("[r][\\p{Punct}]["+doubleRegex+"][\\p{Punct}][g][\\p{Punct}]["+doubleRegex+"][\\p{Punct}][b][\\p{Punct}]["+doubleRegex+"][\\p{Punct}]");
//		rgbRegexs.add("[r][\\p{Punct}][\\p{Digit}*][\\p{Punct}][g][\\p{Punct}][\\p{Digit}*][\\p{Punct}][b][\\p{Punct}][\\p{Digit}*][\\p{Punct}]");
		rgbRegexs.add(doubleRegex);
//		rgbRegexs.add(intRegex);
		List<String> rgbs = getValuesByPatterns(originalValue, rgbRegexs);
		if (rgbs == null || rgbs.size() == 0) {
			return null;
		}
		
		String[] valueParts = new String[3];
		String valuePart = null;
		if (value.length() == 3) {
			valuePart = value.substring(0, 1);
			valueParts[0] = new StringBuffer(valuePart).append(valuePart).toString();	//	Red
			
			valuePart = value.substring(1, 2);
			valueParts[1] = new StringBuffer(valuePart).append(valuePart).toString();	//	Green
			
			valuePart = value.substring(2);
			valueParts[2] = new StringBuffer(valuePart).append(valuePart).toString();	//	Blue
		}
		else {
			valueParts[0] = value.substring(0, 2);	//	Red
			valueParts[1] = value.substring(2, 4);	//	Green
			valueParts[2] = value.substring(4);		//	Blue
		}
		
//		System.out.println(rgbs);*/
		return null;
	}
	
	/*private List<String> getValuesByPatterns(String source, List<String> regexs) {
		if (source == null || regexs == null) {
			return null;
		}
		
		List<String> destination = new ArrayList<String>();
		Pattern p = null;
		Matcher m = null;
		String value = null;
		
		for (int i = 0; i < regexs.size(); i++) {
			try {
				p = Pattern.compile(regexs.get(i));
				m = p.matcher(source);
				while (m.find()) {
					int start = m.start();
					int end = m.end();
					if (canSubstring(source, start, end)) {
						value = source.substring(start, end);
						try {
							Double.valueOf(value);
							destination.add(value);
						} catch(Exception e){
							e.printStackTrace();
						}
					}
				}
			} catch(Exception e) {
			}
		}
		
		return destination;
	}*/
	
	private String getCssExpressionWithDecimalNumbers(String expression) {
		if (expression.indexOf(CoreConstants.NUMBER_SIGN) == -1) {
			return expression;
		}
		
		int start = -1;
		int end = -1;
		String key = CoreConstants.NUMBER_SIGN;
		String[] _searchTerms = {"0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "a", "b", "c", "d", "e", "f", "A", "B", "C", "D", "E", "F"};
		List<String> searchTerms = ListUtil.convertStringArrayToList(_searchTerms);
		String hexidecimal = null;
		int decimal = -1;
		while (expression.indexOf(CoreConstants.NUMBER_SIGN) != -1) {
			start = getStartIndexForCssVariable(expression, key, CoreConstants.NUMBER_SIGN);
			end = getEndIndexForCssVariable(expression, key, searchTerms, false, false);
			
			if (!(canSubstring(expression, start, end))) {
				return null;	//	Error
			}
			
			hexidecimal = expression.substring(start, end);
			try {
				decimal = Integer.decode(hexidecimal);
			} catch(Exception e) {
				e.printStackTrace();
				return null;
			}
			
			expression = StringHandler.replace(expression, hexidecimal, String.valueOf(decimal));
		}
		
		return expression;
	}
	
	private boolean canSubstring(String content, int start, int end) {
		if (content == null || start == -1 || end == -1) {
			return false;
		}
		
		if (start < 0 || end <= start || end > content.length()) {
			return false;
		}
		
		return true;
	}
	
	private int getEndIndexForCssVariable(String content, String key, List<String> searchTerms, boolean checkIfContains, boolean increaseIndex) {
		int index = content.indexOf(key);
		if (index == -1) {
			return -1;
		}
		
		index += key.length();
		
		boolean foundPercentMarkAtTheEnd = false;
		while (!foundPercentMarkAtTheEnd && index < content.length()) {
			if (checkIfContains) {
				if (searchTerms.contains(content.substring(index, index + 1))) {
					foundPercentMarkAtTheEnd = true;
				}
				else {
					index++;
				}
			}
			else {
				if (!(searchTerms.contains(content.substring(index, index + 1)))) {
					foundPercentMarkAtTheEnd = true;
				}
				else {
					index++;
				}
			}
		}
		
		if (increaseIndex) {
			if (index + 1 < content.length()) {
				index++;
			}
		}
		return index;
	}
	
	private int getStartIndexForCssVariable(String content, String key, String searchTerm) {
		int index = content.indexOf(key);
		if (index == -1) {
			return -1;
		}
		
		boolean foundPercentMarkAtTheBegin = false;
		while (!foundPercentMarkAtTheBegin && index > 0) {
			if (content.substring(index).startsWith(searchTerm)) {
				foundPercentMarkAtTheBegin = true;
			}
			else {
				index--;
			}
		}
		
		return index;
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
				helper.closeInputStream(stream);
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
			proceedStyleFile(theme.getLinkToBase(), new StringBuffer(theme.getLinkToBase()).append(defaultStyles.get(i)).toString());
		}
		return true;
	}
	
	private boolean proceedStyleFile(String linkToTheme, String linkToStyle) {
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
			log.error(new StringBuilder("Cann't get CSS file: '").append(linkToStyle).append("' from Theme pack!"));
			return false;
		}
		InputStreamReader isr = new InputStreamReader(is);
		BufferedReader buf = new BufferedReader(isr);
		CssScanner scanner = new CssScanner(buf, linkToTheme);
		
		helper.closeInputStream(is);
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
		for (int i = 0; i < regularExpressionsForNeedlessStuff.size(); i++) {
			expression = regularExpressionsForNeedlessStuff.get(i);
			
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
		return uploadDocument(out.outputString(doc), linkToBase, fileName, theme, isTheme, false);
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
					if (!unChangedCaseAttributes.contains(name)) {
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
	
	/*private Attribute getElementAtribute(Element e, String name, Namespace ns) {
		if (e == null || name == null) {
			return null;
		}
		
		Attribute a = null;
		if (ns == null) {
			a = e.getAttribute(name);
		}
		else {
			a = e.getAttribute(name, ns);
			if (a == null) {
				a = e.getAttribute(name);
			}
		}
		
		return a;
	}*/
	
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
		return new StringBuffer("<a href=\"javascript:void(0)\">").append(TOOLBAR_NAV_MENU[0]).append("</a>").append(NO_BREAK_STRING).append(NO_BREAK_STRING).toString();
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
			navMenu.append("</li>");
		}
		navMenu.append("</ul>");
		return navMenu.toString();
	}
	
	private String getContentReplace(String defaultValue) {
		StringBuffer content = new StringBuffer();
		if (defaultValue != null && !ThemesConstants.EMPTY.equals(defaultValue)) {
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
		
		IWTimestamp timestamp = new IWTimestamp(new Date());
		content.append(ThemesConstants.NEW_LINE);
		for (int i = 0; i < ThemesConstants.DUMMY_ARTICLES.size(); i++) {
			content.append(CONTENT_PARAGRAPH_TITLE).append(ThemesConstants.ARTICLE_TITLE);
			if (ThemesConstants.DUMMY_ARTICLES.size() > 1) {
				content.append(CoreConstants.SPACE).append(i + 1);
			}
			content.append(CONTENT_PARAGRAPH_DATE);
			content.append(timestamp.getLocaleDate(l, DateFormat.MEDIUM));
			content.append(CONTENT_PARAGRAPH_LINK);
			content.append(IMAGE_START).append(ThemesConstants.SINGLE_QUOTE);
			content.append(ThemesConstants.BASE_THEME_IMAGES);
			content.append(ThemesConstants.THEME_IMAGES.get(helper.getRandomNumber(ThemesConstants.THEME_IMAGES.size())));
			content.append(ThemesConstants.SINGLE_QUOTE).append(CoreConstants.SPACE).append(IMAGE_POSITION);
			content.append(ThemesConstants.IMAGE_POSITIONS.get(helper.getRandomNumber(ThemesConstants.IMAGE_POSITIONS.size())));
			content.append(ThemesConstants.SINGLE_QUOTE).append(IMAGE_END);
			content.append(ThemesConstants.DUMMY_ARTICLES.get(i)).append(CONTENT_PARAGRAPH_END);
		}
		content.append(ThemesConstants.NEW_LINE);
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
		
		if (value.equals(ThemesConstants.TOOLBAR)) {
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
		
		String propertyValue = getApplicationSettings().getProperty(key.toString());
		if (propertyValue != null) {
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
			
			region.append(propertyValue);
		}
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
		
		if (!hasElementChildren(e)) {
			e.addContent(getComment(IDEGA_COMMENT));
		}
		
		return true;
	}
	
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
		if (detachElement(e, heading, headingKeyword)) {
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
	
	private boolean detachElement(Element parent, String elementName, String content) {
		if (parent == null || elementName == null || content == null) {
			return false;
		}
		Element useless = parent.getChild(elementName, namespace);
		if (useless != null) {
			String text = useless.getTextNormalize();
			if (text == null) {
				return false;
			}
			if (text.indexOf(content) == -1) {
				return false;
			}
			useless.detach();
			return true;
		}
		return false;
	}
	
	private Collection <Element> getSimpleTextElement(String containerName, String propertyKey) {
		if (propertyKey == null) {
			return new ArrayList<Element>();
		}
		IWMainApplicationSettings settings = getApplicationSettings();
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
	 * @param themeKey
	 * @param styleGroupName
	 * @param variation
	 * @return String
	 */
	public String changeTheme(String themeKey, String themeName, ThemeChange change) {		
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
		
		String changed = changeTheme(doc, theme, themeName, change);
		if (changed == null) {
			return null;
		}
		
		if (finishThemeChange(theme, doc, true)) {
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

		if (!helper.generatePreviewsForTheme(theme, true, ThemesConstants.IS_THEME_PREVIEW_JPG, ThemesConstants.THEME_PREVIEW_QUALITY, generateOnlyBigPreview)) {
			theme.setLinkToDraft(null);
			return false;
		}
		
		clearThemeVariationsFromCache(theme.getId());
		
		return true;
	}
	
	private String changeTheme(Document doc, Theme theme, String themeName, ThemeChange change) {
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
			if (!changeThemeColourVariation(theme, change)) {
				return null;
			}
		} else if (change.isPredefinedStyle()) {
			//	Using predefined style
			//	TODO
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
		
		if (styleChanger != null) {
			addThemeChange(theme, styleChanger, limitedSelection);
		}
		
		return theme.getId();
	}
	
	/*private Element getChildElement(Element parent, String childName, Namespace ns) {
		if (parent == null || childName == null) {
			return null;
		}
		
		Element child = null;
		if (ns == null) {
			child = parent.getChild(childName);
		}
		else {
			child = parent.getChild(childName, ns);
			if (child == null) {
				child = parent.getChild(childName);
			}
		}
		
		return child;
	}*/
	
	private boolean changeThemeColourVariation(Theme theme, ThemeChange change) {
		if (theme == null || change == null) {
			return false;
		}
		
		String variable = change.getVariable();
		String value = change.getVariation();
		if (variable == null || value == null) {
			return false;
		}
		
		ThemeStyleGroupMember colourVariation = getColorGroupMember(theme, variable);
		if (colourVariation == null) {
			return false;
		}
		
		addThemeChange(theme, colourVariation, true);
		theme.addStyleVariable(variable, value);
		return setValuesToColourFiles(theme);
	}
	
	private ThemeStyleGroupMember getColorGroupMember(Theme theme, String variable) {
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
	
	/*private List<Element> getElementChildren(Element parent, String name, Namespace ns) {
		if (parent == null || name == null) {
			return null;
		}
		
		List<Element> children = null;
		if (ns == null) {
			children = parent.getChildren(name);
		}
		else {
			children = parent.getChildren(name, ns);
			if (children == null || children.size() == 0) {
				children = parent.getChildren(name);
			}
		}
		
		return children;
	}*/
	
	/**
	 * Changes theme's old style with new
	 * @param head
	 * @param oldStyle
	 * @param newStyle
	 * @return boolean
	 */
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
			log.info("Creating new theme: " + themeName);
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
			helper.closeInputStream(is);
		}
		theme.setLocked(false);
		
		theme.setLinkToDraft(null);
		if (theme.getLinkToDraftPreview() != null) {
			theme.setLinkToThemePreview(theme.getLinkToDraftPreview());
		}
		theme.setLinkToDraftPreview(null);
		
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
		
		InputStream is = helper.getInputStream(new StringBuffer(helper.getFullWebRoot()).append(linkToTheme).toString());
		if (is == null) {
			return false;
		}
		Document themeDoc = null; 
		try {
			themeDoc = helper.getXMLDocument(is);
		} catch (Exception e) {
			e.printStackTrace();
		} finally {
			helper.closeInputStream(is);
		}
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
		 if (!helper.generatePreviewsForTheme(child, false, ThemesConstants.IS_THEME_PREVIEW_JPG, ThemesConstants.THEME_PREVIEW_QUALITY, false)) {
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
				changed = changeTheme(doc, theme, themeName, change);
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
	
	/*private boolean isThemeChanged(Theme theme) {
		List<ThemeChange> changes = theme.getChanges();
		boolean existsChanges = true;
		if (changes == null || changes.size() == 0) {
			existsChanges = false;
		}
		
		return existsChanges;
	}*/
	
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