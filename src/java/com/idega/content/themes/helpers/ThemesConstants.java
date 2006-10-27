package com.idega.content.themes.helpers;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

public class ThemesConstants {
	
	protected static final String NAMESPACE = "http://www.w3.org/1999/xhtml";
	protected static final String NAMESPACE_ID = "xmlns";
	
	protected static final String CONTENT = "/content";
	protected static final String THEMES = "/themes/";
	public static final String THEMES_PATH = "/files/cms" + THEMES;
	public static final String THEMES_PREVIEW_PATH = THEMES_PATH + "preview/";
	
	protected static final String DOT = ".";
	public static final String SLASH = "/";
	public static final String AT = "@";
	public static final String EMPTY = "";
	protected static final String COMMA = ",";
	public static final String SEMICOLON = ";";
	public static final String PLUS = "+";
	public static final String SPACE = " ";
	public static final String SPACE_ENCODED = "%20";
	public static final String ENCODING = "UTF-8";
	
	protected static final String THEME_PREVIEW = "_theme_preview";
	protected static final String DRAFT_PREVIEW = "_draft_preview";
	protected static final String SMALL_PREVIEW_IMAGE = "small_theme_preview";
	
	protected static final String THEME_SEARCH_KEY = "*.htm*";
	
	protected static final String IDEGA_THEME_INFO = "_idega_theme.xml";
	
	private static final String[] _FILTER = new String[] {"htm", "html", "xhtml", "ibxml", "jsp"};
	protected static final List <String> FILTER = Collections.unmodifiableList(Arrays.asList(_FILTER));
	
	private static final String[] _PROPERTIES_FILES = new String[] {"Theme.plist", IDEGA_THEME_INFO};
	protected static final List <String> PROPERTIES_FILES = Collections.unmodifiableList(Arrays.asList(_PROPERTIES_FILES));
	
	private static final String[] _USELESS_CONTENT = new String[] {"%pathto(", ")%", "%"};
	protected static final List <String> USELESS_CONTENT = Collections.unmodifiableList(Arrays.asList(_USELESS_CONTENT));
	
	private static final String[] _REGIONS = new String[] {"%title%", "%header%", "%user_styles%", "%user_javascript%",
		"%logo%", "%site_title%", "%site_slogan%", "%content%", "%toolbar%", "%sidebar_title%", "%sidebar%", "%plugin_sidebar%",
		"%breadcrumb%", "%footer%"};
	protected static final List <String> REGIONS = Collections.unmodifiableList(Arrays.asList(_REGIONS));
	
	private static final String[] _BASIC_IDS_FOR_REGIONS = new String[] {"pageHeader", "contentContainer", "content",
		"sidebarContainer", "navcontainer", "sidebar", "footer", "breadcrumbcontainer"};
	protected static final List <String> BASIC_IDS_FOR_REGIONS = Collections.unmodifiableList(Arrays.asList(_BASIC_IDS_FOR_REGIONS));
	
	protected static final String COMMENT_BEGIN = "<!--";
	protected static final String COMMENT_END = "-->";
	protected static final String TEMPLATE_REGION_BEGIN = " TemplateBeginEditable name=\"";
	protected static final String TEMPLATE_REGION_MIDDLE = "\" ";
	protected static final String TEMPLATE_REGION_END = " TemplateEndEditable ";
	
	protected static final String DIV_TAG_INSTRUCTION = "//" + NAMESPACE_ID + ":div";
	
	// Strings used extracting theme properties
	protected static final String TAG_DICT = "dict";
	protected static final String TAG_ARRAY = "array";
	protected static final String TAG_NAME = "Name";
	protected static final String TAG_TYPE = "Type";
	protected static final String TAG_ENABLED = "Enabled";
	protected static final String TAG_FILES = "Files";
	protected static final String TAG_TRUE = "true";

	protected static final String RW_THEME_IMAGE = "RWThemeImage";
	protected static final String RW_THEME_NAME = "RWThemeName";
	protected static final String RW_STYLE_VARIATIONS = "RWStyleVariations";
	protected static final String RW_GROUP_NAME = "GroupName";
	protected static final String RW_SELECTION_LIMIT = "GroupSelectionLimit";
	protected static final String RW_GROUP_MEMBERS = "GroupMembers";
	
	protected static final String DRAFT = "_idega_draft.html";
	protected static final String THEME = "_idega_theme.html";
	
	protected static final String ELEMENT_LINK = "link";
	protected static final String TAG_ATTRIBUTE_HREF = "href";
	protected static final String TAG_ATTRIBUTE_SRC = "src";
	protected static final String TAG_ATTRIBUTE_ID = "id";
	protected static final String TAG_ATTRIBUTE_TYPE = "type";
	
	protected static final String THEME_SETTINGS = "/idegaweb/bundles/com.idega.content.bundle/resources/themes/theme.xml";
	protected static final String THEME_SETTING_CODE = "code";
	protected static final String THEME_SETTING_LABEL = "label";
	protected static final String THEME_SETTING_DEFAULT_VALUE = "defaultValue";
	protected static final String THEME_SETTING_TYPE = "type";
	protected static final String THEME_SETTING_METHOD = "method";
	
	protected static final String CON_THEME = "theme";
	protected static final String CON_NAME = "name";
	protected static final String CON_STYLES = "styles";
	protected static final String CON_STYLE = "style";
	protected static final String CON_GROUP = "group";
	protected static final String CON_VARIATION = "variation";
	protected static final String CON_PREVIEW = "preview";
	
	private static final String[] _DEFAULT_STYLE_FILES = new String[] {"styles.css", "handheld.css", "print.css"};
	protected static final List <String> DEFAULT_STYLE_FILES = Collections.unmodifiableList(Arrays.asList(_DEFAULT_STYLE_FILES));
	
}