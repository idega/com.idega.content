package com.idega.content.themes.helpers;

public class ThemesConstants {
	
	public static final String NAMESPACE = "http://www.w3.org/1999/xhtml";
	public static final String NAMESPACE_ID = "xmlns";
	
	public static final String CONTENT = "/content"; 
	public static final String THEMES_PATH = "/files/cms/themes/";
	public static final String THEMES_PREVIEW_PATH = THEMES_PATH + "preview/";
	
	public static final String UNDEFINED_FILE_NAME = "undefined_";
	public static final String DOT = ".";
	public static final String SLASH = "/";
	public static final String AT = "@";
	public static final String EMPTY = "";
	
	public static final String PREVIEW_IMAGE = "theme_preview";
	public static final String PREVIEW_GENERATED = "2";
	public static final String ERROR_OCURRED = "1";
	public static final String NO_ACTION_NEEDED = "0";
	
	public static final String PNG_IMG_FILE_EXTENSION = ".png";
	public static final String JPG_IMG_FILE_EXTENSION = ".jpeg";
	public static final String THEME_SEARCH_KEY = "*.htm*";
	
	public static final String[] FILTER = new String[] {"htm", "html", "xhtml", "ibxml", "jsp"};
	public static final String[] PROPERTIES_FILES = new String[] {"Theme.plist"};
	public static final String[] USELESS_CONTENT = new String[] {"%pathto(", ")%", "%"/*, "xml:space=\"preserve\"", "xmlns=\"\""*/};
	public static final String[] REGIONS = new String[] {"%header%", "%user_styles%", "%user_javascript%",
		"%logo%", "%site_title%", "%site_slogan%", "%content%", "%toolbar%", "%sidebar_title%", "%sidebar%", "%plugin_sidebar%",
		"%breadcrumb%", "%footer%"};
	public static final String[] BASICS_IDS_FOR_REGIONS = new String[] {"pageHeader", "contentContainer", "content",
		"sidebarContainer", "navcontainer", "sidebar", "footer", "breadcrumbcontainer"};
	
	public static final String PERCENTS = "%";
	public static final String COMMENT_BEGIN = "<!--";
	public static final String COMMENT_END = "-->";
	public static final String TEMPLATE_REGION_BEGIN = " TemplateBeginEditable name=\"";
	public static final String TEMPLATE_REGION_MIDDLE = "\" ";
	public static final String TEMPLATE_REGION_END = " TemplateEndEditable ";
	
	public static final String DIV_TAG_INSTRUCTION = "//" + NAMESPACE_ID + ":div";
	
	// Strings used extracting theme properties
	public static final String TAG_DICT = "dict";
	public static final String TAG_ARRAY = "array";
	public static final String TAG_NAME = "Name";
	public static final String TAG_TYPE = "Type";
	public static final String TAG_ENABLED = "Enabled";
	public static final String TAG_FILES = "Files";
	public static final String TAG_TRUE = "true";

	public static final String RW_THEME_IMAGE = "RWThemeImage";
	public static final String RW_THEME_NAME = "RWThemeName";
	public static final String RW_STYLE_VARIATIONS = "RWStyleVariations";
	public static final String RW_GROUP_NAME = "GroupName";
	public static final String RW_GROUP_MEMBERS = "GroupMembers";
	
	public static final String DRAFT = ".draft";
	
	public static final String ELEMENT_LINK = "link";
	public static final String TAG_ATTRIBUTE_HREF = "href";
	public static final String TAG_ATTRIBUTE_SRC = "src";
	public static final String TAG_ATTRIBUTE_ID = "id";
	public static final String TAG_ATTRIBUTE_TYPE = "type";
}
