package com.idega.content.themes.helpers;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import com.idega.content.business.ContentConstants;
import com.idega.content.business.ContentUtil;
import com.idega.util.CoreConstants;

public class ThemesConstants {
	
	public static final String IDEGA_THEME = "idega_theme";
	
	public static final String NAVIGATION = "navcontainer";
	public static final String TOOLBAR = "toolbar";
	public static final String BREADCRUMB = "breadcrumbcontainer";
	public static final String PLUGIN_SIDEBAR = "plugin_sidebar";
	
	public static final String XML_EXTENSION = "xml";
	
	private static final String BASE_ROOT_APPL = ContentUtil.getBundle().getResourcesPath();
	
	protected static final String BASE_THEME_IMAGES = BASE_ROOT_APPL + "/images/themes/";
	
	protected static final String NAMESPACE = "http://www.w3.org/1999/xhtml";
	protected static final String NAMESPACE_ID = "xmlns";
	
	protected static final String PLUS = "+";
	protected static final String SPACE_ENCODED = "%20";
	protected static final String SINGLE_QUOTE = "\"";
	
	protected static final String THEME_PREVIEW = "_theme_preview";
	protected static final String THEME_SMALL_PREVIEW = "_small" + THEME_PREVIEW;
	protected static final String DRAFT_PREVIEW = "_draft_preview";
	
	protected static final String THEME_SEARCH_KEY = "*.htm*";
	public static final String THEME_PROPERTIES_FILE_END = ".plist";
	protected static final String THEME_PROPERTIES_FILE = "Theme" + THEME_PROPERTIES_FILE_END;
	public static final String IDEGA_THEME_INFO = CoreConstants.UNDER + IDEGA_THEME + CoreConstants.DOT + XML_EXTENSION;
	
	public static final String ARTICLE_TITLE = "Article";
	
	protected static final String USELESS_PATHTO_ELEMENT = "%pathto(";
	
	private static final String[] _REGIONS_NEEDED_TO_CREATE = new String[] {TOOLBAR, PLUGIN_SIDEBAR};
	protected static final List <String> REGIONS_NEEDED_TO_CREATE = Collections.unmodifiableList(Arrays.asList(_REGIONS_NEEDED_TO_CREATE));
	
	private static final String[] _DUMMY_ARTICLES = new String[] {"Lorem ipsum dolor sit amet, consectetuer adipiscing elit. Vestibulum bibendum, ligula ut feugiat rutrum, mauris libero ultricies nulla, at hendrerit lectus dui bibendum metus. Phasellus quis nulla nec mauris sollicitudin ornare. Vivamus faucibus. Class aptent taciti sociosqu ad litora torquent per conubia nostra, per inceptos hymenaeos. Cras vulputate condimentum ipsum. Duis urna eros, commodo id, sagittis sed, sodales eu, ante. Etiam ante. Cras risus dolor, porta nec, adipiscing eu, scelerisque at, metus. Mauris nunc eros, porttitor nec, tincidunt ut, rutrum eget, massa. In facilisis nisi. Sed non lorem malesuada quam egestas bibendum. Quisque bibendum ullamcorper purus. Integer id diam vel elit adipiscing consectetuer. Phasellus vitae eros. Vivamus laoreet consectetuer tortor. In congue dignissim quam. Suspendisse nec purus vel velit ultricies bibendum."};
	public static final List <String> DUMMY_ARTICLES = Collections.unmodifiableList(Arrays.asList(_DUMMY_ARTICLES));
	
	private static final String[] _THEME_IMAGES = new String[] {"eagle.jpg", "grapes.jpg", "rocks.jpg", "ship.jpg", "sky.jpg"};
	protected static final List <String> THEME_IMAGES = Collections.unmodifiableList(Arrays.asList(_THEME_IMAGES));
	
	private static final String[] _IMAGE_POSITIONS = new String[] {"left", "right"};
	protected static final List <String> IMAGE_POSITIONS = Collections.unmodifiableList(Arrays.asList(_IMAGE_POSITIONS));
	
	private static final String[] _THEME_SKELETONS_FILTER = new String[] {"htm", "html", "xhtml", "ibxml", "jsp"};
	public static final List <String> THEME_SKELETONS_FILTER = Collections.unmodifiableList(Arrays.asList(_THEME_SKELETONS_FILTER));
	
	private static final String[] _THEME_PROPERTIES_FILES = new String[] {THEME_PROPERTIES_FILE, IDEGA_THEME_INFO, "theme" + THEME_PROPERTIES_FILE_END};
	protected static final List <String> THEME_PROPERTIES_FILES = Collections.unmodifiableList(Arrays.asList(_THEME_PROPERTIES_FILES));
	
	private static final String[] _USELESS_CONTENT = new String[] {USELESS_PATHTO_ELEMENT, ")%", "%", " xml:space=\"preserve\""};
	protected static final List <String> USELESS_CONTENT = Collections.unmodifiableList(Arrays.asList(_USELESS_CONTENT));
	
	private static final String[] _REGIONS = new String[] {"%logo%", "%content%", "%toolbar%", "%sidebar_title%", "%sidebar%",
		"%plugin_sidebar%", "%breadcrumb%", "%footer%", "%site_title%", "%site_slogan%"};
	protected static final List <String> REGIONS = Collections.unmodifiableList(Arrays.asList(_REGIONS));
	
	private static final String[] _BASIC_IDS_FOR_REGIONS = new String[] {"pageHeader", "contentContainer", "sidebarContainer",
		BREADCRUMB, NAVIGATION};
	protected static final List <String> BASIC_IDS_FOR_REGIONS = Collections.unmodifiableList(Arrays.asList(_BASIC_IDS_FOR_REGIONS));
	
	private static final String[] _DOCUMENT_PUBLIC_IDS = new String[] {"Apple Computer", "DTD XHTML 1.0 Strict"};
	protected static final List <String> DOCUMENT_PUBLIC_IDS = Collections.unmodifiableList(Arrays.asList(_DOCUMENT_PUBLIC_IDS));
	
	private static final String[] _DOCUMENT_SYSTEM_IDS = new String[] {"PropertyList", "xhtml1-strict.dtd"};
	protected static final List <String> DOCUMENT_SYSTEM_IDS = Collections.unmodifiableList(Arrays.asList(_DOCUMENT_SYSTEM_IDS));

	private static final String[] _DEFAULT_STYLE_FILES = new String[] {"styles.css", "handheld.css", "print.css"};
	protected static final List <String> DEFAULT_STYLE_FILES = Collections.unmodifiableList(Arrays.asList(_DEFAULT_STYLE_FILES));
	
	protected static final String COMMENT_BEGIN = "<!--";
	protected static final String COMMENT_END = "-->";
	protected static final String TEMPLATE_REGION_BEGIN = " TemplateBeginEditable name=\"";
	protected static final String TEMPLATE_REGION_MIDDLE = "\" ";
	protected static final String TEMPLATE_REGION_END = " TemplateEndEditable ";
	
	public static final String SITE_TITLE = "site_title";
	public static final String SITE_SLOGAN = "site_slogan";
	
	protected static final String DIV_TAG_INSTRUCTION = "//" + NAMESPACE_ID + ":div";
	protected static final String OBJECT_TAG_INSTRUCTION = "//" + NAMESPACE_ID + ":object";
	protected static final String PARAM_TAG_INSTRUCTION = "//" + NAMESPACE_ID + ":param";
	
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
	protected static final String THEME = "_" + IDEGA_THEME + ".html";
	
	protected static final String ELEMENT_LINK = "link";
	protected static final String TAG_ATTRIBUTE_HREF = "href";
	protected static final String TAG_ATTRIBUTE_SRC = "src";
	protected static final String TAG_ATTRIBUTE_ID = "id";
	protected static final String TAG_ATTRIBUTE_TYPE = "type";
	
	protected static final String SETTING_CODE = "code";
	protected static final String SETTING_LABEL = "label";
	protected static final String SETTING_DEFAULT_VALUE = "defaultValue";
	protected static final String SETTING_TYPE = "type";
	protected static final String SETTING_METHOD = "method";
	
	protected static final String CON_THEME = "theme";
	protected static final String CON_NAME = "name";
	protected static final String CON_STYLES = "styles";
	protected static final String CON_STYLE = "style";
	protected static final String CON_GROUP = "group";
	protected static final String CON_VARIATION = "variation";
	protected static final String CON_PREVIEW = "preview";
	protected static final String CON_SMALL_PREVIEW = "small-preview";
	protected static final String CON_PAGE_ID = "page-id";
	
	protected static final int SMALL_PREVIEW_WIDTH = 149;
	protected static final int SMALL_PREVIEW_HEIGHT = 112;
	
	protected static final int PREVIEW_WIDTH = 800;
	protected static final int PREVIEW_HEIGHT = 600;
	
	protected static final int REDUCED_PREVIEW_WIDTH = 580;
	protected static final int REDUCED_PREVIEW_HEIGHT = 435;
	
	protected static final String DEFAULT_MIME_TYPE = "image/";
	
	protected static final String PAGES_PATH_APPL = BASE_ROOT_APPL + "/pages/";
	protected static final String ARTICLE_PATH_APPL = BASE_ROOT_APPL + "/themes/" + IDEGA_THEME + "_article." + XML_EXTENSION;
	
	public static final String THEMES = "/themes/";
	
	public static final String THEMES_PROPERTY_START = "theme.";
	public static final String THEMES_PROPERTY_END = ".def";
	
	public static final String THEMES_PATH = ContentConstants.BASE_ROOT_SLIDE + THEMES;
	public static final String PAGES_PATH_SLIDE = ContentConstants.BASE_ROOT_SLIDE + ContentConstants.PAGES_START_URI + ContentConstants.SLASH;
	public static final String THEMES_PREVIEW_PATH = THEMES_PATH + "preview/";
	
	public static final String AT = "@";
	public static final String EMPTY = "";
	public static final String SEMICOLON = ";";
	public static final String COMMA = ",";
	
	public static final String THEME_SETTINGS = "resources/themes/theme.xml";
	public static final String PAGE_SETTINGS = BASE_ROOT_APPL + "/themes/page.xml";
	
	public static final String LAST_USED_THEME = "theme.last_used.ibpage_id";
	
	public static final String SYSTEM_SETTINGS = "mainDomain";
	public static final String DOMAIN_NAME = SYSTEM_SETTINGS + "Name";
	public static final String DOMAIN_SERVER_NAME = SYSTEM_SETTINGS + "ServerName";
	
	public static final String MINUS_ONE = "-1";
	
	public static final String DEFAULT_DOMAIN_NAME = "Default Site";
	
	protected static final String DOCUMENT_HEADER = "<?xml version='1.0' encoding='"+CoreConstants.ENCODING_UTF8+"'?>";
	
	public static final String ADD_FOR_PROPERTY_CHANGE = "_changeProperty";
	
	protected static final String NEW_LINE = "\n";
	
	public static final String THEME_STYLE_VARIATIONS_CACHE_KEY = "theme_style_variations_block";
	
	public static final String THEME_ID_APPLICATION_ATTRIBUTE = "theme_id_application_attribute";

	protected static final Boolean IS_THEME_PREVIEW_JPG = Boolean.TRUE;
	protected static final Float THEME_PREVIEW_QUALITY = new Float(1f);
}
