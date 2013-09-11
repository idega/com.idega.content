package com.idega.content.themes.helpers.business;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import com.idega.builder.bean.AdvancedProperty;
import com.idega.content.business.ContentConstants;
import com.idega.content.business.ContentUtil;
import com.idega.util.CoreConstants;

public class ThemesConstants {
	
	private static boolean INITIALIZED;
	
	public static final String IDEGA_THEME = "idega_theme";
	
	public static final String TOOLBAR = "toolbar";
	public static final String BREADCRUMB = "breadcrumb";
	public static final String PLUGIN_SIDEBAR = "plugin_sidebar";
	public static final String LOGO = "logo";
	
	public static final String XML_EXTENSION = "xml";
	
	private static String CONTENT_BUNDLE_RESOURCES = null;
	
	public static String BASE_THEME_IMAGES = null;
	
	public static final String PLUS = "+";
	public static final String SPACE_ENCODED = "%20";
	public static final String SINGLE_QUOTE = "\"";
	
	public static final String THEME_PREVIEW = "_theme_preview";
	public static final String THEME_SMALL_PREVIEW = "_small" + THEME_PREVIEW;
	public static final String DRAFT_PREVIEW = "_draft_preview";
	
	public static final String THEME_SEARCH_KEY = "*.htm*";
	public static final String THEME_PROPERTIES_FILE_END = ".plist";
	public static final String THEME_PROPERTIES_FILE = "Theme" + THEME_PROPERTIES_FILE_END;
	public static final String IDEGA_THEME_INFO = CoreConstants.UNDER + IDEGA_THEME + CoreConstants.DOT + XML_EXTENSION;
	
	public static final String ARTICLE_TITLE = "Article";
	
	public static final String USELESS_PATHTO_ELEMENT = "%pathto(";
	public static final String USELESS_DATA_ELEMENT = "![CDATA[";
	
	private static final String[] _REGIONS_NEEDED_TO_CREATE = new String[] {PLUGIN_SIDEBAR};
	public static final List <String> REGIONS_NEEDED_TO_CREATE = Collections.unmodifiableList(Arrays.asList(_REGIONS_NEEDED_TO_CREATE));
	
	private static final String[] _FIRST_SENTENCES_OF_DUMMY_ARTICLES = new String[] {"Lorem ipsum dolor sit amet, consectetuer adipiscing elit."};
	public static final List<String> FIRST_SENTENCES_OF_DUMMY_ARTICLES = Collections.unmodifiableList(Arrays.asList(_FIRST_SENTENCES_OF_DUMMY_ARTICLES));
	
	private static final String[] _DUMMY_ARTICLES = new String[] { "<p>" + _FIRST_SENTENCES_OF_DUMMY_ARTICLES[0] + " Vestibulum bibendum, ligula ut feugiat rutrum, mauris libero ultricies nulla, at hendrerit lectus dui bibendum metus. Phasellus quis nulla nec mauris sollicitudin ornare. Vivamus faucibus. Class aptent taciti sociosqu ad litora torquent per conubia nostra, per inceptos hymenaeos. Cras vulputate condimentum ipsum.</p><p>Duis urna eros, commodo id, sagittis sed, sodales eu, ante. Etiam ante. Cras risus dolor, porta nec, adipiscing eu, scelerisque at, metus. Mauris nunc eros, porttitor nec, tincidunt ut, rutrum eget, massa. In facilisis nisi. Sed non lorem malesuada quam egestas bibendum. Quisque bibendum ullamcorper purus. Integer id diam vel elit adipiscing consectetuer. Phasellus vitae eros. Vivamus laoreet consectetuer tortor. In congue dignissim quam. Suspendisse nec purus vel velit ultricies bibendum.</p>"};
	public static final List <String> DUMMY_ARTICLES = Collections.unmodifiableList(Arrays.asList(_DUMMY_ARTICLES));
	
	private static final String[] _THEME_IMAGES = new String[] {"eagle.jpg", "grapes.jpg", "rocks.jpg", "ship.jpg", "sky.jpg"};
	public static final List <String> THEME_IMAGES = Collections.unmodifiableList(Arrays.asList(_THEME_IMAGES));
	
	private static final String[] _IMAGE_POSITIONS = new String[] {"left", "right"};
	public static final List <String> IMAGE_POSITIONS = Collections.unmodifiableList(Arrays.asList(_IMAGE_POSITIONS));
	
	private static final String[] _THEME_SKELETONS_FILTER = new String[] {"htm", "html", "xhtml", "ibxml", "jsp"};
	public static final List <String> THEME_SKELETONS_FILTER = Collections.unmodifiableList(Arrays.asList(_THEME_SKELETONS_FILTER));
	
	private static final String[] _THEME_PROPERTIES_FILES = new String[] {THEME_PROPERTIES_FILE, IDEGA_THEME_INFO, "theme" + THEME_PROPERTIES_FILE_END};
	public static final List <String> THEME_PROPERTIES_FILES = Collections.unmodifiableList(Arrays.asList(_THEME_PROPERTIES_FILES));
	
	private static final String[] _USELESS_CONTENT = new String[] {")%", " xml:space=\"preserve\"", "&lt;" + USELESS_DATA_ELEMENT, "]]&amp;amp;", " xmlns=\"\"",
		" profile=\"\""};
	public static final List <String> USELESS_CONTENT = Collections.unmodifiableList(Arrays.asList(_USELESS_CONTENT));
	
	private static final String[] _REGIONS = new String[] {"%logo%", "%content%", "%toolbar%", "%sidebar_title%", "%sidebar%", "%plugin_sidebar%", "%breadcrumb%",
		"%footer%", "%site_title%", "%site_slogan%"};
	public static final List <String> REGIONS = Collections.unmodifiableList(Arrays.asList(_REGIONS));
	
	private static final String[] _BASIC_IDS_FOR_REGIONS = new String[] {"pageHeader", "contentContainer", "sidebarContainer", "sideContent"};
	public static final List <String> BASIC_IDS_FOR_REGIONS = Collections.unmodifiableList(Arrays.asList(_BASIC_IDS_FOR_REGIONS));
	
	private static final String[] _DOCUMENT_PUBLIC_IDS = new String[] {"Apple Computer", "DTD XHTML 1.0 Strict"};
	public static final List <String> DOCUMENT_PUBLIC_IDS = Collections.unmodifiableList(Arrays.asList(_DOCUMENT_PUBLIC_IDS));
	
	private static final String[] _DOCUMENT_SYSTEM_IDS = new String[] {"PropertyList", "xhtml1-strict.dtd"};
	public static final List <String> DOCUMENT_SYSTEM_IDS = Collections.unmodifiableList(Arrays.asList(_DOCUMENT_SYSTEM_IDS));

	private static final String[] _DEFAULT_STYLE_FILES = new String[] {"styles.css", "handheld.css", "print.css"};
	public static final List <String> DEFAULT_STYLE_FILES = Collections.unmodifiableList(Arrays.asList(_DEFAULT_STYLE_FILES));
	
	public static final String COMMENT_BEGIN = "<!--";
	public static final String COMMENT_END = "-->";
	public static final String TEMPLATE_REGION_BEGIN = " TemplateBeginEditable name=\"";
	public static final String TEMPLATE_REGION_MIDDLE = "\" ";
	public static final String TEMPLATE_REGION_END = " TemplateEndEditable ";
	
	public static final String SITE_TITLE = "site_title";
	public static final String SITE_SLOGAN = "site_slogan";
	
	public static final String DIV_TAG_INSTRUCTION = "div";
	public static final String OBJECT_TAG_INSTRUCTION = "object";
	public static final String PARAM_TAG_INSTRUCTION = "param";
	public static final String LINK_TAG_INSTRUCTION = "link";
	public static final String EMBED_TAG_INSTRUCTION = "embed";
	
	// Strings used extracting theme properties
	public static final String TAG_DICT = "dict";
	public static final String TAG_ARRAY = "array";
	public static final String TAG_STRING = "string";
	public static final String TAG_NAME = "Name";
	public static final String TAG_DEFAULT_COLOUR = "DefaultColour";
	public static final String TAG_TAG = "Tag";
	public static final String TAG_TYPE = "Type";
	public static final String TAG_ENABLED = "Enabled";
	public static final String TAG_FILES = "Files";
	public static final String TAG_TRUE = "true";

	public static final String RW_THEME_IMAGE = "RWThemeImage";
	public static final String RW_THEME_NAME = "RWThemeName";
	public static final String RW_STYLE_VARIATIONS = "RWStyleVariations";
	public static final String RW_COLOUR_TAG_FILES = "RWColourTagCSSFiles";
	public static final String RW_GROUP_NAME = "GroupName";
	public static final String RW_SELECTION_LIMIT = "GroupSelectionLimit";
	public static final String RW_GROUP_MEMBERS = "GroupMembers";
	
	public static final String RW_STYLE_VARIATION_TYPE_COLOUR = "Colour";
	
	public static final String DRAFT = "_idega_draft.html";
	public static final String THEME = "_" + IDEGA_THEME + ".html";
	
	public static final String ELEMENT_LINK = "link";
	public static final String TAG_ATTRIBUTE_HREF = "href";
	public static final String TAG_ATTRIBUTE_SRC = "src";
	public static final String TAG_ATTRIBUTE_ID = "id";
	public static final String TAG_ATTRIBUTE_TYPE = "type";
	
	public static final String SETTING_CODE = "code";
	public static final String SETTING_LABEL = "label";
	public static final String SETTING_DEFAULT_VALUE = "defaultValue";
	public static final String SETTING_TYPE = "type";
	public static final String SETTING_METHOD = "method";
	
	public static final String CON_THEME = "theme";
	public static final String CON_NAME = "name";
	public static final String CON_STYLES = "styles";
	public static final String CON_STYLE = "style";
	public static final String CON_GROUP = "group";
	public static final String CON_VARIATION = "variation";
	public static final String CON_VALUE = "value";
	public static final String CON_COLOR = "color";
	public static final String CON_VARIABLE = "variable";
	public static final String CON_PREVIEW = "preview";
	public static final String CON_SMALL_PREVIEW = "small-preview";
	public static final String CON_PAGE_ID = "page-id";
	public static final String CON_EXTRA_REGIONS = "extra-regions";
	public static final String CON_EXTRA_REGION = "extra-region";
	public static final String CON_ATT_EXTRA_REGION_PARENT = "extra-region-parent";
	public static final String CON_URI_OF_CURRENT_BUILT_IN_STYLE = "uri-of-current-built-in-style";
	public static final String CON_COLOUR_FILES = "colour-files";
	public static final String CON_COLOUR_FILES_ORIGINAL = "colour-files-original";
	public static final String CON_COLOUR_FILE = "colour-file";
	
	public static final int SMALL_PREVIEW_WIDTH = 149;
	public static final int SMALL_PREVIEW_HEIGHT = 112;
	
	public static final int PREVIEW_WIDTH = 1024;
	public static final int PREVIEW_HEIGHT = 768;
	
	public static final int REDUCED_PREVIEW_WIDTH = 580;
	public static final int REDUCED_PREVIEW_HEIGHT = 435;
	
	public static final String DEFAULT_MIME_TYPE = "image/";
	
	public static final String THEMES = "/themes/";
	
	public static final String THEMES_PROPERTY_START = "theme.";
	public static final String THEMES_PROPERTY_END = ".def";
	
	public static final String THEMES_PATH = ContentConstants.BASE_ROOT_REPOSITORY + THEMES;
	public static final String PAGES_PATH_REPOSITORY = ContentConstants.BASE_ROOT_REPOSITORY + ContentConstants.PAGES_START_URI + ContentConstants.SLASH;
	public static final String THEMES_PREVIEW_PATH = THEMES_PATH + "preview/";
	
	public static final String AT = "@";
	public static final String EMPTY = CoreConstants.EMPTY;
	public static final String SEMICOLON = CoreConstants.SEMICOLON;
	public static final String COMMA = CoreConstants.COMMA;
	
	public static String PAGE_SETTINGS = null;
	
	public static final String LAST_USED_THEME = "theme.last_used.ibpage_id";
	
	public static final String SYSTEM_SETTINGS = "mainDomain";
	public static final String DOMAIN_NAME = SYSTEM_SETTINGS + "Name";
	public static final String DOMAIN_SERVER_NAME = SYSTEM_SETTINGS + "ServerName";
	
	public static final String MINUS_ONE = ContentConstants.MINUS_ONE;
	
	public static final String DEFAULT_DOMAIN_NAME = "Default Site";
	
	public static final String DOCUMENT_HEADER = "<?xml version='1.0' encoding='"+CoreConstants.ENCODING_UTF8+"'?>";
	
	public static final String ADD_FOR_PROPERTY_CHANGE = "_changeProperty";
	
	public static final String NEW_LINE = "\n";
	
	public static final String THEME_STYLE_VARIATIONS_CACHE_KEY = "theme_style_variations_block";
	
	public static final String THEME_ID_APPLICATION_ATTRIBUTE = "theme_id_application_attribute";

	public static final String THEME_PREDEFINED_STYLE_CONFIG_FILE = ".rwstyle";
	public static final String DEFAULT_THEME_STYLE_ID = "defaultThemeStyleId";
	
	//	Add more themes here
	public static final List<AdvancedProperty> DEFAULT_THEMES = Collections.unmodifiableList(Arrays.asList(
			new AdvancedProperty("eGov.rwtheme", "http://github.com/idega/eGov.rwtheme/raw/master/src/eGov.rwtheme.zip")
	));
	
	private static String getContentBundleResourcesUri() {
		if (CONTENT_BUNDLE_RESOURCES == null) {
			CONTENT_BUNDLE_RESOURCES = ContentUtil.getBundle().getResourcesPath();
		}
		return CONTENT_BUNDLE_RESOURCES;
	}
	
	public static void initializeThemeConstants() {
		if (INITIALIZED) {
			return;
		}
		
		BASE_THEME_IMAGES = getContentBundleResourcesUri() + "/images/themes/";
		PAGE_SETTINGS = getContentBundleResourcesUri() + "/themes/page.xml";
		
		INITIALIZED = true;
	}
}
