package com.idega.content.business;

import com.idega.content.themes.helpers.business.ThemesConstants;

public class ContentConstants {

	public static final String IW_BUNDLE_IDENTIFIER = "com.idega.content";

	public static final String SLASH = "/";
	public static final String SPACE = " ";
	public static final String EMPTY = "";
	public static final String DOT = ".";
	public static final String UNDER = "_";
	public static final String MINUS_ONE = "-1";
	public static final String BRACKET_OPENING = "(";
	public static final String BRACKET_CLOSING = ")";

	public static final String BASE_ROOT_REPOSITORY = ContentUtil.getContentBaseFolderPath();

	public static final String PAGES_START_URI_WITHOUT_FIRST_SLASH = "pages";
	public static final String PAGES_START_URI = SLASH + PAGES_START_URI_WITHOUT_FIRST_SLASH;

	public static final String ARTICLE_VIEWER_URI = "/articleviewer/";

	public static final String ARTICLE_PATH_START = BASE_ROOT_REPOSITORY + "/article";
	public static final String ARTICLE_SCOPE = "article";

	public static final String COMMENT_SCOPE = "comment";
	public static final String COMMENT_PREFIX = DOT + COMMENT_SCOPE;

	public static final String XML_MIME_TYPE = "text/" + ThemesConstants.XML_EXTENSION;

	public static final String SITE_MAP_KEY = "siteMap";
	public static final String PAGES_MAP_KEY = "pageMap";

	public static final String PAGE_TYPES_CACHE_KEY = "IWPageTypes";

	public static final String UPLOAD_FIELD_NAME = "web2FileUploadField";
	public static final String UPLOADER_PATH = "web2FileUploaderPathValue";
	public static final String UPLOADER_UPLOAD_ZIP_FILE = "web2FileUploaderUploadZipFileValue";
	public static final String UPLOADER_UPLOAD_THEME_PACK = "web2FileUploaderUploadThemePackValue";
	public static final String UPLOADER_UPLOAD_EXTRACT_ARCHIVED_FILE = "web2FileUploaderExtractArchivedFileValue";
	public static final String UPLOADER_UPLOAD_IDENTIFIER = "web2FileUploaderUploadIdentifier";
	public static final String UPLOADER_STRIP_NON_ROMAN_LETTERS = "web2FileUploaderStripNonRomanLetters";
	public static final char[] UPLOADER_EXCEPTIONS_FOR_LETTERS = new char[] {'/', '\\', '-', '_', '.', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9'};

	public final static String ATTRIBUTE_HEADLINE = "headline";
	public final static String ATTRIBUTE_CREATION_DATE = "creation_date";
	public final static String ATTRIBUTE_BODY = "body";
	public final static String ATTRIBUTE_TEASER = "teaser";

	public static final String ARTICLE_ITEM_HEADLINE_STYLE_CLASS = "blog-entry-title";
	public static final String ARTICLE_ITEM_DATE_STYLE_CLASS = "blog-entry-date";
	public static final String ARTICLE_ITEM_BODY_STYLE_CLASS = "blog-entry-body";

	public static final String BOOLEAN_TYPE = "boolean";

	public static final String CONTENT_ITEM_ACTION_EDIT = "edit";
	public static final String CONTENT_ITEM_ACTION_DELETE = "delete";
	public static final String CONTENT_ITEM_ACTION_CREATE = "create";

	public static final String RENDERING_COMPONENT_OF_ARTICLE_LIST = "renderingComponentOfParentArticleList";

	public static final String ARTICLE_NOT_AVAILABLE_BODY = "The article you have chosen is not available in the selected language";

	public static final String DELETED_PAGE_IN_LUCID_PROPERTIES_FOR_ARTICLE = "deletedPageInLucidPropertiesForArticle";

	public static final String HIDE_MENU_IN_PAGE = "hidePageInMenu";
	public static final String PUBLISH_PAGE_IN_LUCID_CODE = "setPagePublished";
	public static final String SET_PAGE_LOCKED_IN_LUCID_CODE = "setPageLocked";
	public static final String PAGE_IS_CATEGORY_TYPE = "setCategoryPage";

	public static final String ARTICLE_CATEGORIES_VIEWER_BLOCK_CACHE_KEY = "articleCategoriesViewerBlockCacheKey";

	public static final String CONTENT_ITEM_VIEWER_IDENTIFIER_PARAMETER = "contentItemViewerIdentifier";

	public static final String FACET_NAME_FOR_CONTENT_ITEMS_LIST_BACK_BUTTON = "contentItemsListBackButton";

	public static final String BASIC_THEMES_ADDED_PROPERTY = "basicThemesAdded";

	public static final String CONTENT_ITEM_IDENTIFIER_NAME = "contentItemIdentifierName";
	public static final String CONTENT_ITEM_IDENTIFIER_STYLE_CLASS = "contentItemIdentifierStyleClass";
	public static final String CONTENT_LIST_ITEMS_IDENTIFIER_NAME = "contentListItemsIdentifierName";
	public static final String CONTENT_LIST_ITEMS_IDENTIFIER = "contentListItemsIdentifier";

	public static final String MAX_UPLOAD_SIZE = "max_content_upload_size";

}