package com.idega.content.business;

import com.idega.content.themes.helpers.ThemesConstants;

public class ContentConstants {

	public static final String SLASH = "/";
	public static final String SPACE = " ";
	public static final String EMPTY = "";
	public static final String DOT = ".";
	public static final String UNDER = "_";
	public static final String BRACKET_OPENING = "(";
	public static final String BRACKET_CLOSING = ")";
	
	public static final String BASE_ROOT_SLIDE = ContentUtil.getContentBaseFolderPath();
	
	public static final String PAGES_START_URI_WITHOUT_FIRST_SLASH = "pages";
	public static final String PAGES_START_URI = SLASH + PAGES_START_URI_WITHOUT_FIRST_SLASH;
	
	public static final String ARTICLE_VIEWER_URI = "/articleviewer/";
	
	public static final String CONTENT = "/content";
	
	public static final String ARTICLE_PATH_START = BASE_ROOT_SLIDE + "/article";
	public static final String ARTICLE_SCOPE = "article";

	//public static final String CONTENT_ITEM_COMMENTS_URI = RSSBusinessBean.RSS_FOLDER_URI + "comments";
	public static final String COMMENT_SCOPE = "comment";
	public static final String COMMENT_PREFIX = DOT + COMMENT_SCOPE;
	
	public static final String XML_MIME_TYPE = "text/" + ThemesConstants.XML_EXTENSION;
}
