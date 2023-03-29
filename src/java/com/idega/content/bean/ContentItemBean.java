/*
 * $Id: ContentItemBean.java,v 1.50 2009/06/22 14:17:17 valdas Exp $
 *
 * Copyright (C) 2004-2005 Idega. All Rights Reserved.
 *
 * This software is the proprietary information of Idega.
 * Use is subject to license terms.
 *
 */
package com.idega.content.bean;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.Serializable;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.faces.context.FacesContext;
import javax.jcr.RepositoryException;

import com.idega.block.rss.business.EntryData;
import com.idega.content.business.ContentConstants;
import com.idega.content.business.ContentItemHelper;
import com.idega.content.themes.helpers.business.ThemesConstants;
import com.idega.content.themes.helpers.business.ThemesHelper;
import com.idega.core.accesscontrol.business.StandardRoles;
import com.idega.idegaweb.IWUserContext;
import com.idega.presentation.IWContext;
import com.idega.repository.RepositoryService;
import com.idega.repository.bean.RepositoryItem;
import com.idega.repository.bean.RepositoryItemVersionInfo;
import com.idega.util.CoreConstants;
import com.idega.util.CoreUtil;
import com.idega.util.IWTimestamp;
import com.idega.util.ListUtil;
import com.idega.util.StringUtil;
import com.idega.util.expression.ELUtil;
import com.sun.syndication.io.impl.DateParser;

/**
 * <p>
 * Base bean for "content items", i.e. resources that can be read from the repository store and displayed as content.
 * </p>
 *  Last modified: $Date: 2009/06/22 14:17:17 $ by $Author: valdas $
 *
 * @author Anders Lindman,<a href="mailto:tryggvil@idega.com">tryggvil</a>
 * @version $Revision: 1.50 $
 */
public abstract class ContentItemBean implements Serializable, ContentItem {

	private static final long serialVersionUID = -1620171698501618358L;

    public static final String DISPLAYNAME = "displayname";
    public static final String GETCONTENTLANGUAGE = "getcontentlanguage";
    public static final String GETCONTENTLENGTH = "getcontentlength";
    public static final String GETLASTMODIFIED = "getlastmodified";
    public static final String CREATIONDATE = "creationdate";
    public static final String RESOURCETYPE = "resourcetype";
    public static final String SOURCE = "source";
    public static final String GETCONTENTTYPE = "getcontenttype";
    public static final String GETETAG = "getetag";
    public static final String ISHIDDEN = "ishidden";
    public static final String ISCOLLECTION = "iscollection";
    public static final String SUPPORTEDLOCK = "supportedlock";
    public static final String LOCKDISCOVERY = "lockdiscovery";

	private Locale _locale = null;
	private String _name = null;
	private String _description = null;
	private String _itemType = null;
	private int _createdByUserId = -1;
	private boolean autoCreateResource;
	private boolean loaded;

	private String _pendingLocaleId = null;
	private String _requestedStatus = null;

	private ContentItemCase _caseBean = null;

	private Map<String, List<ContentItemField>> _itemFields = null;
	private Map<String, Locale> _locales = null;

	public final static String FIELDNAME_ATTACHMENT = "attachment";
	public final static String FIELDNAME_CREATION_DATE = "creation_date";
	public final static String FIELDNAME_RESOURCE_PATH = "resource_path";
	public final static String FIELDNAME_VERSION_NAME = "version_name";
	public final static String FIELDNAME_STATUS = "status";
	public final static String FIELDNAME_LAST_MODIFIED_DATE = "lastmodified";
	public static final String FIELDNAME_PUBLISHED_DATE = "published_date";
	public final static String FIELDNAME_BODY = "body";

	private Boolean doRender = Boolean.TRUE;
	private boolean exists=false;

	private final static String[] ACTION_EXISTS_ARRAY = new String[] {ContentConstants.CONTENT_ITEM_ACTION_DELETE, ContentConstants.CONTENT_ITEM_ACTION_EDIT};
	private final static String[] ACTION_NOT_EXISTS_ARRAY = new String[] {ContentConstants.CONTENT_ITEM_ACTION_CREATE};

	private List<RepositoryItemVersionInfo> versions;

	private String categories = null; // This string is parsed from WebDavResource

	private boolean setPublishedDateByDefault = false;

	public ContentItemBean() {}

	@Override
	public Locale getLocale() {
		if(this._locale==null){
			IWContext iwc = IWContext.getIWContext(FacesContext.getCurrentInstance());
			return iwc.getLocale();
		}
		return this._locale;
	}

	public String getName() { return this._name; }
	public String getDescription() { return this._description; }
	public String getItemType() { return this._itemType; }
	public int getCreatedByUserId() { return this._createdByUserId; }

	public void setName(String s) { this._name = s; }
	public void setDescription(String s) { this._description = s; }
	public void setItemType(String s) { this._itemType = s; }
	public void setCreatedByUserId(int id) { this._createdByUserId = id; }

	public void setLocale(Locale locale) {
		this._locale = locale;
		if (this._locales == null) {
			this._locales = new HashMap<>();
		}
		this._locales.put(locale.getLanguage(), locale);
	}

	public Map<String, Locale> getLocales() { return this._locales; }

	public String getPendingLocaleId() { return this._pendingLocaleId != null ? this._pendingLocaleId : this._locale.getLanguage(); }
	public void setPendingLocaleId(String localeId) { this._pendingLocaleId = localeId; }

	public String getRequestedStatus() { return this._requestedStatus; }
	public void setRequestedStatus(String requestedStatus) { this._requestedStatus = requestedStatus; }

	/**
	 * Clears all attributes for this bean.
	 */
	public void clear() {
		this._name = null;
		this._description = null;
		this._itemType = null;
		this._createdByUserId = -1;

		this._pendingLocaleId = null;

		this._caseBean = null;

		this._itemFields = null;
		this.versions=null;

		setStatus(ContentItemCase.STATUS_NEW);
	}

	/**
	 * Returns the item field with the specified key.
	 */
	public ContentItemField getItemField(String key) {
		if (this._itemFields == null) {
			this._itemFields = new HashMap<>();
		}
		List<ContentItemField> fields = this._itemFields.get(key + getLanguage());
		return ListUtil.isEmpty(fields) ? null : fields.get(0);
	}

	public List<ContentItemField> getAttachments() { return getItemFields(FIELDNAME_ATTACHMENT); }
	public void setAttachment(List<ContentItemField> l) { setItemFields(FIELDNAME_ATTACHMENT, l); }

	@Override
	public Object getValue(String fieldName){
		ContentItemField field = getItemField(fieldName);
		if(field != null){
			return field.getValue();
		} else {
			return null;
		}
	}

	@Override
	public void setValue(String fieldName, Object value){
		setItemFieldValue(fieldName, value);
	}

	@Override
	public abstract String[] getContentFieldNames();

	@Override
	public String[] getToolbarActions(){
		if(getExists()){
			return ACTION_EXISTS_ARRAY;
		}
		else{
			return ACTION_NOT_EXISTS_ARRAY;
		}
	}
	/**
	 *Sets the item field with the specified key.
	 */
	public void setItemField(String key, ContentItemField field) {
		if (this._itemFields == null) {
			this._itemFields = new HashMap<>();
		}

		key = key + getLanguage();
		List<ContentItemField> items = _itemFields.get(key);
		if (items == null) {
			items = new ArrayList<>();
			this._itemFields.put(key, items);
		}
		items.add(field);
	}

	/**
	 * Returns the list of item fields with the specified key.
	 */
	public List<ContentItemField> getItemFields(String key) {
		if (this._itemFields == null) {
			return null;
		}
		return this._itemFields.get(key + getLanguage());
	}

	/**
	 *Sets the list of item fields with the specified key.
	 */
	public void setItemFields(String key, List<ContentItemField> fields) {
		if (this._itemFields == null) {
			this._itemFields = new HashMap<>();
		}
		this._itemFields.put(key + getLanguage(), fields);
	}

	/**
	 *Sets the item field value with the specified key.
	 */
	public void setItemFieldValue(String key, Object value) {
		if (this._itemFields == null) {
			this._itemFields = new HashMap<>();
		}
		ContentItemField field = getItemField(key);
		if (field == null) {
			String type = null;
			if (value instanceof String) {
				type = ContentItemField.FIELD_TYPE_STRING;
			} else if(value instanceof Timestamp) {
				type = ContentItemField.FIELD_TYPE_TIMESTAMP;
			}
			field = new ContentItemFieldBean(-1, -1, key, value, 0, type);
			setItemField(key, field);
		} else {
			field.setValue(value);
		}

	}

	/**
	 * Returns the case for this content item.
	 */
	public ContentItemCase getCase() {
		return this._caseBean;
	}

	/**
	 * Sets the case for this content item.
	 */
	public void setCase(ContentItemCase caseBean) {
		this._caseBean = caseBean;
	}

	/**
	 * Returns the case status for this content item.
	 */
	public String getStatus() {
		return (String)getValue(FIELDNAME_STATUS);
	}

	/**
	 * Sets the case status for this content item.
	 */
	public void setStatus(String status) {
		setValue(FIELDNAME_STATUS,status);
	}

	/**
	 * Update pending locale change.
	 */
	public void updateLocale() {
		if (this._pendingLocaleId != null) {
			setLocale(new Locale(this._pendingLocaleId));
			this._pendingLocaleId = null;
		}
	}

	/**
	 * <p>
	 * Loads this resource from the folder set by setResourcepath();
	 * </p>
	 * @throws IOException
	 * @throws Exception If there is an exception loading
	 */
	@Override
	public void load() throws IOException {
		if (!isLoaded()) {
			String resourcePath = getResourcePath();
			if (resourcePath == null)
				throw new FileNotFoundException("Error loading content Item. No resourcePath set");

			setLoaded(load(resourcePath));
		}
	}

	public void reload() throws IOException {
		setLoaded(false);
		load();
	}

	/**
	 * Loads all xml files in the given folder
	 * @param folder
	 * @return List containing ArticleItemBean
	 * @throws IOException
	 * @throws XmlException
	 * @throws IOException
	 */
	protected boolean load(String path) throws IOException {
		clear();
		return loadFromJCR(path);
	}

//	protected boolean loadFromWebDav(String path) throws IOException,
//			RemoteException, HttpException {
//		IWUserContext iwuc = IWContext.getInstance();
//		boolean returner = true;
//		try {
//			IWSlideSession session = getIWSlideSession(iwuc);
//
//			WebdavExtendedResource webdavResource = session.getResource(path, Boolean.FALSE);
//			webdavResource.setProperties();
//
//			//here I don't use the varible 'path' since it can actually be the URI
//			setResourcePath(webdavResource.getPath());
//			setName(webdavResource.getDisplayName());
//
//			//String versionName = webdavResource.getVersionName();
//			/*List versions = VersionHelper.getAllVersions(webdavResource);
//			if(versions!=null){
//				setVersions(versions);
//				String latestVersion = VersionHelper.getLatestVersionName(versions);
//				setVersionName(latestVersion);
//			}*/
//
//			String createDate = webdavResource.getCreationDateString();
//			if(createDate != null){
//				setCreationDate(new IWTimestamp(createDate).getTimestamp());
//			}
//
//			long lLastmodified = webdavResource.getGetLastModified();
//			IWTimestamp lastModified = new IWTimestamp(lLastmodified);
//			setLastModifiedDate(lastModified.getTimestamp());
//
//			setWebDavResourceCategories(webdavResource.propfindMethod(IWSlideConstants.PROPERTYNAME_CATEGORY));
//			returner = load(webdavResource);
//			setExists(true);
////			System.out.print("["+this.toString()+"]:");
////			System.out.println("Load "+((returner)?"":"not")+" successful of path "+path);
//		} catch(HttpException e) {
//			if(e.getReasonCode()==WebdavStatus.SC_NOT_FOUND) {
//				/*if(isAutoCreateResource()){
//					//in this case ignore the error message that it isn't fount
//					return true;
//				}
//				else{*/
//					setRendered(false);
//					return false;
//				//}
//			} else {
//				throw e;
//			}
//		}
//		return returner;
//	}

	protected boolean loadFromJCR(String path) throws IOException {
		if (isLoaded())
			return true;

		boolean result = true;
		try {
			RepositoryItem item = getRepositoryService().getRepositoryItemAsRootUser(path);
			if (item == null || !item.exists())
				return false;

			setResourcePath(item.getPath());
			setName(item.getName());

			long created = item.getCreationDate();
			if (created > 0)
				setCreationDate(new IWTimestamp(created).getTimestamp());

			long lastModified = item.getLastModified();
			if (lastModified > 0)
				setLastModifiedDate(new IWTimestamp(lastModified).getTimestamp());

			result = load(item);
			setExists(result);
		} catch (RepositoryException e) {
			e.printStackTrace();
			setRendered(false);
			return false;
		}
		return result;
	}

	protected boolean load(RepositoryItem item) throws IOException, RepositoryException {
		return true;
	}

	public Timestamp getPublishedDate() {
		return (Timestamp) getValue(FIELDNAME_PUBLISHED_DATE);
	}

	public void setPublishedDate(Timestamp date) {
		setValue(FIELDNAME_PUBLISHED_DATE, date);
		if (date != null)
			setCreationDate(date);
	}

	@Override
	public Timestamp getCreationDate() {
		return (Timestamp) getValue(FIELDNAME_CREATION_DATE);
	}

	@Override
	public Timestamp getLastModifiedDate() {
		return (Timestamp) getValue(FIELDNAME_LAST_MODIFIED_DATE);
	}

	@Override
	public String getResourcePath() {
		return (String) getValue(FIELDNAME_RESOURCE_PATH);
	}

	@Override
	public String getVersionName() {
		return (String) getValue(FIELDNAME_VERSION_NAME);
	}

	public void setVersionName(String name) {
		setValue(FIELDNAME_VERSION_NAME, name);
	}

	@Override
	public boolean createItemFolder(){
		IWContext iwc = CoreUtil.getIWContext();
		return createItemFolder(iwc);
	}

	@Override
	public boolean createItemFolder(IWContext iwc) {
		try {
			return getRepositoryService().createFolderAsRoot(getResourcePath());
		} catch (RepositoryException e) {
			Logger.getLogger(ContentItemBean.class.getName()).log(Level.WARNING, "Failed creating item folder", e);
			return false;
		}
	}

	@Override
	public Boolean getRendered() {
		return this.doRender;
	}

	public void setRendered(boolean render) {
		setRendered(Boolean.valueOf(render));
	}

	public void setRendered(Boolean render){
		this.doRender = render;
	}

	public void setCreationDate(Timestamp date) {
		setValue(FIELDNAME_CREATION_DATE, date);
		//setItemType(ContentItemField.FIELD_TYPE_TIMESTAMP);
	}

	public void setLastModifiedDate(Timestamp date) {
		setValue(FIELDNAME_LAST_MODIFIED_DATE, date);
		//setItemType(ContentItemField.FIELD_TYPE_TIMESTAMP);
	}

	public void setResourcePath(String path) {
		setValue(FIELDNAME_RESOURCE_PATH, path);
	}

	/**
	 * @return Returns the autoCreateResource.
	 */
	public boolean isAutoCreateResource() {
		return this.autoCreateResource;
	}

	/**
	 * @param autoCreateResource The autoCreateResource to set.
	 */
	public void setAutoCreateResource(boolean autoCreateResource) {
		this.autoCreateResource = autoCreateResource;
	}

	/**
	 * <p>
	 * Gets wheather this contentItem exists in the content repository.
	 * </p>
	 * @return Returns the exists.
	 */
	public boolean getExists() {
		return this.exists;
	}

	/**
	 * @param exists The exists to set.
	 */
	public void setExists(boolean exists) {
		this.exists = exists;
	}

	/**
	 * <p>
	 * Gets the language part of the locale
	 * </p>
	 * @return
	 */
	public String getLanguage() {
		Locale locale = getLocale();
		return locale == null ? CoreConstants.EMPTY : locale.getLanguage();
	}

	public void setLanguage(String lang){
		if (lang == null) {
			return;
		}
		Locale locale = new Locale(lang);
		setLocale(locale);
	}

	/**
	 * @return Returns the versions.
	 */
	public List<RepositoryItemVersionInfo> getVersions() {
		return this.versions;
	}

	/**
	 * @param versions The versions to set.
	 */
	public void setVersions(List<RepositoryItemVersionInfo> versions) {
		this.versions = versions;
	}

	protected RepositoryItem getRepositoryItem() {
		String resourcePath = getResourcePath();
		IWUserContext iwuc = CoreUtil.getIWContext();
		RepositoryItem item = null;
		try {
			item = getRepositoryService().getRepositoryItem(iwuc.getLoggedInUser(), resourcePath);
		} catch (RepositoryException e) {
			e.printStackTrace();
		} catch (Exception e) {
			throw new RuntimeException(e);
		}
		return item;
	}

	@Override
	public void delete(){
		try {
			String resourcePath = getResourcePath();
			deleteFromJCR(resourcePath);
		} catch (Exception e) {
			throw new RuntimeException(e);
		}
	}

	private void deleteFromJCR(String resourcePath) throws Exception {
		getRepositoryService().delete(resourcePath);
		clear();
	}

	/**
	 * @return Returns the loaded.
	 */
	public boolean isLoaded() {
		return this.loaded;
	}

	/**
	 * @param loaded The loaded to set.
	 */
	public void setLoaded(boolean loaded) {
		this.loaded = loaded;
	}

	public void setWebDavResourceCategories(Enumeration<String> webDavResourceCategories) {
		if (webDavResourceCategories == null) {
			setCategories(null);
		}
		StringBuffer categories = new StringBuffer();
		while (webDavResourceCategories.hasMoreElements()) {
			categories.append(webDavResourceCategories.nextElement());
		}
		setCategories(categories.toString());
	}

	public String getCategoriesString() {
		return this.categories;
	}

	public void setCategories(String categories){
		this.categories=categories;
	}

	/**
	 * Adds new entry to feed or modifies existing entry in feed
	 * @param iwc
	 * @param feedTitle
	 * @param feedName
	 * @param feedType
	 * @param feedDescription
	 * @param title
	 * @param description
	 * @param body
	 * @param author
	 * @param categories
	 * @param source
	 * @param comment
	 * @param moduleClass
	 * @return String of SyndFeed xml if entry was successfully added to feed, otherwise - null
	 */
	public String getFeedEntryAsXML(IWContext iwc, String feedTitle, String feedDescription, String moduleClass,
			EntryData entryData) {

		ThemesHelper themesHelper = null;
		try {
			themesHelper = ELUtil.getInstance().getBean(ThemesHelper.class);
		} catch(Exception e) {
			e.printStackTrace();
		}

		Timestamp published = getPublishedDate();
		String server = themesHelper.getFullServerName(iwc);
		StringBuffer articleURL = new StringBuffer(server);
		ContentItemHelper helper = new ContentItemHelper(getResourcePath());
		String pageUri = helper.getPageUrlByArticleResourcePath(iwc, moduleClass);
		articleURL.append(pageUri);

		if (isSetPublishedDateByDefault()) {
			if (published == null && iwc.hasRole(StandardRoles.ROLE_KEY_EDITOR)) {
				// Setting published date the same as creation
				published = new Timestamp(System.currentTimeMillis());
				setPublishedDate(published);
			}
		}

		String creatorId = null;
		if (getCreatedByUserId() < 0) {
			creatorId = iwc.getCurrentUser().getId();
			setCreatedByUserId(Integer.valueOf(creatorId));
		}
		else {
			creatorId = String.valueOf(getCreatedByUserId());
		}

		if (entryData.getLinkToComments() == null) {
			entryData.setLinkToComments(themesHelper.getArticleCommentLink(iwc, pageUri));
		}

		String fixedDescription = getFixedDescription(entryData.getDescription());
		entryData.setDescription(fixedDescription);

		ContentItemFeedBean feedBean = new ContentItemFeedBean(iwc, ContentItemFeedBean.FEED_TYPE_ATOM_1);
		entryData.setLink(articleURL.toString());
		return getFeedEntryAsXML(feedTitle, feedDescription,server, feedBean, entryData);
	}

	public String getFeedEntryAsXML(String feedTitle, String feedDescription, String server,ContentItemFeedBean feedBean, EntryData entryData) {
		entryData.setLanguage(getLanguage());
		return feedBean.getFeedEntryAsXML(feedTitle, server, feedDescription, entryData);
	}

	private String getFixedDescription(String description) {
		boolean changeCurrentDescription = false;
		if (description == null) {
			changeCurrentDescription = true;
		}
		else {
			String tempValue = description;
			// Removing needless characters
			tempValue = tempValue.replaceAll("\b", CoreConstants.EMPTY);
			tempValue = tempValue.replaceAll("\t", CoreConstants.EMPTY);
			tempValue = tempValue.replaceAll("\f", CoreConstants.EMPTY);
			tempValue = tempValue.replaceAll("\r", CoreConstants.SPACE);
			tempValue = tempValue.replaceAll("\n", CoreConstants.EMPTY);
			for (int i = 0; i < ThemesConstants.FIRST_SENTENCES_OF_DUMMY_ARTICLES.size(); i++) {
				if (tempValue.indexOf(ThemesConstants.FIRST_SENTENCES_OF_DUMMY_ARTICLES.get(i)) != -1) {
					changeCurrentDescription = true;
				}
			}
		}
		if (changeCurrentDescription) {
			description = CoreConstants.EMPTY;
		}

		return description;
	}

	@SuppressWarnings("finally")
	public Timestamp getParsedDateFromFeed(String dateValue) {
		Timestamp t = null;
		if (StringUtil.isEmpty(dateValue)) {
			return t;
		}

		try {
			t = new Timestamp(DateParser.parseW3CDateTime(dateValue).getTime());
		} catch(Exception e) {
			e.printStackTrace();
		} finally {
			return t;
		}
	}

	public boolean isDummyContentItem() {
		boolean isDummyArticle = false;
		String body = null;

		Object value = getValue(FIELDNAME_BODY);
		if (value instanceof String) {
			body = (String) value;
		}

		if (body == null) {
			return false;
		}

		if (body.equals(ContentConstants.ARTICLE_NOT_AVAILABLE_BODY)) {
			return true;
		}

		String tempValue = body;
		// Removing needless characters
		tempValue = tempValue.replaceAll("\b", CoreConstants.EMPTY);
		tempValue = tempValue.replaceAll("\t", CoreConstants.EMPTY);
		tempValue = tempValue.replaceAll("\f", CoreConstants.EMPTY);
		tempValue = tempValue.replaceAll("\r", CoreConstants.SPACE);
		tempValue = tempValue.replaceAll("\n", CoreConstants.EMPTY);
		for (int i = 0; (i < ThemesConstants.DUMMY_ARTICLES.size() && !isDummyArticle); i++) {
			if (tempValue.indexOf(ThemesConstants.DUMMY_ARTICLES.get(i)) != -1) {
				isDummyArticle = true;
			}
		}

		return isDummyArticle;
	}

	public boolean isSetPublishedDateByDefault() {
		return setPublishedDateByDefault;
	}

	public void setSetPublishedDateByDefault(boolean setPublishedDateByDefault) {
		this.setPublishedDateByDefault = setPublishedDateByDefault;
	}

	protected RepositoryService getRepositoryService() {
		return ELUtil.getInstance().getBean(RepositoryService.class);
	}

	public boolean isPersistToWebDav() {
		return getContentRepositoryMode().isPersistToWebDav();
	}

	public boolean isPersistToJCR() {
		return getContentRepositoryMode().isPersistToJCR();
	}

	public ContentRepositoryMode getContentRepositoryMode(){
		return (ContentRepositoryMode)ELUtil.getInstance().getBean(ContentRepositoryMode.SPRING_BEAN_IDENTIFIER);
	}

}