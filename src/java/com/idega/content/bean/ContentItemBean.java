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
import java.io.InputStream;
import java.io.Serializable;
import java.rmi.RemoteException;
import java.sql.Timestamp;
import java.util.Calendar;
import java.util.Enumeration;
import java.util.GregorianCalendar;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import javax.faces.context.FacesContext;
import javax.jcr.Node;
import javax.jcr.PathNotFoundException;
import javax.jcr.Property;
import javax.jcr.RepositoryException;
import javax.jcr.Session;
import javax.jcr.ValueFormatException;

import org.apache.commons.httpclient.HttpException;
import org.apache.webdav.lib.WebdavResource;
import org.apache.webdav.lib.util.WebdavStatus;

import com.idega.block.rss.business.EntryData;
import com.idega.business.IBOLookup;
import com.idega.business.IBOLookupException;
import com.idega.content.business.ContentConstants;
import com.idega.content.business.ContentItemHelper;
import com.idega.content.themes.helpers.business.ThemesConstants;
import com.idega.content.themes.helpers.business.ThemesHelper;
import com.idega.core.accesscontrol.business.StandardRoles;
import com.idega.core.content.RepositoryHelper;
import com.idega.idegaweb.IWMainApplication;
import com.idega.idegaweb.IWUserContext;
import com.idega.presentation.IWContext;
import com.idega.slide.business.IWSlideService;
import com.idega.slide.business.IWSlideSession;
import com.idega.slide.util.IWSlideConstants;
import com.idega.slide.util.WebdavExtendedResource;
import com.idega.util.CoreConstants;
import com.idega.util.IWTimestamp;
import com.idega.util.expression.ELUtil;
import com.sun.syndication.io.impl.DateParser;

/**
 * <p>
 * Base bean for "content items", i.e. resources that can be read from the WebDav store
 * and displayed as content.
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

	private Map _itemFields = null;
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

	private List versions;

	private String categories = null; // This string is parsed from WebDavResource

	private boolean setPublishedDateByDefault = false;
	//private boolean persistToWebDav=false;
	//private boolean persistToJCR=true;
	private Session session;
	/**
	 * Default constructor.
	 */
	public ContentItemBean() {}

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
//	public Date getCreatedTimestamp() { return _createdTimestamp; }
	public int getCreatedByUserId() { return this._createdByUserId; }

	public void setName(String s) { this._name = s; }
	public void setDescription(String s) { this._description = s; }
	public void setItemType(String s) { this._itemType = s; }
//	public void setCreatedTimestamp(Date d) { _createdTimestamp = d; }
	public void setCreatedByUserId(int id) { this._createdByUserId = id; }

	public void setLocale(Locale locale) {
		this._locale = locale;
		if (this._locales == null) {
			this._locales = new HashMap<String, Locale>();
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
		//setLocale(Locale.getDefault());
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
			this._itemFields = new HashMap();
		}
		ContentItemField field = (ContentItemField) this._itemFields.get(key + getLanguage());
		return field;
	}


	/*
	 *
	 */
	public List getAttachments() { return getItemFields(FIELDNAME_ATTACHMENT); }
	public void setAttachment(List l) { setItemFields(FIELDNAME_ATTACHMENT, l); }

	public Object getValue(String fieldName){
		ContentItemField field = getItemField(fieldName);
		if(field != null){
			return field.getValue();
		} else {
			return null;
		}
	}

	public void setValue(String fieldName, Object value){
		setItemFieldValue(fieldName, value);
	}

	public abstract String[] getContentFieldNames();

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
			this._itemFields = new HashMap();
		}
		this._itemFields.put(key + getLanguage(), field);
	}

	/**
	 * Returns the list of item fields with the specified key.
	 */
	public List getItemFields(String key) {
		if (this._itemFields == null) {
			return null;
		}
		return (List) this._itemFields.get(key + getLanguage());
	}

	/**
	 *Sets the list of item fields with the specified key.
	 */
	public void setItemFields(String key, List fields) {
		if (this._itemFields == null) {
			this._itemFields = new HashMap<String, List>();
		}
		this._itemFields.put(key + getLanguage(), fields);
	}

	/**
	 *Sets the item field value with the specified key.
	 */
	public void setItemFieldValue(String key, Object value) {
		if (this._itemFields == null) {
			this._itemFields = new HashMap();
		}
		ContentItemField field = getItemField(key);
		if (field == null) {
			String type = null;
			if(value instanceof String){
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
	public void load() throws IOException {
		if(!isLoaded()){
			String resourcePath = getResourcePath();
			if(resourcePath==null){
				throw new FileNotFoundException("Error loading content Item. No resourcePath set");
			}
			load(resourcePath);
			setLoaded(true);
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
		if (isLoaded())
			return true;

		clear();
		boolean returner = false;
		if(isPersistToWebDav()){
			returner = loadFromWebDav(path);
		}
		else if(isPersistToJCR()){
			returner = loadFromJCR(path);
		}

		setLoaded(returner);
		return returner;
	}

	protected boolean loadFromWebDav(String path) throws IOException,
			RemoteException, HttpException {
		IWUserContext iwuc = IWContext.getInstance();
		boolean returner = true;
		try {
			IWSlideSession session = getIWSlideSession(iwuc);

			WebdavExtendedResource webdavResource = session.getResource(path, Boolean.FALSE);
			webdavResource.setProperties();

			//here I don't use the varible 'path' since it can actually be the URI
			setResourcePath(webdavResource.getPath());
			setName(webdavResource.getDisplayName());

			//String versionName = webdavResource.getVersionName();
			/*List versions = VersionHelper.getAllVersions(webdavResource);
			if(versions!=null){
				setVersions(versions);
				String latestVersion = VersionHelper.getLatestVersionName(versions);
				setVersionName(latestVersion);
			}*/

			String createDate = webdavResource.getCreationDateString();
			if(createDate != null){
				setCreationDate(new IWTimestamp(createDate).getTimestamp());
			}

			long lLastmodified = webdavResource.getGetLastModified();
			IWTimestamp lastModified = new IWTimestamp(lLastmodified);
			setLastModifiedDate(lastModified.getTimestamp());

			setWebDavResourceCategories(webdavResource.propfindMethod(IWSlideConstants.PROPERTYNAME_CATEGORY));
			returner = load(webdavResource);
			setExists(true);
//			System.out.print("["+this.toString()+"]:");
//			System.out.println("Load "+((returner)?"":"not")+" successful of path "+path);
		} catch(HttpException e) {
			if(e.getReasonCode()==WebdavStatus.SC_NOT_FOUND) {
				/*if(isAutoCreateResource()){
					//in this case ignore the error message that it isn't fount
					return true;
				}
				else{*/
					setRendered(false);
					return false;
				//}
			} else {
				throw e;
			}
		}
		return returner;
	}

	/**
	 * @param webdavResource
	 * @throws IOException
	 */
	protected boolean load(WebdavExtendedResource webdavResource) throws IOException {
		return true;
	}

	protected boolean loadFromJCR(String path) throws IOException,
	RemoteException, HttpException {
		//IWContext iwc = IWContext.getInstance();
		boolean returner = true;
		try {
			//IWSlideSession session = getIWSlideSession(iwuc);
			Session session = getSession();
			Node folderNode = session.getRootNode().getNode(path);

			//WebdavExtendedResource webdavResource = session.getWebdavResource(path);
			//webdavResource.setProperties();

			//here I don't use the varible 'path' since it can actually be the URI
			setResourcePath(folderNode.getPath());
			Property displayNameProp = folderNode.getProperty(DISPLAYNAME);
			if(displayNameProp!=null){
				setName(displayNameProp.getValue().getString());
			}

			//String versionName = webdavResource.getVersionName();
			/*List versions = VersionHelper.getAllVersions(webdavResource);
			if(versions!=null){
				setVersions(versions);
				String latestVersion = VersionHelper.getLatestVersionName(versions);
				setVersionName(latestVersion);
			}*/

			Property createDateProp = folderNode.getProperty(CREATIONDATE);
			if(createDateProp!=null){
				try{
					Calendar creationDate = createDateProp.getDate();
					//String sCreateDate = createDateProp.getValue().getString();
					setCreationDate(new IWTimestamp((GregorianCalendar)creationDate).getTimestamp());
				}
				catch(Exception e){
					e.printStackTrace();
				}
			}

			Property lastmodifiedProp = folderNode.getProperty(GETLASTMODIFIED);
			if(lastmodifiedProp!=null){
				//long lLastmodified = Long.parseLong(lastmodifiedProp.getValue().getString());
				try{
					//long lLastmodified = lastmodifiedProp.getValue().getLong();
					//IWTimestamp lastModified = new IWTimestamp(lLastmodified);
					//setLastModifiedDate(lastModified.getTimestamp());
					Calendar creationDate = lastmodifiedProp.getDate();
					//String sCreateDate = createDateProp.getValue().getString();
					setLastModifiedDate(new IWTimestamp((GregorianCalendar)creationDate).getTimestamp());
				}
				catch(ValueFormatException vfe){
					vfe.printStackTrace();
				}
			}

			try{
				Property categoriesProp = folderNode.getProperty(IWSlideConstants.PROPERTYNAME_CATEGORY);
				if(categoriesProp!=null){
					String categories = categoriesProp.getValue().getString();
					setCategories(categories);
				}
			}
			catch(PathNotFoundException pnfe){}

			returner = load(folderNode);
			setExists(true);
		//	System.out.print("["+this.toString()+"]:");
		//	System.out.println("Load "+((returner)?"":"not")+" successful of path "+path);
		} catch(PathNotFoundException e) {
			//if(e.getReasonCode()==WebdavStatus.SC_NOT_FOUND) {
				/*if(isAutoCreateResource()){
					//in this case ignore the error message that it isn't fount
					return true;
				}
				else{*/
					setRendered(false);
					return false;
				//}
			//} else {
			//	throw e;
			//}
		} catch (RepositoryException e) {
			e.printStackTrace();
			setRendered(false);
			return false;
		}
		return returner;
	}

	protected boolean load(Node folderNode) throws IOException, RepositoryException{
		return true;
	}

	protected IWSlideSession getIWSlideSession(IWUserContext iwuc){
		IWSlideSession session=null;
		try {
			session = (IWSlideSession)IBOLookup.getSessionInstance(iwuc,IWSlideSession.class);
		}
		catch (IBOLookupException e) {
			e.printStackTrace();
		}
		return session;
	}

	public Timestamp getPublishedDate() {
		return (Timestamp) getValue(FIELDNAME_PUBLISHED_DATE);
	}

	public void setPublishedDate(Timestamp date) {
		setValue(FIELDNAME_PUBLISHED_DATE, date);
		if (date != null) {
			setCreationDate(date);
		}
	}

	public Timestamp getCreationDate() {
		return (Timestamp)getValue(FIELDNAME_CREATION_DATE);
	}

	public Timestamp getLastModifiedDate() {
		return (Timestamp)getValue(FIELDNAME_LAST_MODIFIED_DATE);
	}

	public String getResourcePath() {
		return (String)getValue(FIELDNAME_RESOURCE_PATH);
	}

	public String getVersionName(){
		return (String)getValue(FIELDNAME_VERSION_NAME);
	}

	public void setVersionName(String name){
		setValue(FIELDNAME_VERSION_NAME,name);
	}

	public Boolean getRendered() {
		return this.doRender;
	}

	public void setRendered(boolean render){
		setRendered(Boolean.valueOf(render));
	}

	public void setRendered(Boolean render){
		this.doRender = render;
	}

	public void setCreationDate(Timestamp date) {
		setValue(FIELDNAME_CREATION_DATE,date);
		//setItemType(ContentItemField.FIELD_TYPE_TIMESTAMP);
	}

	public void setLastModifiedDate(Timestamp date) {
		setValue(FIELDNAME_LAST_MODIFIED_DATE,date);
		//setItemType(ContentItemField.FIELD_TYPE_TIMESTAMP);
	}

	public void setResourcePath(String path) {
		setValue(FIELDNAME_RESOURCE_PATH,path);
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
		return locale == null ? "" : locale.getLanguage();
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
	public List getVersions() {
		return this.versions;
	}

	/**
	 * @param versions The versions to set.
	 */
	public void setVersions(List versions) {
		this.versions = versions;
	}

	/**
	 * <p>
	 * TODO tryggvil describe method getIWSlideService
	 * </p>
	 * @param iwuc
	 * @return
	 */
	protected IWSlideService getIWSlideService(IWUserContext iwuc) {
		try {
			IWSlideService slideService = IBOLookup.getServiceInstance(iwuc.getApplicationContext(),IWSlideService.class);
			return slideService;
		}
		catch (IBOLookupException e) {
			throw new RuntimeException(e);
		}
	}

	protected WebdavExtendedResource getWebdavResource(){
		String resourcePath = getResourcePath();
		IWUserContext iwuc = IWContext.getInstance();
		IWSlideSession session = getIWSlideSession(iwuc);
		WebdavExtendedResource webdavResource;
		try {
			webdavResource = session.getWebdavResource(resourcePath);
			return webdavResource;
		}
		catch (Exception e) {
			throw new RuntimeException(e);
		}
	}

	protected Node getNode(){
		String resourcePath = getResourcePath();
		try {
			return getSession().getRootNode().getNode(resourcePath);
		}
		catch (Exception e) {
			throw new RuntimeException(e);
		}
	}

	public Session getSession() throws RepositoryException{
		if(session==null){
			IWContext iwc = IWContext.getInstance();
			Session session = iwc.getRepositorySession();
			return session;
		}
		else{
			return session;
		}
	}

	public void setSession(Session session){
		this.session=session;
	}

	public void delete(){
		try {
			String resourcePath = getResourcePath();
			if(isPersistToWebDav()){
				deleteFromWebDav(resourcePath);
			}
			else if(isPersistToJCR()){
				deleteFromJCR(resourcePath);
			}
			System.out.println("Deleted: "+resourcePath+" successfully");
		}
		catch (Exception e) {
			throw new RuntimeException(e);
		}
	}

	private void deleteFromWebDav(String resourcePath) throws HttpException, IOException {
		WebdavExtendedResource webdavResource = getWebdavResource();
		webdavResource.deleteMethod();
		webdavResource.close();
		clear();
	}

	private void deleteFromJCR(String resourcePath) throws Exception {
		Node folderNode = getNode();
		folderNode.remove();
		clear();
		getSession().save();
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

	public void setWebDavResourceCategories(Enumeration webDavResourceCategories) {
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
		Timestamp updated = getLastModifiedDate();
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


	public String getFeedEntryAsXML(String feedTitle, String feedDescription,
			String server,ContentItemFeedBean feedBean, EntryData entryData) {
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


	/*public void setPersistToWebDav(boolean persistToWebDav) {
		this.persistToWebDav = persistToWebDav;
	}*/


	public boolean isPersistToWebDav() {
		return getContentRepositoryMode().isPersistToWebDav();
	}


	/*public void setPersistToJCR(boolean persistToJCR) {
		this.persistToJCR = persistToJCR;
	}*/


	public boolean isPersistToJCR() {
		return getContentRepositoryMode().isPersistToJCR();
	}

	public RepositoryHelper getRepositoryHelper() {
		return (RepositoryHelper)ELUtil.getInstance().getBean(RepositoryHelper.SPRING_BEAN_IDENTIFIER);
	}

	public ContentRepositoryMode getContentRepositoryMode(){
		return (ContentRepositoryMode)ELUtil.getInstance().getBean(ContentRepositoryMode.SPRING_BEAN_IDENTIFIER);
	}

	protected InputStream getStream(WebdavResource resource) throws IOException {
		IWSlideService slideService = IBOLookup.getServiceInstance(IWMainApplication.getDefaultIWApplicationContext(), IWSlideService.class);
		return slideService.getInputStream(resource.getPath());
	}
}