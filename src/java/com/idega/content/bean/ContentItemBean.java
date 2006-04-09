/*
 * $Id: ContentItemBean.java,v 1.20 2006/04/09 12:01:55 laddi Exp $
 *
 * Copyright (C) 2004-2005 Idega. All Rights Reserved.
 *
 * This software is the proprietary information of Idega.
 * Use is subject to license terms.
 *
 */
package com.idega.content.bean;

import java.io.IOException;
import java.io.Serializable;
import java.sql.Timestamp;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import javax.faces.context.FacesContext;

import org.apache.commons.httpclient.HttpException;
import org.apache.webdav.lib.util.WebdavStatus;

import com.idega.business.IBOLookup;
import com.idega.business.IBOLookupException;
import com.idega.idegaweb.IWUserContext;
import com.idega.presentation.IWContext;
import com.idega.slide.business.IWSlideService;
import com.idega.slide.business.IWSlideSession;
import com.idega.slide.util.WebdavExtendedResource;
import com.idega.util.IWTimestamp;

/**
 * <p>
 * Base bean for "content items", i.e. resources that can be read from the WebDav store
 * and displayed as content.
 * </p>
 *  Last modified: $Date: 2006/04/09 12:01:55 $ by $Author: laddi $
 * 
 * @author Anders Lindman,<a href="mailto:tryggvil@idega.com">tryggvil</a>
 * @version $Revision: 1.20 $
 */
public abstract class ContentItemBean implements Serializable, ContentItem{//,ICFile {
	
	private Locale _locale = null;
	private String _name = null;
	private String _description = null;
	private String _itemType = null;
	private int _createdByUserId = 0;
	private boolean autoCreateResource;
	private boolean loaded;
	
	private String _pendingLocaleId = null;
	private String _requestedStatus = null;
	
	private ContentItemCase _caseBean = null;
	
	private Map _itemFields = null;
	private Map _locales = null;
	
	public final static String FIELDNAME_ATTACHMENT = "attachment";
	public final static String FIELDNAME_CREATION_DATE = "creation_date";
	public final static String FIELDNAME_RESOURCE_PATH = "resource_path";
	public final static String FIELDNAME_VERSION_NAME = "version_name";
	public final static String FIELDNAME_STATUS = "status";
	public final static String FIELDNAME_LAST_MODIFIED_DATE = "lastmodified";
	
	private Boolean doRender = Boolean.TRUE;
	private boolean exists=false;

	private final static String[] ACTION_EXISTS_ARRAY = new String[] {"delete","edit"};
	private final static String[] ACTION_NOT_EXISTS_ARRAY = new String[] {"create"};
	
	private List versions;
	
	/**
	 * Default constructor.
	 */
	public ContentItemBean() {}
	
		
	public Locale getLocale() { 
		if(this._locale==null){
			FacesContext context = FacesContext.getCurrentInstance();
			return context.getViewRoot().getLocale();
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
			this._locales = new HashMap();
		}
		this._locales.put(locale.getLanguage(), locale);
	}
	
	public Map getLocales() { return this._locales; }
	
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
		this._createdByUserId = 0;

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
	/*
	 * 
	 */
	
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
			this._itemFields = new HashMap();
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
	 * @throws Exception If there is an exception loading
	 */
	public void load()throws Exception{
		if(!isLoaded()){
			String resourcePath = getResourcePath();
			if(resourcePath==null){
				throw new Exception("Error loading content Item. No resourcePath set");
			}
			load(resourcePath);
			setLoaded(true);
		}
	}
	
	public void reload()throws Exception{
		setLoaded(false);
		load();
	}
	
	/**
	 * Loads all xml files in the given folder
	 * @param folder
	 * @return List containing ArticleItemBean
	 * @throws XmlException
	 * @throws IOException
	 */
	protected boolean load(String path) throws Exception{
//		System.out.print("["+this.toString()+"]:");
//		System.out.println("Attempting to load path "+path);
		clear();
		IWUserContext iwuc = IWContext.getInstance();
		boolean returner = true;
		try {
			IWSlideSession session = getIWSlideSession(iwuc);
			
			WebdavExtendedResource webdavResource = session.getWebdavResource(path);
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
			
			
			returner = load(webdavResource);
			setExists(true);
//			System.out.print("["+this.toString()+"]:");
//			System.out.println("Load "+((returner)?"":"not")+" successful of path "+path);
		}catch(HttpException e) {
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
	 */
	protected boolean load(WebdavExtendedResource webdavResource) throws Exception {
		return true;
	}

	
	protected IWSlideSession getIWSlideSession(IWUserContext iwuc){
		IWSlideSession session=null;
		try {
			session = (IWSlideSession)IBOLookup.getSessionInstance(iwuc,IWSlideSession.class);
		}
		catch (IBOLookupException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return session;
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
			IWSlideService slideService = (IWSlideService) IBOLookup.getServiceInstance(iwuc.getApplicationContext(),IWSlideService.class);
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
	

	public void delete(){
		try {
			String resourcePath = getResourcePath();
			WebdavExtendedResource webdavResource = getWebdavResource();
			webdavResource.deleteMethod();
			webdavResource.close();
			clear();
			System.out.println("Deleted: "+resourcePath+" successfully");
		}
		catch (Exception e) {
			throw new RuntimeException(e);
		}
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

}