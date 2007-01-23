/*
 * $Id: CategoryBean.java,v 1.3.2.1 2007/01/23 12:02:37 gediminas Exp $
 *
 * Copyright (C) 2004 Idega. All Rights Reserved.
 *
 * This software is the proprietary information of Idega.
 * Use is subject to license terms.
 *
 */
package com.idega.content.business;

import java.io.IOException;
import java.rmi.RemoteException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.StringTokenizer;
import java.util.TreeMap;

import org.apache.commons.httpclient.HttpException;
import org.apache.webdav.lib.util.WebdavStatus;

import com.idega.business.IBOLookup;
import com.idega.business.IBOLookupException;
import com.idega.idegaweb.IWMainApplication;
import com.idega.idegaweb.IWUserContext;
import com.idega.presentation.IWContext;
import com.idega.slide.business.IWSlideService;
import com.idega.slide.business.IWSlideSession;
import com.idega.slide.util.WebdavRootResource;
import com.idega.util.StringHandler;


/**
 * <p>
 * Class for manipulating Categories that are stored in slide.<br/>
 * Includes functions for getting and setting all the available categories
 * </p>
 *  Last modified: $Date: 2007/01/23 12:02:37 $ by $Author: gediminas $
 * 
 * @author <a href="mailto:Joakim@idega.com">Joakim</a>,<a href="mailto:tryggvi@idega.com">Tryggvi Larusson</a>
 * @version $Revision: 1.3.2.1 $
 */
public class CategoryBean {
	
	private static String BEAN_KEY="ContentCategoryBean";
	
	private static final String CATEGORY_CONFIG_PATH = "/files/cms/";
	/**
	 * @deprecated this file will no longer be used
	 */
	private static final String CATEGORY_CONFIG_FILE = CATEGORY_CONFIG_PATH+"categories.prop";
	private static final String CATEGORY_PROPERTIES_FILE = CATEGORY_CONFIG_PATH+"categories.strings";
	private IWMainApplication iwma;
	private WebDAVResourceBundle resourceBundle;

	protected CategoryBean(IWMainApplication iwma){
		this.iwma=iwma;
		this.resourceBundle = new WebDAVResourceBundle(CATEGORY_PROPERTIES_FILE);
		migrateOldFile();
	}
	
	private void migrateOldFile() {
		if (getCategories().isEmpty()) {
			Collection cats = getCategoriesFromString(getCategoriesAsString());
			TreeMap map = new TreeMap();
			for (Iterator iter = cats.iterator(); iter.hasNext();) {
				String cat = (String) iter.next();
				String key = getCategoryKey(cat);
				map.put(key, cat);
			}
			resourceBundle.putAll(map);

			/*
			try {
				WebdavResource resource = getSlideService().getWebdavResourceAuthenticatedAsRoot(CATEGORY_CONFIG_FILE);
				resource.deleteMethod();
			} catch (IOException e) {
				e.printStackTrace();
			}
			*/
		}
	}

	/**
	 * <p>
	 * Get the instance of the bean for this application.
	 * </p>
	 * @return
	 */
	public static CategoryBean getInstance(){
		IWMainApplication iwma = IWMainApplication.getDefaultIWMainApplication();
		CategoryBean bean = (CategoryBean) iwma.getAttribute(BEAN_KEY);
		if(bean==null){
			bean = new CategoryBean(iwma);
			iwma.setAttribute(BEAN_KEY,bean);
		}
		return bean;
	}
	
	public IWSlideService getSlideService(){
		try {
			return (IWSlideService)IBOLookup.getServiceInstance(this.iwma.getIWApplicationContext(),IWSlideService.class);
		}
		catch (IBOLookupException e) {
			throw new RuntimeException("Error getting IWSlideService");
		}
	}
	
	/**
	 * <p> Get a collection of categories </p>
	 * @return collection of strings
	 */
	public Collection getCategories() {
		//Collection ret = getCategoriesFromString(getCategoriesAsString());
		return resourceBundle.keySet();
	}
	
	public String getCategoryName(String categoryKey) {
		return resourceBundle.getString(categoryKey);
	}
	
	/**
	 * <p>Getts all the categories as one String (comma separated)</p>
	 * @return categories
	 * @deprecated this file is no longer used
	 */
	public String getCategoriesAsString() {
		IWUserContext iwuc = IWContext.getInstance();
		String categories = null;
		try {
			IWSlideSession session = (IWSlideSession)IBOLookup.getSessionInstance(iwuc,IWSlideSession.class);
			WebdavRootResource rootResource = session.getWebdavRootResource();
//			String filePath = service.getURI(CATEGORY_FILE_PATH);
//			System.out.println("Loading categories for "+filePath);
			
			String path = getSlideService().getURI(CATEGORY_CONFIG_FILE);
			
			categories = rootResource.getMethodDataAsString(path);
			if(rootResource.getStatusCode() != WebdavStatus.SC_OK){
				return "";
			}
		}
		catch (IBOLookupException e) {
			e.printStackTrace();
		}
		catch (RemoteException e) {
			e.printStackTrace();
		}
		catch (HttpException e) {
			e.printStackTrace();
		}
		catch (IOException e) {
			e.printStackTrace();
		}
		return categories;
	}
	
	/**
	 * <p>
	 * Constructs a collection from a comma separated list of categories
	 * </p>
	 * @param categoryCommaSeparatedList
	 */
	public static Collection getCategoriesFromString(String categoryCommaSeparatedList){
		Collection ret = new ArrayList();
		
		if( categoryCommaSeparatedList != null){
			StringTokenizer st = new StringTokenizer(categoryCommaSeparatedList,",");
			while(st.hasMoreTokens()) {
				ret.add(st.nextToken().trim());
			}
		}
		return ret;
	}
	
	/**
	 * <p>Adds a category to the available categories</p>
	 * @param category
	 */
	public void addCategory(String category) {
		String key = getCategoryKey(category);
		resourceBundle.put(key, category);
	}

	private String getCategoryKey(String category) {
		return StringHandler.stripNonRomanCharacters(category).toLowerCase();
	}

}
