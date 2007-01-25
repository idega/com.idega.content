/*
 * $Id: CategoryBean.java,v 1.6 2007/01/25 13:53:18 gediminas Exp $
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
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Iterator;
import java.util.StringTokenizer;

import org.apache.commons.httpclient.HttpException;
import org.apache.webdav.lib.PropertyName;
import org.apache.webdav.lib.WebdavResource;
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
 *  Last modified: $Date: 2007/01/25 13:53:18 $ by $Author: gediminas $
 * 
 * @author <a href="mailto:Joakim@idega.com">Joakim</a>,<a href="mailto:tryggvi@idega.com">Tryggvi Larusson</a>
 * @version $Revision: 1.6 $
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
	
	public static final String CATEGORY_DELIMETER = ",";

	protected CategoryBean(IWMainApplication iwma){
		this.iwma=iwma;
		this.resourceBundle = new WebDAVResourceBundle(CATEGORY_PROPERTIES_FILE);
		if (getCategories().isEmpty()) {
			CategoriesMigrator migrator = new CategoriesMigrator();
			Collection oldCategories = getCategoriesFromString(getCategoriesAsString());
			migrator.migrate(oldCategories);
		}
	}

	private class CategoriesMigrator {
		private final String PROPERTY_NAME_CATEGORIES = new PropertyName("DAV","categories").toString();

		private HashMap valuesToKeys;
		private IWSlideSession session;
		private IWSlideService service;
		
		private void migrate(Collection cats) {
			System.out.println("Migrating " + CATEGORY_CONFIG_FILE + " to new format at " + CATEGORY_PROPERTIES_FILE);
			HashMap map = new HashMap();
			valuesToKeys = new HashMap();
			for (Iterator iter = cats.iterator(); iter.hasNext();) {
				String cat = (String) iter.next();
				String key = CategoryBean.getCategoryKey(cat);
				map.put(key, cat);
				valuesToKeys.put(cat, key);
			}
			resourceBundle.putAll(map);
			
			try {
				IWContext iwc = IWContext.getInstance();
				session = (IWSlideSession)IBOLookup.getSessionInstance(iwc,IWSlideSession.class);
				service = (IWSlideService)IBOLookup.getServiceInstance(iwc,IWSlideService.class);
	
				updateCategoriesOnFiles(CATEGORY_CONFIG_PATH);
	
				/*
				System.out.println("Deleting old file " + CATEGORY_CONFIG_FILE);
				WebdavResource resource = session.getWebdavResource(service.getURI(CATEGORY_CONFIG_FILE));
				resource.deleteMethod();
				*/
			} catch (IBOLookupException e) {
				e.printStackTrace();
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
		
		private void updateCategoriesOnFiles(String resourcePath) {
			try {
				String filePath = resourcePath;
				String serverURI = service.getWebdavServerURI();
				if(!resourcePath.startsWith(serverURI)) {
					filePath = service.getURI(resourcePath);
				}
	
				WebdavResource resource = session.getWebdavResource(filePath);
				
				String oldCats = CATEGORY_DELIMETER;
				Enumeration enumerator = resource.propfindMethod(PROPERTY_NAME_CATEGORIES);
				if (enumerator.hasMoreElements()) {
					StringBuffer cats = new StringBuffer();
					while(enumerator.hasMoreElements()) {
						cats.append(enumerator.nextElement());
					}
					oldCats = cats.toString();
				}
				
				if (!oldCats.equals(CATEGORY_DELIMETER) && !oldCats.equals("")) {
					System.out.println("Updating categories on resource " + resourcePath);
					System.out.println("- " + oldCats);
					
					StringTokenizer tokenizer = new StringTokenizer(oldCats, CATEGORY_DELIMETER);
					StringBuffer newCats = new StringBuffer(CATEGORY_DELIMETER);
					while (tokenizer.hasMoreTokens()) {
						String cat = tokenizer.nextToken();
						String key = (String) valuesToKeys.get(cat);
						// if we renamed the category key, replace category with it, otherwise leave as is
						if (key != null) {
							newCats.append(key);
						} else {
							newCats.append(cat);
						}
						newCats.append(CATEGORY_DELIMETER);
					}
	
					System.out.println("+ " + newCats.toString());
					resource.proppatchMethod(PROPERTY_NAME_CATEGORIES, newCats.toString(), true);
				}
				
				// update categories on all child resources
				Enumeration children = resource.getChildResources().getResourceNames();
				if (children.hasMoreElements()) {
					while(children.hasMoreElements()) {
						String child = (String) children.nextElement();
						updateCategoriesOnFiles(child);
					}
				}
			
			} catch (Exception e) {
				System.err.println("Exception updating categories on resource " + resourcePath);
			}
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
	public static Collection<String> getCategoriesFromString(String categoryCommaSeparatedList){
		Collection<String> ret = new ArrayList<String>();
		
		if( categoryCommaSeparatedList != null){
			StringTokenizer st = new StringTokenizer(categoryCommaSeparatedList,CATEGORY_DELIMETER);
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

	private static final char[] LEAVE_AS_IS = {'1','2','3','4','5','6','7','8','9','0','-'};
	private static String getCategoryKey(String category) {
		return StringHandler.stripNonRomanCharacters(category, LEAVE_AS_IS).toLowerCase();
	}

}
