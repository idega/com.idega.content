/*
 * $Id: CategoryBean.java,v 1.1 2007/09/24 15:04:06 valdas Exp $
 *
 * Copyright (C) 2004 Idega. All Rights Reserved.
 *
 * This software is the proprietary information of Idega.
 * Use is subject to license terms.
 *
 */
package com.idega.content.business.categories;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.rmi.RemoteException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.StringTokenizer;
import java.util.TreeMap;

import org.apache.commons.httpclient.HttpException;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.webdav.lib.PropertyName;
import org.apache.webdav.lib.WebdavResource;
import org.apache.webdav.lib.util.WebdavStatus;
import org.jdom.Document;
import org.jdom.Element;
import org.jdom.JDOMException;
import org.jdom.filter.Filter;
import org.jdom.input.SAXBuilder;

import com.idega.business.IBOLookup;
import com.idega.business.IBOLookupException;
import com.idega.content.data.CategoryComparator;
import com.idega.content.data.ContentCategory;
import com.idega.content.themes.helpers.ThemesConstants;
import com.idega.idegaweb.IWMainApplication;
import com.idega.idegaweb.IWUserContext;
import com.idega.presentation.IWContext;
import com.idega.slide.business.IWSlideService;
import com.idega.slide.business.IWSlideSession;
import com.idega.slide.util.WebdavRootResource;
import com.idega.util.CoreConstants;
import com.idega.util.StringHandler;


/**
 * <p>
 * Class for manipulating Categories that are stored in slide.<br/>
 * Includes functions for getting and setting all the available categories
 * </p>
 *  Last modified: $Date: 2007/09/24 15:04:06 $ by $Author: valdas $
 * 
 * @author <a href="mailto:Joakim@idega.com">Joakim</a>,<a href="mailto:tryggvi@idega.com">Tryggvi Larusson</a>
 * @version $Revision: 1.1 $
 */
public class CategoryBean {
	private static final Log log = LogFactory.getLog(CategoryBean.class);
	
	private static String BEAN_KEY="ContentCategoryBean";
	
	private static final String CATEGORY_CONFIG_PATH = CoreConstants.CONTENT_PATH + CoreConstants.SLASH;
	/**
	 * @deprecated this file will no longer be used
	 */
	private static final String CATEGORY_CONFIG_FILE = CATEGORY_CONFIG_PATH+"categories.prop";
	private static final String CATEGORY_PROPERTIES_FILE = CATEGORY_CONFIG_PATH+"categories.xml";
	private IWMainApplication iwma;
	protected Map<String, ContentCategory> categories;
	
	public static final String CATEGORY_DELIMETER = ",";

	private CategoryBean() {
		this(IWMainApplication.getDefaultIWMainApplication());
	}
	
	private CategoryBean(IWMainApplication iwma){
		this.iwma=iwma;
		this.categories = loadCategories();
		if (this.categories == null) {
			CategoriesMigrator migrator = new CategoriesMigrator();
			Collection<String> oldCategories = getCategoriesFromString(getCategoriesAsString());
			migrator.migrate(oldCategories);
		}
	}
	

	protected class CategoriesMigrator {
		private final Log log = LogFactory.getLog(CategoriesMigrator.class);
		private final String PROPERTY_NAME_CATEGORIES = new PropertyName("DAV","categories").toString();

		private HashMap<String, String> valuesToKeys;
		private IWSlideSession session;
		private IWSlideService service;
		
		protected void migrate(Collection<String> cats) {
			log.info("Migrating " + CATEGORY_CONFIG_FILE + " to new format at " + CATEGORY_PROPERTIES_FILE);
			categories = new TreeMap<String, ContentCategory>();
			valuesToKeys = new HashMap<String, String>();
			String lang = getCurrentLocale();
			for (Iterator<String> iter = cats.iterator(); iter.hasNext();) {
				String cat = (String) iter.next();
				String key = CategoryBean.getCategoryKey(cat);
				ContentCategory category = new ContentCategory(key);
				category.addName(lang, cat);
				categories.put(key, category);
				valuesToKeys.put(cat, key);
			}
			
			storeCategories();
			
			try {
				IWContext iwc = IWContext.getInstance();
				session = (IWSlideSession)IBOLookup.getSessionInstance(iwc,IWSlideSession.class);
				service = (IWSlideService)IBOLookup.getServiceInstance(iwc,IWSlideService.class);
	
				updateCategoriesOnFiles(CATEGORY_CONFIG_PATH);
	
				/*
				log.info("Deleting old file " + CATEGORY_CONFIG_FILE);
				WebdavResource resource = session.getWebdavResource(service.getURI(CATEGORY_CONFIG_FILE));
				resource.deleteMethod();
				*/
			} catch (IBOLookupException e) {
				e.printStackTrace();
			}
		}
		
		@SuppressWarnings("unchecked")
		private void updateCategoriesOnFiles(String resourcePath) {
			if (resourcePath.indexOf(ThemesConstants.THEMES_PATH) >= 0) {
				return;
			}
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
					log.info("Updating categories on resource " + resourcePath);
					log.info("- " + oldCats);
					
					StringTokenizer tokenizer = new StringTokenizer(oldCats, CATEGORY_DELIMETER);
					StringBuffer newCats = new StringBuffer(CATEGORY_DELIMETER);
					while (tokenizer.hasMoreTokens()) {
						String cat = tokenizer.nextToken();
						String key = valuesToKeys.get(cat);
						// if we renamed the category key, replace category with it, otherwise leave as is
						if (key != null) {
							newCats.append(key);
						} else {
							newCats.append(cat);
						}
						newCats.append(CATEGORY_DELIMETER);
					}
	
					log.info("+ " + newCats.toString());
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
				
				resource.close();			
			} catch (Exception e) {
				log.error("Exception updating categories on resource " + resourcePath + ": " + e.getMessage());
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
	
	public IWSlideSession getSlideSession() {
		try {
			IWContext iwc = IWContext.getInstance();
			IWSlideSession session = (IWSlideSession)IBOLookup.getSessionInstance(iwc,IWSlideSession.class);
			return session;
		} catch (IBOLookupException e) {
			throw new RuntimeException("Error getting IWSlideSession");
		}
	}

	public IWSlideService getSlideService() {
		try {
			return (IWSlideService)IBOLookup.getServiceInstance(this.iwma.getIWApplicationContext(),IWSlideService.class);
		}
		catch (IBOLookupException e) {
			throw new RuntimeException("Error getting IWSlideService");
		}
	}
	
	/**
	 * <p> Get a collection of categories, sorted by keys </p>
	 * @return collection of strings
	 */
	public Collection<ContentCategory> getCategories() {
		return this.categories.values();
	}
	
	public ContentCategory getCategory(String id) {
		return this.categories.get(id);
	}
	
	/**
	 * <p> Get a collection of categories, sorted by given locale </p>
	 * @return collection of strings
	 */
	public Collection<ContentCategory> getCategories(Locale locale) {
		CategoryComparator comparator = new CategoryComparator(locale);
		List<ContentCategory> list = new ArrayList<ContentCategory>(this.categories.values());
		Collections.sort(list, comparator);
		return Collections.unmodifiableCollection(list);
	}
	
	public String getCategoryName(String categoryKey) {
		ContentCategory cat = this.categories.get(categoryKey);
		if (cat == null) {
			return categoryKey;
		}
		String lang = getCurrentLocale();
		String name = cat.getName(lang);
		if (name == null) {
			lang = iwma.getDefaultLocale().toString();
			name = cat.getName(lang);
		}
		if (name == null) {
			name = categoryKey;
		}
		return name;
	}
	
	protected String getCurrentLocale() {
		return IWContext.getInstance().getCurrentLocale().toString();
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
		addCategory(category, getCurrentLocale());
	}
	
	/**
	 * Adds a category to the available categories
	 * @param category
	 * @param language
	 * @return
	 */
	public boolean addCategory(String category, String language) {
		if (category == null || CoreConstants.EMPTY.equals(category)) {
			return false;
		}
		String key = getCategoryKey(category);
		ContentCategory cat = this.categories.get(key);
		if (cat == null) {
			cat = new ContentCategory(key);
		}
		cat.addName(language, category);
		this.categories.put(key, cat);
		storeCategories();
		return true;
	}

	protected static final char[] LEAVE_AS_IS = {'-','0','1','2','3','4','5','6','7','8','9'};
	protected static String getCategoryKey(String category) {
		return StringHandler.stripNonRomanCharacters(category, LEAVE_AS_IS).toLowerCase();
	}
	
	/**
	 * Loads category definitions from <code>categories.xml</code> file.
	 * @return categories as Map<String, ContentCategory>, or <code>null</code> if loading failed.
	 */
	@SuppressWarnings({ "unchecked", "serial" })
	protected Map<String, ContentCategory> loadCategories() {
		Map<String, ContentCategory> map = new TreeMap<String, ContentCategory>();
		
		try {
			String resourcePath = getSlideService().getURI(CATEGORY_PROPERTIES_FILE);
			WebdavRootResource rootResource = getSlideSession().getWebdavRootResource();
			InputStream in = rootResource.getMethodData(resourcePath);

			if (rootResource.getStatusCode() != WebdavStatus.SC_OK) {
				throw new FileNotFoundException("File not found " + CATEGORY_PROPERTIES_FILE);
			}
			
			SAXBuilder builder = new SAXBuilder();
			Document document = builder.build(in);
			Element root = document.getRootElement();
			Iterator<Element> cats = root.getDescendants(new Filter() {
				public boolean matches(Object obj) {
					if (obj instanceof Element) {
						Element elem = (Element) obj;
						if ("category".equals(elem.getName())) {
							return true;
						}
					}
					return false;
				}
			});
			while (cats.hasNext()) {
				Element cat = cats.next();
				ContentCategory category = new ContentCategory(cat);
				String key = category.getId();
				if (key == null || key.equals("")) {
					continue;
				}
				map.put(key, category);
			}
			
			in.close();
			rootResource.close();
		} catch (IOException e) {
			log.warn("Error loading categories: " + e.getMessage());
			return null;
		}
		catch (JDOMException e) {
			e.printStackTrace();
		}

		return map;
	}

	public void storeCategories() {
		storeCategories(false);
	}
	
	public synchronized boolean storeCategories(boolean useThread) {
		CategoriesWriter writer = null;
		try {
			writer = new CategoriesWriter(this.categories, getSlideService().getURI(CATEGORY_PROPERTIES_FILE), getSlideSession());
		} catch (RemoteException e) {
			e.printStackTrace();
			return false;
		}
		
		if (useThread) {
			writer.run();
		}
		else {
			return writer.writeCategories();
		}
		
		return true;
	}
	
	public boolean deleteCategory(String id) {
		try {
			this.categories.remove(id);
		} catch(Exception e) {
			e.printStackTrace();
			return false;
		}
		return true;
	}
	
}