/*
 * $Id: CategoryBean.java,v 1.10 2009/05/15 07:23:54 valdas Exp $
 *
 * Copyright (C) 2004 Idega. All Rights Reserved.
 *
 * This software is the proprietary information of Idega.
 * Use is subject to license terms.
 *
 */
package com.idega.content.business.categories;

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
import java.util.logging.Level;
import java.util.logging.Logger;

import org.apache.commons.httpclient.HttpException;
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
import com.idega.content.themes.helpers.business.ThemesConstants;
import com.idega.idegaweb.IWMainApplication;
import com.idega.presentation.IWContext;
import com.idega.slide.business.IWSlideService;
import com.idega.slide.business.IWSlideSession;
import com.idega.slide.util.WebdavRootResource;
import com.idega.util.CoreConstants;
import com.idega.util.CoreUtil;
import com.idega.util.IOUtil;
import com.idega.util.StringHandler;

/**
 * <p>
 * Class for manipulating Categories that are stored in slide.<br/>
 * Includes functions for getting and setting all the available categories
 * </p>
 *  Last modified: $Date: 2009/05/15 07:23:54 $ by $Author: valdas $
 *
 * @author <a href="mailto:Joakim@idega.com">Joakim</a>,<a href="mailto:tryggvi@idega.com">Tryggvi Larusson</a>
 * @version $Revision: 1.10 $
 */
public class CategoryBean {
	
    private static Logger LOGGER = Logger.getLogger(CategoryBean.class.getName());
    
    private static String BEAN_KEY="ContentCategoryBean";
    
    private static final String CATEGORY_CONFIG_PATH = CoreConstants.CONTENT_PATH + CoreConstants.SLASH;
    
    /**
     * @deprecated this file will no longer be used
     */
    @Deprecated
    private static final String CATEGORY_CONFIG_FILE = CATEGORY_CONFIG_PATH + "categories.prop";
    public static final String CATEGORIES_FILE = "categories.xml";
    private static final String CATEGORY_PROPERTIES_FILE = CATEGORY_CONFIG_PATH + CATEGORIES_FILE;
    
    private IWMainApplication iwma;
    protected Map<String, ContentCategory> categories;
    
    public static final String CATEGORY_DELIMETER = CoreConstants.COMMA;

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
        private final String PROPERTY_NAME_CATEGORIES = new PropertyName("DAV","categories").toString();
        
        private HashMap<String, String> valuesToKeys;
        private IWSlideService service;
        
        protected void migrate(Collection<String> cats) {
            LOGGER.info("Migrating " + CATEGORY_CONFIG_FILE + " to new format at " + CATEGORY_PROPERTIES_FILE);
            categories = new TreeMap<String, ContentCategory>();
            valuesToKeys = new HashMap<String, String>();
            String lang = getCurrentLocale();
            for (Iterator<String> iter = cats.iterator(); iter.hasNext();) {
                String cat = iter.next();
                String key = CategoryBean.getCategoryKey(cat);
                ContentCategory category = new ContentCategory(key);
                category.addName(lang, cat);
                categories.put(key, category);
                valuesToKeys.put(cat, key);
            }
            
            storeCategories();
            
            try {
            	service = IBOLookup.getServiceInstance(IWMainApplication.getDefaultIWApplicationContext(), IWSlideService.class);
	            updateCategoriesOnFiles(CATEGORY_CONFIG_PATH);
            } catch (IBOLookupException e) {
                LOGGER.log(Level.WARNING, "Error getting EJB bean: " + IWSlideService.class, e);
            }
        }
        
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
                
                WebdavResource resource = service.getWebdavResourceAuthenticatedAsRoot(filePath);
				
                String oldCats = CATEGORY_DELIMETER;
                Enumeration<?> enumerator = resource.propfindMethod(PROPERTY_NAME_CATEGORIES);
                if (enumerator.hasMoreElements()) {
                    StringBuffer cats = new StringBuffer();
                    while(enumerator.hasMoreElements()) {
                        cats.append(enumerator.nextElement());
                    }
                    oldCats = cats.toString();
                }
                
                if (!oldCats.equals(CATEGORY_DELIMETER) && !oldCats.equals("")) {
                    LOGGER.info("Updating categories on resource " + resourcePath);
                    LOGGER.info("- " + oldCats);
                    
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
	
                    LOGGER.info("+ " + newCats.toString());
                    resource.proppatchMethod(PROPERTY_NAME_CATEGORIES, newCats.toString(), true);
                }
				
                // update categories on all child resources
                Enumeration<?> children = resource.getChildResources().getResourceNames();
                if (children.hasMoreElements()) {
                    while(children.hasMoreElements()) {
                        String child = (String) children.nextElement();
                        updateCategoriesOnFiles(child);
                    }
                }
				
                resource.close();			
            } catch (Exception e) {
                LOGGER.warning("Exception updating categories on resource " + resourcePath + ": " + e.getMessage());
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
        if (bean==null) {
            bean = new CategoryBean(iwma);
            iwma.setAttribute(BEAN_KEY, bean);
        }
        return bean;
    }

    public IWSlideService getSlideService() {
        try {
            return IBOLookup.getServiceInstance(this.iwma.getIWApplicationContext(),IWSlideService.class);
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
        Locale locale = null;
        IWContext iwContext = CoreUtil.getIWContext();
        locale = iwContext == null ? IWMainApplication.getDefaultIWMainApplication().getDefaultLocale() : iwContext.getCurrentLocale();
        locale = locale == null ? Locale.ENGLISH : locale;
        return locale.toString();
    }

    /**
     * <p>Getts all the categories as one String (comma separated)</p>
     * @return categories
     * @deprecated this file is no longer used
     */
    @Deprecated 
    public String getCategoriesAsString() {
    	IWContext iwc = CoreUtil.getIWContext();
    	if (iwc == null)
    		return null;
    	
        String categories = null;
        try {
            IWSlideSession session = IBOLookup.getSessionInstance(iwc, IWSlideSession.class);
            WebdavRootResource rootResource = session.getWebdavRootResource();
            
            String path = getSlideService().getURI(CATEGORY_CONFIG_FILE);
			
            categories = rootResource.getMethodDataAsString(path);
            if(rootResource.getStatusCode() != WebdavStatus.SC_OK){
                return "";
            }
        }
        catch (IBOLookupException e) {
            LOGGER.log(Level.WARNING, e.toString());
        }
        catch (RemoteException e) {
            LOGGER.log(Level.WARNING, e.toString());
        }
        catch (HttpException e) {
            LOGGER.log(Level.WARNING, e.toString());
        }
        catch (IOException e) {
            LOGGER.log(Level.WARNING, e.toString());
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
    public String addCategory(String category, String language) {
        if (category == null || CoreConstants.EMPTY.equals(category)) {
            return null;
        }
        String key = getCategoryKey(category);
        ContentCategory cat = this.categories.get(key);
        if (cat == null) {
            cat = new ContentCategory(key);
        }
        cat.addName(language, category);
        this.categories.put(key, cat);
        storeCategories();
        return key;
    }

    protected static final char[] LEAVE_AS_IS = {'-','0','1','2','3','4','5','6','7','8','9'};
    protected static String getCategoryKey(String category) {
        return StringHandler.stripNonRomanCharacters(category, LEAVE_AS_IS).toLowerCase();
    }
    
    /**
     * Loads category definitions from <code>categories.xml</code> file.
     * @return categories as Map<String, ContentCategory>, or <code>null</code> if loading failed.
     */
    private Map<String, ContentCategory> loadCategories() {
        Map<String, ContentCategory> map = new TreeMap<String, ContentCategory>();
        
        InputStream stream = null;
        try {
            String resourcePath = getSlideService().getURI(CATEGORY_PROPERTIES_FILE);
            stream = getSlideService().getInputStream(resourcePath);
            if (stream == null)
            	return null;
            
            SAXBuilder builder = new SAXBuilder();
            Document document = builder.build(stream);
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
                if (key == null || CoreConstants.EMPTY.equals(key)) {
                    continue;
                }
                map.put(key, category);
            }
        } catch (IOException e) {
            LOGGER.log(Level.WARNING, "Error loading categories: " + e.getMessage(), e);
            return null;
        }
        catch (JDOMException e) {
            LOGGER.log(Level.WARNING, e.toString());
        } finally {
            IOUtil.closeInputStream(stream);
        }

        return map;
    }
    
    public void storeCategories() {
        storeCategories(false);
    }
	
    public synchronized boolean storeCategories(boolean useThread) {
        CategoriesWriter writer = null;
        try {
            writer = new CategoriesWriter(this.categories, CATEGORY_PROPERTIES_FILE, getSlideService());
        } catch (Exception e) {
            LOGGER.log(Level.INFO, "Failed to create CategoriesWriter: ", e);
            return false;
        }

        if (useThread) {
            IWContext iwc = CoreUtil.getIWContext();
            if (iwc == null) {
                useThread = false;
            }
            else {
                useThread = !iwc.isIE();
            }
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
            LOGGER.log(Level.WARNING, "Failed to remove category with id: "+id+ " from categories.xml", e);
            return Boolean.FALSE;
        }
        return Boolean.TRUE;
    }
	
    public String getCategoryName(String id, String language, IWContext iwc) {		
        if (id == null) {
            return null;
        }
    	
        return getCategoryName(this.categories.get(id), language, iwc);
    }
	
    /**
     * @param category Category content.
     * @param language Category locale or default application locale: "en", "is_IS"
     * @return Category name represented in: 
     */
    public String getCategoryName(ContentCategory category, String language, IWContext iwc) {
        if (category == null || language == null || iwc == null) {
            return null;
        }
        
        String name = category.getName(language);
        if (name != null) {
            return name;
        }
        //	Didn't find category's name by current locale, looking for by default locale
        Locale defaultLocale = IWMainApplication.getIWMainApplication(iwc).getDefaultLocale();
        if (defaultLocale == null) {
            return null;
        }
        return category.getName(defaultLocale.toString());	//	Returning name by default locale or null if such doesn't exist
    }
}