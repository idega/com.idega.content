package com.idega.content.themes.business;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.SortedMap;
import java.util.TreeMap;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;

import org.jdom.Document;
import org.jdom.Element;

import com.idega.business.IBOLookup;
import com.idega.business.IBOLookupException;
import com.idega.content.business.ContentConstants;
import com.idega.content.themes.helpers.ThemesHelper;
import com.idega.content.tree.PageTemplate;
import com.idega.content.tree.SiteTemplate;
import com.idega.core.cache.IWCacheManager2;
import com.idega.core.search.business.SearchResult;
import com.idega.idegaweb.IWMainApplication;
import com.idega.idegaweb.IWModuleLoader;
import com.idega.idegaweb.JarLoader;
import com.idega.slide.business.IWSlideService;
import com.idega.util.CoreConstants;

/**
 * A utility class to find page types and site templates from bundles and slide
 * @author Justinas,eiki
 *
 */
public class TemplatesLoader implements JarLoader {
	
	public static final String SLIDE_TEMPLATES_FOLDER = CoreConstants.CONTENT_PATH + "/templates/";
	public static final String SITE_TEMPLATES_XML_FILE_NAME = "site-templates.xml";
	public static final String PAGE_TEMPLATES_XML_FILE_NAME = "page-templates.xml";
	public static final String SITE_TEMPLATES_CACHE_KEY = "IWSiteTemplates";
	
	private static final String TEMPLATES_LOADER_APPLICATION_ATTIBUTE = "TemplatesLoader";
	
	private IWMainApplication iwma = null;
	private ThemesHelper themesHelper = null;
	
	private TemplatesLoader(IWMainApplication iwma) {
		super();
		this.iwma = iwma;
		this.themesHelper = ThemesHelper.getInstance(false);
	}
	
	public static TemplatesLoader getInstance(IWMainApplication iwma){
		TemplatesLoader loader = (TemplatesLoader) iwma.getAttribute(TEMPLATES_LOADER_APPLICATION_ATTIBUTE);
		if (loader == null) {
			loader = new TemplatesLoader(iwma);
			iwma.setAttribute(TEMPLATES_LOADER_APPLICATION_ATTIBUTE, loader);
		}
		return loader;
	}
	
	public void loadTemplatesFromBundles() {
		IWModuleLoader loader = new IWModuleLoader(iwma);		
		loader.getJarLoaders().add(this);
		loader.loadBundlesFromJars();
	}
	
	public void loadJar(File bundleJarFile, JarFile jarFile, String jarPath) {

		JarEntry pageTemplatesEntry = jarFile.getJarEntry("resources/templates/"+PAGE_TEMPLATES_XML_FILE_NAME);
		JarEntry siteTemplatesEntry = jarFile.getJarEntry("resources/templates/"+SITE_TEMPLATES_XML_FILE_NAME);
		
		if (pageTemplatesEntry != null) {
			InputStream stream = null;
			try {
				stream = jarFile.getInputStream(pageTemplatesEntry);
				Document pageDocument = themesHelper.getXMLDocument(stream);
				addPageTypesFromDocument(pageDocument);				
			}
			catch (Exception e) {
				e.printStackTrace();
			} finally {
				closeInputStream(stream);
			}
		}
		
		if (siteTemplatesEntry != null) {
			InputStream stream = null;
			try {
				stream = jarFile.getInputStream(siteTemplatesEntry);
				Document pageDocument = themesHelper.getXMLDocument(stream);
				addSiteTemplatesFromDocument(pageDocument);
			}
			catch (Exception e) {
				e.printStackTrace();
			} finally {
				closeInputStream(stream);
			}
		}		
	}
	
	private void closeInputStream(InputStream is) {
		if (is == null) {
			return;
		}
		
		try {
			is.close();
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	/**
	 * A generic way to add page types from an xml Document
	 * @param pageDocument
	 */
	@SuppressWarnings("unchecked")
	public void addPageTypesFromDocument(Document pageDocument) {
		Element root = pageDocument.getRootElement();		
		Collection<Element> siteRoot = root.getChildren();
		if (siteRoot == null) {
			return;
		}
		
		String appContextUri = iwma.getApplicationContextURI();
		String iconFile = "iconfile";
		String pageType = null;
		Map<String, PageTemplate> pageMap = getPageMap();
		for (Iterator<Element> it = siteRoot.iterator(); it.hasNext();) {
			Element current = it.next();
			PageTemplate page = new PageTemplate();
			pageType = current.getAttributeValue("type");
			page.setName(current.getAttributeValue("name"));
			page.setType(pageType);
			if (current.getAttributeValue(iconFile).equals(CoreConstants.EMPTY)) {
				page.setIconFile(CoreConstants.EMPTY);
			}
			else {
				page.setIconFile(new StringBuffer(appContextUri).append(current.getAttributeValue(iconFile)).toString());
			}
			page.setTemplateFile(new StringBuffer(appContextUri).append(current.getAttributeValue("templatefile")).toString());
			pageMap.put(pageType, page);
		}
	}
	
	@SuppressWarnings("unchecked")
	public void addSiteTemplatesFromDocument(Document siteTemplateDocument) {
		SortedMap <String, SiteTemplate> siteMap = null;
		Map siteTemplatesFromCache = IWCacheManager2.getInstance(iwma).getCache(SITE_TEMPLATES_CACHE_KEY);
		
		if (siteTemplatesFromCache.containsKey(ContentConstants.SITE_MAP_KEY)){
			siteMap = (SortedMap <String, SiteTemplate>)siteTemplatesFromCache.get(ContentConstants.SITE_MAP_KEY);
		}
		else {			
			siteMap = Collections.synchronizedSortedMap(new TreeMap<String, SiteTemplate>());
		}						
		
		Element root = siteTemplateDocument.getRootElement();	
		
		Collection<Element> siteRoot = root.getChildren();
		if (siteRoot == null) {
			return;
		}
		Element siteTemplate = null;
		for(Iterator<Element> it = siteRoot.iterator(); it.hasNext();){
			SiteTemplate siteStruct = new SiteTemplate();
			siteTemplate = it.next();
			String panelName = siteTemplate.getAttributeValue("name");			
			Element structure = (Element) siteTemplate.getChildren().get(0);			
			siteStruct = getNode(structure);
			siteMap.put(panelName, siteStruct);		
		}		
		
		siteTemplatesFromCache.put(ContentConstants.SITE_MAP_KEY, siteMap);
	}
	
	private SiteTemplate getNode(Element currElement) {
		String pageName = null;	
		String pageType = null;
		String iconFile = null;
		String templateFile = null;
		SiteTemplate currNode = new SiteTemplate();
		pageType = currElement.getAttributeValue("type");
		currNode.setType(pageType);
		pageName = currElement.getAttributeValue("name");
		currNode.setName(pageName);
		iconFile = currElement.getAttributeValue("iconfile");
		if (iconFile != null){
			if (!currElement.getAttributeValue("iconfile").equals(""))
				currNode.setIconFile(iwma.getApplicationContextURI() + currElement.getAttributeValue("iconfile"));
			else
				currNode.setIconFile("");
			currNode.setIconFile(iconFile);
		}
		currNode.setTemplateFile(iwma.getApplicationContextURI() + currElement.getAttributeValue("templatefile"));		
		
		if (templateFile != null)
			currNode.setTemplateFile(templateFile);
		Iterator it = (currElement.getChildren()).iterator();
		while(it.hasNext()){
			currNode.addChild(getNode((Element)it.next()));
		}
		
		return currNode;
	}
	
	public Map<String, PageTemplate> getPageTemplates() {
		Map pageTemplates = IWCacheManager2.getInstance(iwma).getCache(ContentConstants.PAGE_TYPES_CACHE_KEY);
		
		if (!pageTemplates.containsKey(ContentConstants.PAGES_MAP_KEY)) {
		    loadTemplatesFromBundles();
		}
		
		//	Load from slide

		String templatesFolder = getSlideTemplatesFolderURI();
	
		Collection<SearchResult> results = themesHelper.search(PAGE_TEMPLATES_XML_FILE_NAME, templatesFolder);
		
		if (results == null) {
			return (Map <String, PageTemplate>)pageTemplates.get(ContentConstants.PAGES_MAP_KEY);
		}
		
		String serverURL = iwma.getIWApplicationContext().getDomain().getURL();
		Document xml = null;
		for (Iterator<SearchResult> iter = results.iterator(); iter.hasNext();) {
			xml = getTemplateDocument(iter.next(), serverURL);
			if (xml != null) {
				addPageTypesFromDocument(xml);
			}
		}
		
		return (Map <String, PageTemplate>)pageTemplates.get(ContentConstants.PAGES_MAP_KEY);
	}
	
	public SortedMap<String, SiteTemplate> getSiteTemplates() {
		Map siteTemplates = IWCacheManager2.getInstance(iwma).getCache(TemplatesLoader.SITE_TEMPLATES_CACHE_KEY);

		if (!siteTemplates.containsKey(ContentConstants.SITE_MAP_KEY)) {
		    loadTemplatesFromBundles();
		}
		
		//	Load from slide	
		String templatesFolder = getSlideTemplatesFolderURI();
	
		Collection<SearchResult> results = themesHelper.search(SITE_TEMPLATES_XML_FILE_NAME, templatesFolder);
		if (results == null) {
			return (SortedMap <String, SiteTemplate>)siteTemplates.get(ContentConstants.SITE_MAP_KEY);
		}
		
		String serverURL = iwma.getIWApplicationContext().getDomain().getURL();
		Document xml = null;
		for (Iterator<SearchResult> it = results.iterator(); it.hasNext();) {
			xml = getTemplateDocument(it.next(), serverURL);
			if (xml != null) {
				addSiteTemplatesFromDocument(xml);
			}
		}
		
		return (SortedMap <String, SiteTemplate>)siteTemplates.get(ContentConstants.SITE_MAP_KEY);
	}

	protected String getSlideTemplatesFolderURI() {
		String templatesFolder = SLIDE_TEMPLATES_FOLDER;
		try {
			templatesFolder = getIWSlideService().getWebdavServerURI()+SLIDE_TEMPLATES_FOLDER;
		} catch (Exception e) {
			e.printStackTrace();
		}
		return templatesFolder;
	}
	
	private Document getTemplateDocument(SearchResult result, String serverName) {
		if (result == null || serverName == null) {
			return null;
		}
		
		if (result.getSearchResultName().startsWith("._")) {
			return null;
		}
		
		String uri = result.getSearchResultURI();
		//	TODO fetch in authenticated manner httpclient? getmethod, or by slide api
		return themesHelper.getXMLDocument(new StringBuffer(serverName).append(uri.substring(1)).toString());
	}
	
	protected IWSlideService getIWSlideService(){
		try {
			return (IWSlideService) IBOLookup.getServiceInstance(iwma.getIWApplicationContext(), IWSlideService.class);
		} catch (IBOLookupException e) {
			e.printStackTrace();
			return null;
		}
	}
	
	public Map<String, PageTemplate> getPageMap(){
		Map pageTemplatesFromCache = IWCacheManager2.getInstance(iwma).getCache(ContentConstants.PAGE_TYPES_CACHE_KEY);
		Map<String, PageTemplate> pageMap = null;
		if (pageTemplatesFromCache.containsKey(ContentConstants.PAGES_MAP_KEY)){
			pageMap = (Map <String, PageTemplate>)pageTemplatesFromCache.get(ContentConstants.PAGES_MAP_KEY);
		}
		else {
			pageMap = new HashMap <String, PageTemplate> ();
			pageTemplatesFromCache.put(ContentConstants.PAGES_MAP_KEY, pageMap);
		}	
		return pageMap;
	}
}
