package com.idega.content.themes.business;

import java.io.File;
import java.io.InputStream;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;

import org.jdom.Document;
import org.jdom.Element;

import com.idega.business.IBOLookup;
import com.idega.business.IBOLookupException;
import com.idega.content.business.ContentConstants;
import com.idega.content.themes.helpers.ThemesHelper;
import com.idega.content.tree.PageTemplate;
import com.idega.content.tree.SiteTemplateStructure;
import com.idega.core.cache.IWCacheManager2;
import com.idega.core.search.business.SearchResult;
import com.idega.idegaweb.IWMainApplication;
import com.idega.idegaweb.IWModuleLoader;
import com.idega.idegaweb.JarLoader;
import com.idega.slide.business.IWSlideService;

/**
 * A utility class to find page types and site templates from bundles and slide
 * @author Justinas,eiki
 *
 */
public class TemplatesLoader implements JarLoader {
	
	public static final String SLIDE_TEMPLATES_FOLDER = "files/cms/templates/";

	protected static final String TEMPLATES_LOADER_APPLICATION_ATTIBUTE = "TemplatesLoader";

	private IWMainApplication iwma = null;
	
	public static final String SITE_TEMPLATES_XML_FILE_NAME = "site-templates.xml";
	public static final String PAGE_TEMPLATES_XML_FILE_NAME = "page-templates.xml";
	
	public static final String SITE_TEMPLATES_CACHE_KEY = "IWSiteTemplates";
	

	private ThemesHelper themesHelper;
	
	protected TemplatesLoader(IWMainApplication iwma) {
		super();
		this.iwma = iwma;
		themesHelper = ThemesHelper.getInstance(false);
	}
	
	public static TemplatesLoader getInstance(IWMainApplication iwma){
		TemplatesLoader loader = (TemplatesLoader) iwma.getAttribute(TEMPLATES_LOADER_APPLICATION_ATTIBUTE);
		if(loader==null){
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
		
//		pageTemplatesEntry.getAttributes().g
		if (pageTemplatesEntry != null) {
			try {
				InputStream stream = jarFile.getInputStream(pageTemplatesEntry);
				Document pageDocument = themesHelper.getXMLDocument(stream);
				addPageTypesFromDocument(pageDocument);				
			}
			catch (Exception e) {
				e.printStackTrace();
			}
		}
		if (siteTemplatesEntry != null) {

			try {
				InputStream stream = jarFile.getInputStream(siteTemplatesEntry);
				Document pageDocument = themesHelper.getXMLDocument(stream);
				addSiteTemplatesFromDocument(pageDocument);
			}
			catch (Exception e) {
				e.printStackTrace();
			}
		}		
	}

	/**
	 * A generic way to add page types from an xml Document
	 * @param pageDocument
	 */
	public void addPageTypesFromDocument(Document pageDocument) {
		Map pageTemplatesFromCache = IWCacheManager2.getInstance(iwma).getCache(ContentConstants.PAGE_TYPES_CACHE_KEY);
		
		Map<String, PageTemplate> pageMap;
		Element root = pageDocument.getRootElement();		
		Collection siteRoot = root.getChildren();								
		Iterator itr = siteRoot.iterator();
		String pageType = null;
		
		if (pageTemplatesFromCache.containsKey(ContentConstants.PAGES_MAP_KEY)){
			pageMap = (Map <String, PageTemplate>)pageTemplatesFromCache.get(ContentConstants.PAGES_MAP_KEY);
		}
		else {
			pageMap = new HashMap <String, PageTemplate> ();
			pageTemplatesFromCache.put(ContentConstants.PAGES_MAP_KEY, pageMap);
		}				
//		IWContext iwc = null;
//		iwc.get
//		System.out.println("context_url: "+iwma.get);
//		iwma.getApplicationSpecialVirtualPath();
		
		 
		while(itr.hasNext()){
			Element current = (Element)itr.next();
			PageTemplate page = new PageTemplate();
			pageType = current.getAttributeValue("type");
			page.setName(current.getAttributeValue("name"));
			page.setType(pageType);
			if (!current.getAttributeValue("iconfile").equals(""))
				page.setIconFile(iwma.getApplicationContextURI() + current.getAttributeValue("iconfile"));
			else
				page.setIconFile("");
			page.setTemplateFile(iwma.getApplicationContextURI() + current.getAttributeValue("templatefile"));
			pageMap.put(pageType, page);
		}
	}
	
	public void addSiteTemplatesFromDocument(Document siteTemplateDocument) {
		Map <String, SiteTemplateStructure> siteMap;
		Map siteTemplatesFromCache = IWCacheManager2.getInstance(iwma).getCache(SITE_TEMPLATES_CACHE_KEY);
		
		if (siteTemplatesFromCache.containsKey(ContentConstants.SITE_MAP_KEY)){
			siteMap = (Map <String, SiteTemplateStructure>)siteTemplatesFromCache.get(ContentConstants.SITE_MAP_KEY);
		}
		else {			
			siteMap = new HashMap<String, SiteTemplateStructure>();
		}						
		
		Element root = siteTemplateDocument.getRootElement();	
		
		Collection siteRoot = root.getChildren();			
		Iterator itr = siteRoot.iterator();
		
		while(itr.hasNext()){
			SiteTemplateStructure siteStruct = new SiteTemplateStructure();
			Element currentSite = (Element)itr.next();
			String panelName = currentSite.getAttributeValue("name");			
			Element structure = (Element)currentSite.getChildren().get(0);			
			siteStruct = getNode(structure);
			siteMap.put(panelName, siteStruct);		
		}		
		
		siteTemplatesFromCache.put(ContentConstants.SITE_MAP_KEY, siteMap);
	}	
	private SiteTemplateStructure getNode(Element currElement){
		String pageName = null;	
		String pageType = null;
		String iconFile = null;
		String templateFile = null;
		SiteTemplateStructure currNode = new SiteTemplateStructure();
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
		
		
//		iconFile = currElement.getAttributeValue("iconfile");			
//		templateFile = currElement.getAttributeValue("templatefile");				
//		if (iconFile != null)
//			currNode.setIconFile(iconFile);
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
		ThemesHelper helper = ThemesHelper.getInstance(false);
		
		
		if (!pageTemplates.containsKey(ContentConstants.PAGES_MAP_KEY)) {
		    loadTemplatesFromBundles();
		}
		
		//load from slide
		Collection results = helper.search(PAGE_TEMPLATES_XML_FILE_NAME, SLIDE_TEMPLATES_FOLDER);
		for (Iterator iter = results.iterator(); iter.hasNext();) {
			SearchResult element = (SearchResult) iter.next();
			//don't read hidden files!
			if(!element.getSearchResultName().startsWith("._")){
				String uri = element.getSearchResultURI();
				//TODO fetch in authenticated manner httpclient? getmethod, or by slide api
				String serverURL = iwma.getIWApplicationContext().getDomain().getURL();
				String url = serverURL+uri.substring(1);//get rid of the starting /
				addPageTypesFromDocument(helper.getXMLDocument(url));
			}
		}
		
		return (Map <String, PageTemplate>)pageTemplates.get(ContentConstants.PAGES_MAP_KEY);
	}
	
	public Map<String, SiteTemplateStructure> getSiteTemplates() {
		ThemesHelper helper = ThemesHelper.getInstance(false);
		Map siteTemplates = IWCacheManager2.getInstance(iwma).getCache(TemplatesLoader.SITE_TEMPLATES_CACHE_KEY);

		if (!siteTemplates.containsKey(ContentConstants.SITE_MAP_KEY)) {
		    loadTemplatesFromBundles();
		}
		

//		load from slide
		Collection results = helper.search(SITE_TEMPLATES_XML_FILE_NAME, SLIDE_TEMPLATES_FOLDER);
		for (Iterator iter = results.iterator(); iter.hasNext();) {
			SearchResult element = (SearchResult) iter.next();
			//don't read hidden files!
			if(!element.getSearchResultName().startsWith("._")){
				String uri = element.getSearchResultURI();
				//TODO fetch in authenticated manner httpclient? getmethod, or by slide api
				String serverURL = iwma.getIWApplicationContext().getDomain().getURL();
				String url = serverURL+uri.substring(1);//get rid of the starting /
				addSiteTemplatesFromDocument(helper.getXMLDocument(url));
			}
		}
		
		return (Map <String, SiteTemplateStructure>)siteTemplates.get(ContentConstants.SITE_MAP_KEY);
	}
	
	protected IWSlideService getIWSlideService() throws IBOLookupException{
		return (IWSlideService) IBOLookup.getServiceInstance(iwma.getIWApplicationContext(), IWSlideService.class);
	}
	
}
