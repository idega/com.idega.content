package com.idega.content.themes.business;

import java.io.File;
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

import org.jdom2.Document;
import org.jdom2.Element;
import org.springframework.beans.factory.annotation.Autowired;

import com.idega.content.business.ContentConstants;
import com.idega.content.themes.helpers.business.ThemesHelper;
import com.idega.content.tree.PageTemplate;
import com.idega.content.tree.SiteTemplate;
import com.idega.core.cache.IWCacheManager2;
import com.idega.idegaweb.IWMainApplication;
import com.idega.idegaweb.IWModuleLoader;
import com.idega.idegaweb.JarLoader;
import com.idega.repository.RepositoryService;
import com.idega.util.CoreConstants;
import com.idega.util.IOUtil;
import com.idega.util.expression.ELUtil;
import com.idega.util.xml.XmlUtil;

/**
 * An utility class to find page types and site templates from bundles and repository
 * @author Justinas,eiki
 *
 */
public class TemplatesLoader implements JarLoader {

	public static final String REPOSITORY_TEMPLATES_FOLDER = CoreConstants.CONTENT_PATH + "/templates/";
	public static final String SITE_TEMPLATES_XML_FILE_NAME = "site-templates.xml";
	public static final String PAGE_TEMPLATES_XML_FILE_NAME = "page-templates.xml";
	public static final String SITE_TEMPLATES_CACHE_KEY = "IWSiteTemplates";

	private static final String TEMPLATES_LOADER_APPLICATION_ATTIBUTE = "TemplatesLoader";

	private IWMainApplication iwma = null;

	@Autowired
	private ThemesHelper themesHelper;

	@Autowired
	private RepositoryService repositoryService;

	private TemplatesLoader(IWMainApplication iwma) {
		super();
		this.iwma = iwma;
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

	@Override
	public void loadJar(File bundleJarFile, JarFile jarFile, String jarPath) {

		JarEntry pageTemplatesEntry = jarFile.getJarEntry("resources/templates/"+PAGE_TEMPLATES_XML_FILE_NAME);
		JarEntry siteTemplatesEntry = jarFile.getJarEntry("resources/templates/"+SITE_TEMPLATES_XML_FILE_NAME);

		InputStream stream = null;
		if (pageTemplatesEntry != null) {
			try {
				stream = jarFile.getInputStream(pageTemplatesEntry);
				Document pageDocument = XmlUtil.getJDOMXMLDocument(stream);
				addPageTypesFromDocument(pageDocument);
			}
			catch (Exception e) {
				e.printStackTrace();
			} finally {
				IOUtil.closeInputStream(stream);
			}
		}

		if (siteTemplatesEntry != null) {
			try {
				stream = jarFile.getInputStream(siteTemplatesEntry);
				Document pageDocument = XmlUtil.getJDOMXMLDocument(stream);
				addSiteTemplatesFromDocument(pageDocument);
			}
			catch (Exception e) {
				e.printStackTrace();
			} finally {
				IOUtil.closeInputStream(stream);
			}
		}
	}

	/**
	 * A generic way to add page types from an xml Document
	 * @param pageDocument
	 */
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

	public void addSiteTemplatesFromDocument(Document siteTemplateDocument) {
		SortedMap<String, SiteTemplate> siteMap = null;
		Map<String, SortedMap<String, SiteTemplate>> siteTemplatesFromCache = IWCacheManager2.getInstance(iwma).getCache(SITE_TEMPLATES_CACHE_KEY);

		if (siteTemplatesFromCache.containsKey(ContentConstants.SITE_MAP_KEY)){
			siteMap = siteTemplatesFromCache.get(ContentConstants.SITE_MAP_KEY);
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
			Element structure = siteTemplate.getChildren().get(0);
			siteStruct = getNode(structure);
			siteMap.put(panelName, siteStruct);
		}

		siteTemplatesFromCache.put(ContentConstants.SITE_MAP_KEY, siteMap);
	}

	private SiteTemplate getNode(Element currElement) {
		String pageName = null;
		String pageType = null;
		String iconFile = null;
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

		for (Object child: currElement.getChildren()) {
			currNode.addChild(getNode((Element)child));
		}

		return currNode;
	}

	public Map<String, PageTemplate> getPageTemplates() {
		Map<String, Map<String, PageTemplate>> pageTemplates = IWCacheManager2.getInstance(iwma).getCache(ContentConstants.PAGE_TYPES_CACHE_KEY);

		if (!pageTemplates.containsKey(ContentConstants.PAGES_MAP_KEY)) {
		    loadTemplatesFromBundles();
		}

		return pageTemplates.get(ContentConstants.PAGES_MAP_KEY);
	}

	public SortedMap<String, SiteTemplate> getSiteTemplates() {
		Map<String, SortedMap<String, SiteTemplate>> siteTemplates = IWCacheManager2.getInstance(iwma).getCache(TemplatesLoader.SITE_TEMPLATES_CACHE_KEY);

		if (!siteTemplates.containsKey(ContentConstants.SITE_MAP_KEY)) {
		    loadTemplatesFromBundles();
		}

		return siteTemplates.get(ContentConstants.SITE_MAP_KEY);
	}

	protected String getRepositoryTemplatesFolderURI() {
		String templatesFolder = REPOSITORY_TEMPLATES_FOLDER;
		try {
			templatesFolder = getRepositoryService().getWebdavServerURL()+REPOSITORY_TEMPLATES_FOLDER;
		} catch (Exception e) {
			e.printStackTrace();
		}
		return templatesFolder;
	}

	protected RepositoryService getRepositoryService() {
		if (repositoryService == null)
			ELUtil.getInstance().autowire(this);
		return repositoryService;
	}

	public Map<String, PageTemplate> getPageMap(){
		Map<String, Map<String, PageTemplate>> pageTemplatesFromCache = IWCacheManager2.getInstance(iwma).getCache(ContentConstants.PAGE_TYPES_CACHE_KEY);
		Map<String, PageTemplate> pageMap = null;
		if (pageTemplatesFromCache.containsKey(ContentConstants.PAGES_MAP_KEY)){
			pageMap = pageTemplatesFromCache.get(ContentConstants.PAGES_MAP_KEY);
		}
		else {
			pageMap = new HashMap<String, PageTemplate>();
			pageTemplatesFromCache.put(ContentConstants.PAGES_MAP_KEY, pageMap);
		}
		return pageMap;
	}

	public ThemesHelper getThemesHelper() {
		if (themesHelper == null) {
			ELUtil.getInstance().autowire(this);
		}
		return themesHelper;
	}

	public void setThemesHelper(ThemesHelper themesHelper) {
		this.themesHelper = themesHelper;
	}

}
