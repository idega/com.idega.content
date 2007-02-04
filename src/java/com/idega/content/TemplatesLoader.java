package com.idega.content;

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

import com.idega.content.themes.helpers.ThemesHelper;
import com.idega.content.tree.PageTemplate;
import com.idega.content.tree.SiteTemplateStructure;
import com.idega.core.cache.IWCacheManager2;
import com.idega.idegaweb.IWMainApplication;
import com.idega.idegaweb.IWModuleLoader;
import com.idega.idegaweb.JarLoader;

public class TemplatesLoader implements JarLoader {
	
	private IWMainApplication iwma = null;
	
	public TemplatesLoader(IWMainApplication iwma) {
		super();
		this.iwma = iwma;
	}			
	
	public TemplatesLoader(IWMainApplication iwma, boolean loadTemplatesFromConstructor) {
		super();
		this.iwma = iwma;
		if(loadTemplatesFromConstructor)
			loadSiteTemplateFilesFromBundles();
	}		
	
	public void loadSiteTemplateFilesFromBundles() {

//		iwma = bundle.getApplication();
		IWModuleLoader loader = new IWModuleLoader(iwma);		
		loader.getJarLoaders().add(this);
		loader.loadBundlesFromJars();
	}
	
	public void loadJar(File bundleJarFile, JarFile jarFile, String jarPath) {
		JarEntry pageTemplatesEntry = jarFile.getJarEntry("resources/templates/page-templates.xml");
		JarEntry siteTemplatesEntry = jarFile.getJarEntry("resources/templates/site-templates.xml");

		Map <String, PageTemplate> pageMap = null;
		Map <String, SiteTemplateStructure> siteMap = null;
		
		Map pageTemplatesFromCache = IWCacheManager2.getInstance(iwma).getCache("pageMap");
		Map siteTemplatesFromCache = IWCacheManager2.getInstance(iwma).getCache("siteMap");
		
		if (pageTemplatesEntry != null) {
			try {
				InputStream stream = jarFile.getInputStream(pageTemplatesEntry);
				Document pageDocument = ThemesHelper.getInstance(false).getXMLDocument(stream);
						
				Element root = pageDocument.getRootElement();		
				Collection siteRoot = root.getChildren();								
				Iterator itr = siteRoot.iterator();
				String pageType = null;
				if (pageTemplatesFromCache.containsKey("pageMap")){
					pageMap = (Map <String, PageTemplate>)pageTemplatesFromCache.get("pageMap");
				}
				else {			
					pageMap = new HashMap <String, PageTemplate> ();
				}				
				
				while(itr.hasNext()){
					Element current = (Element)itr.next();
					PageTemplate page = new PageTemplate();
					pageType = current.getAttributeValue("type");
					page.setName(current.getAttributeValue("name"));
					page.setType(pageType);
					page.setIconFile(current.getAttributeValue("iconfile"));
					page.setTemplateFile(current.getAttributeValue("templatefile"));
					pageMap.put(pageType, page);
				}
				pageTemplatesFromCache.put("pageMap", pageMap);				
			}
			catch (Exception e) {
				e.printStackTrace();
			}
		}
		if (siteTemplatesEntry != null) {
			try {
				InputStream stream = jarFile.getInputStream(siteTemplatesEntry);
				Document pageDocument = ThemesHelper.getInstance(false).getXMLDocument(stream);
						
				Element root = pageDocument.getRootElement();		
				
				if (siteTemplatesFromCache.containsKey("siteMap")){
					siteMap = (Map <String, SiteTemplateStructure>)siteTemplatesFromCache.get("siteMap");
				}
				else {			
					siteMap = new HashMap<String, SiteTemplateStructure>();
				}						
				siteMap = getSiteInfo(root, siteMap);
				siteTemplatesFromCache.put("siteMap", siteMap);
				
			}
			catch (Exception e) {
				e.printStackTrace();
			}
		}		
	}
	
	private Map <String, SiteTemplateStructure> getSiteInfo(Element root, Map siteMap) {
		
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
		return siteMap;
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
		templateFile = currElement.getAttributeValue("templatefile");				
		if (iconFile != null)
			currNode.setIconFile(iconFile);
		if (templateFile != null)
			currNode.setTemplateFile(templateFile);
		Iterator it = (currElement.getChildren()).iterator();
		while(it.hasNext()){
			currNode.addChild(getNode((Element)it.next()));
		}
		
		return currNode;
	}

	public IWMainApplication getIwma() {
		return iwma;
	}

	public void setIwma(IWMainApplication iwma) {
		this.iwma = iwma;
	}

}
