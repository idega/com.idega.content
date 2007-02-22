/*
 * $Id: IWBundleStarter.java,v 1.17 2007/02/22 15:13:36 justinas Exp $
 * Created on 3.11.2004
 *
 * Copyright (C) 2004 Idega Software hf. All Rights Reserved.
 *
 * This software is the proprietary information of Idega hf.
 * Use is subject to license terms.
 */
package com.idega.content;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.rmi.RemoteException;
import java.util.Map;

import com.idega.block.rss.business.RSSProducer;
import com.idega.block.rss.business.RSSProducerRegistry;
import com.idega.block.rss.data.RSSRequest;
import com.idega.business.IBOLookup;
import com.idega.business.IBOLookupException;
import com.idega.content.business.ContentIWActionURIHandler;
import com.idega.content.business.ContentItemRssProducer;
import com.idega.content.business.ContentUtil;
import com.idega.content.themes.business.ThemesService;
import com.idega.content.themes.helpers.Setting;
import com.idega.content.themes.helpers.ThemesConstants;
import com.idega.content.themes.helpers.ThemesHelper;
import com.idega.content.view.ContentViewManager;
import com.idega.content.view.SiteViewManager;
import com.idega.core.uri.IWActionURIManager;
import com.idega.idegaweb.DefaultIWBundle;
import com.idega.idegaweb.IWApplicationContext;
import com.idega.idegaweb.IWBundle;
import com.idega.idegaweb.IWBundleStartable;
import com.idega.idegaweb.IWMainApplication;
import com.idega.idegaweb.IWMainApplicationSettings;
import com.idega.idegaweb.include.GlobalIncludeManager;
import com.idega.slide.business.IWSlideService;

/**
 * 
 *  Last modified: $Date: 2007/02/22 15:13:36 $ by $Author: justinas $
 * 
 * @author <a href="mailto:tryggvil@idega.com">Tryggvi Larusson</a>
 * @version $Revision: 1.17 $
 */
//public class IWBundleStarter implements IWBundleStartable, JarLoader {
public class IWBundleStarter implements IWBundleStartable{
	
	/**
	 * 
	 */
	
	public IWBundleStarter() {
		super();
		// TODO Auto-generated constructor stub
	}

	/* (non-Javadoc)
	 * @see com.idega.idegaweb.IWBundleStartable#start(com.idega.idegaweb.IWBundle)
	 */
	public void start(IWBundle starterBundle) {
		addIWActionURIHandlers();
		
		ContentViewManager cViewManager = ContentViewManager.getInstance(starterBundle.getApplication());
		cViewManager.initializeStandardNodes(starterBundle);
		GlobalIncludeManager.getInstance().addBundleStyleSheet(ContentUtil.IW_BUNDLE_IDENTIFIER,"/style/content.css");
		
		
		IWApplicationContext iwac = starterBundle.getApplication().getIWApplicationContext();
	    try {
	    	IWSlideService service = (IWSlideService) IBOLookup.getServiceInstance(iwac,IWSlideService.class);
	           
	        ThemesService themesService = (ThemesService) IBOLookup.getServiceInstance(iwac, ThemesService.class);
	        service.addIWSlideChangeListeners(themesService);
	    } catch (IBOLookupException e) {
	    	e.printStackTrace();
	    } catch (RemoteException e) {
	    	e.printStackTrace();
	    }

	    loadThemeValues(starterBundle);
//	    loadSiteTemplateFilesFromBundles(starterBundle.getApplication());
	    IWMainApplication iwmain = starterBundle.getApplication();
	    new TemplatesLoader(iwmain, true);
		SiteViewManager sViewManager = SiteViewManager.getInstance(iwmain);
		sViewManager.initializeStandardNodes(starterBundle);
		
	    RSSProducerRegistry.getInstance().addRSSProducer("files/cms/article", ContentItemRssProducer.getInstance(iwmain));
	}

	/* (non-Javadoc)
	 * @see com.idega.idegaweb.IWBundleStartable#stop(com.idega.idegaweb.IWBundle)
	 */
	public void stop(IWBundle starterBundle) {
		// TODO Auto-generated method stub
	}
	
	/**
	 * 
	 */
	private void addIWActionURIHandlers() {
		IWActionURIManager manager = IWActionURIManager.getInstance();
		
		manager.registerHandler(new ContentIWActionURIHandler());
		
	}
	
	private void loadThemeValues(IWBundle bundle) {
		InputStream stream = null;
		try {
			String sBundlesDirectory = System.getProperty(DefaultIWBundle.SYSTEM_BUNDLES_RESOURCE_DIR);
			if (sBundlesDirectory != null) {
				String filePath = sBundlesDirectory + File.separator + ContentUtil.IW_BUNDLE_IDENTIFIER + File.separator + ThemesConstants.THEME_SETTINGS;
				stream = new FileInputStream(filePath);
			}
			else {
				stream = bundle.getResourceInputStream(ThemesConstants.THEME_SETTINGS);
			}
		} catch (IOException e) {
			e.printStackTrace();
			return;
		}
		ThemesHelper.getInstance(false).loadThemeSettings(stream);
		Map <String, Setting> settings = ThemesHelper.getInstance(false).getThemeSettings();
		if (settings == null) {
			return;
		}
		IWMainApplicationSettings applicationSettings = bundle.getApplication().getSettings();
		for (Setting setting : settings.values()) {
			String key = ThemesConstants.THEMES_PROPERTY_START + setting.getCode() + ThemesConstants.THEMES_PROPERTY_END;
			if (applicationSettings.getProperty(key) == null) { // Not overriding existing values
				applicationSettings.setProperty(key, setting.getDefaultValue());
			}
		}
	}

//	public void loadSiteTemplateFilesFromBundles(IWBundle bundle) {
//
//		iwma = bundle.getApplication();
//		IWModuleLoader loader = new IWModuleLoader(iwma);
//		loader.getJarLoaders().add(this);
//		loader.loadBundlesFromJars();
//	}

//	public void loadJar(File bundleJarFile, JarFile jarFile, String jarPath) {
//		JarEntry pageTemplatesEntry = jarFile.getJarEntry("resources/templates/page-templates.xml");
//		JarEntry siteTemplatesEntry = jarFile.getJarEntry("resources/templates/site-templates.xml");
//
//		Map <String, PageTemplate> pageMap = null;
//		Map <String, SiteTemplateStructure> siteMap = null;
//		
//		Map pageTemplatesFromCache = IWCacheManager2.getInstance(iwma).getCache("pageMap");
//		Map siteTemplatesFromCache = IWCacheManager2.getInstance(iwma).getCache("siteMap");
//		
//		if (pageTemplatesEntry != null) {
//			try {
//				InputStream stream = jarFile.getInputStream(pageTemplatesEntry);
//				Document pageDocument = ThemesHelper.getInstance(false).getXMLDocument(stream);
//						
//				Element root = pageDocument.getRootElement();		
//				Collection siteRoot = root.getChildren();								
//				Iterator itr = siteRoot.iterator();
//				String pageType = null;
//				if (pageTemplatesFromCache.containsKey("pageMap")){
//					pageMap = (Map <String, PageTemplate>)pageTemplatesFromCache.get("pageMap");
//				}
//				else {			
//					pageMap = new HashMap <String, PageTemplate> ();
//				}				
//				
//				while(itr.hasNext()){
//					Element current = (Element)itr.next();
//					PageTemplate page = new PageTemplate();
//					pageType = current.getAttributeValue("type");
//					page.setName(current.getAttributeValue("name"));
//					page.setType(pageType);
//					page.setIconFile(current.getAttributeValue("iconfile"));
//					page.setTemplateFile(current.getAttributeValue("templatefile"));
//					pageMap.put(pageType, page);
//				}
//				pageTemplatesFromCache.put("pageMap", pageMap);				
//			}
//			catch (Exception e) {
//				e.printStackTrace();
//			}
//		}
//		if (siteTemplatesEntry != null) {
//			try {
//				InputStream stream = jarFile.getInputStream(siteTemplatesEntry);
//				Document pageDocument = ThemesHelper.getInstance(false).getXMLDocument(stream);
//						
//				Element root = pageDocument.getRootElement();		
//				
//				if (siteTemplatesFromCache.containsKey("siteMap")){
//					siteMap = (Map <String, SiteTemplateStructure>)siteTemplatesFromCache.get("siteMap");
//				}
//				else {			
//					siteMap = new HashMap<String, SiteTemplateStructure>();
//				}						
//				siteMap = getSiteInfo(root, siteMap);
//				siteTemplatesFromCache.put("siteMap", siteMap);
//				
//			}
//			catch (Exception e) {
//				e.printStackTrace();
//			}
//		}		
//	}	
//	public Map <String, SiteTemplateStructure> getSiteInfo(Element root, Map siteMap) {
//		
//		Collection siteRoot = root.getChildren();			
//		Iterator itr = siteRoot.iterator();
//		
//		while(itr.hasNext()){
//			SiteTemplateStructure siteStruct = new SiteTemplateStructure();
//			Element currentSite = (Element)itr.next();
//			String panelName = currentSite.getAttributeValue("name");			
//			Element structure = (Element)currentSite.getChildren().get(0);			
//			siteStruct = getNode(structure);
//			siteMap.put(panelName, siteStruct);		
//		}		
//		return siteMap;
//	}

//	public SiteTemplateStructure getNode(Element currElement){
//		String pageName = null;	
//		String pageType = null;
//		String iconFile = null;
//		String templateFile = null;
//		SiteTemplateStructure currNode = new SiteTemplateStructure();
//		pageType = currElement.getAttributeValue("type");
//		currNode.setType(pageType);
//		pageName = currElement.getAttributeValue("name");
//		currNode.setName(pageName);
//
//		iconFile = currElement.getAttributeValue("iconfile");			
//		templateFile = currElement.getAttributeValue("templatefile");				
//		if (iconFile != null)
//			currNode.setIconFile(iconFile);
//		if (templateFile != null)
//			currNode.setTemplateFile(templateFile);
//		Iterator it = (currElement.getChildren()).iterator();
//		while(it.hasNext()){
//			currNode.addChild(getNode((Element)it.next()));
//		}
//		
//		return currNode;
//	}	
}
