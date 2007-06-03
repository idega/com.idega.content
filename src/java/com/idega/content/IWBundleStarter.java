/*
 * $Id: IWBundleStarter.java,v 1.25 2007/06/03 16:05:36 eiki Exp $
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

import com.idega.block.rss.business.RSSProducerRegistry;
import com.idega.business.IBOLookup;
import com.idega.business.IBOLookupException;
import com.idega.content.business.ContentIWActionURIHandler;
import com.idega.content.business.ContentRSSProducer;
import com.idega.content.business.ContentUtil;
import com.idega.content.themes.business.TemplatesLoader;
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
 *  Last modified: $Date: 2007/06/03 16:05:36 $ by $Author: eiki $
 * 
 * @author <a href="mailto:tryggvil@idega.com">Tryggvi Larusson</a>
 * @version $Revision: 1.25 $
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
		addRSSProducers(starterBundle);
		
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
	    
	    TemplatesLoader templatesLoader = TemplatesLoader.getInstance(iwmain);
	    templatesLoader.loadTemplatesFromBundles();
	
	    SiteViewManager sViewManager = SiteViewManager.getInstance(iwmain);
		sViewManager.initializeStandardNodes(starterBundle);
		
//	    RSSProducerRegistry.getInstance().addRSSProducer("files/cms/article", ContentItemRssProducer.getInstance(iwmain));
//	    RSSProducerRegistry.getInstance().addRSSProducer("files/cms/article", new ContentItemRssProducer());
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
	
	private void addRSSProducers(IWBundle starterBundle) {
		RSSProducerRegistry registry = RSSProducerRegistry.getInstance();
		
		//ContentRSSProducer, also a IWSlideChangeListener
		
		ContentRSSProducer contentProducer = new ContentRSSProducer();
		registry.addRSSProducer("content", contentProducer);
		
//	    RSSProducerRegistry.getInstance().addRSSProducer("files/cms/article", new ContentItemRssProducer());
		
		
		 IWApplicationContext iwac = starterBundle.getApplication().getIWApplicationContext();
	        try {
	            IWSlideService service = (IWSlideService) IBOLookup.getServiceInstance(iwac,IWSlideService.class);
	            service.addIWSlideChangeListeners(contentProducer);
	            
	        } catch (IBOLookupException e) {
	            e.printStackTrace();
	        } catch (RemoteException e) {
	            e.printStackTrace();
	        }
	}
}
