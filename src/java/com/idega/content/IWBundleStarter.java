/*
 * $Id: IWBundleStarter.java,v 1.7 2006/11/09 07:57:51 valdas Exp $
 * Created on 3.11.2004
 *
 * Copyright (C) 2004 Idega Software hf. All Rights Reserved.
 *
 * This software is the proprietary information of Idega hf.
 * Use is subject to license terms.
 */
package com.idega.content;

import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.rmi.RemoteException;
import java.util.Iterator;
import java.util.Map;

import org.chiba.xml.xslt.impl.ResourceResolver;

import com.idega.block.form.business.BundleResourceResolver;
import com.idega.business.IBOLookup;
import com.idega.business.IBOLookupException;
import com.idega.content.business.ContentIWActionURIHandler;
import com.idega.content.business.ContentUtil;
import com.idega.content.themes.business.ThemesService;
import com.idega.content.themes.helpers.ThemeSettings;
import com.idega.content.themes.helpers.ThemesConstants;
import com.idega.content.themes.helpers.ThemesHelper;
import com.idega.content.view.ContentViewManager;
import com.idega.core.uri.IWActionURIManager;
import com.idega.idegaweb.IWApplicationContext;
import com.idega.idegaweb.IWBundle;
import com.idega.idegaweb.IWBundleStartable;
import com.idega.idegaweb.IWMainApplication;
import com.idega.idegaweb.IWMainApplicationSettings;
import com.idega.idegaweb.include.GlobalIncludeManager;
import com.idega.slide.business.IWSlideService;


/**
 * 
 *  Last modified: $Date: 2006/11/09 07:57:51 $ by $Author: valdas $
 * 
 * @author <a href="mailto:tryggvil@idega.com">Tryggvi Larusson</a>
 * @version $Revision: 1.7 $
 */
public class IWBundleStarter implements IWBundleStartable {
	
	private static final URI SETTINGS = URI.create("bundle://" + ContentUtil.IW_BUNDLE_IDENTIFIER + ThemesConstants.THEME_SETTINGS);

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

	    loadThemeValues(starterBundle.getApplication());
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
	
	private void loadThemeValues(IWMainApplication application) {
		ResourceResolver resolver = new BundleResourceResolver(application);
		InputStream stream = null;
		try {
			stream = resolver.resolve(SETTINGS).getInputStream();
		} catch (IOException e) {
			e.printStackTrace();
			return;
		}
		ThemesHelper.getInstance(false).loadThemeSettings(stream);
		Map <String, ThemeSettings> settings = ThemesHelper.getInstance(false).getSettings();
		if (settings == null) {
			return;
		}
		Iterator<ThemeSettings> it = settings.values().iterator();
		ThemeSettings setting = null;
		IWMainApplicationSettings applicationSettings  = application.getSettings();
		while (it.hasNext()) {
			setting = it.next();
			applicationSettings.setProperty(ThemesConstants.THEMES_PROPERTY_START + setting.getCode() +
					ThemesConstants.THEMES_PROPERTY_END, setting.getDefaultValue());
		}
	}
}
