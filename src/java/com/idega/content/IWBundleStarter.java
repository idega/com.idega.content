/*
 * $Id: IWBundleStarter.java,v 1.33 2008/02/18 12:01:34 eiki Exp $
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
import java.util.Collection;
import java.util.Map;

import javax.ejb.CreateException;

import com.idega.block.rss.business.RSSProducerRegistry;
import com.idega.business.IBOLookup;
import com.idega.business.IBOLookupException;
import com.idega.content.business.ContentConstants;
import com.idega.content.business.ContentIWActionURIHandler;
import com.idega.content.business.ContentRSSProducer;
import com.idega.content.themes.business.TemplatesLoader;
import com.idega.content.themes.business.ThemesService;
import com.idega.content.themes.helpers.bean.Setting;
import com.idega.content.themes.helpers.business.ThemesConstants;
import com.idega.content.themes.helpers.business.ThemesHelper;
import com.idega.content.view.ContentViewManager;
import com.idega.core.accesscontrol.business.StandardRoles;
import com.idega.core.builder.business.BuilderService;
import com.idega.core.uri.IWActionURIManager;
import com.idega.idegaweb.DefaultIWBundle;
import com.idega.idegaweb.IWApplicationContext;
import com.idega.idegaweb.IWBundle;
import com.idega.idegaweb.IWBundleStartable;
import com.idega.idegaweb.IWMainApplication;
import com.idega.idegaweb.IWMainApplicationSettings;
import com.idega.idegaweb.include.GlobalIncludeManager;
import com.idega.slide.business.IWSlideService;
import com.idega.user.business.GroupBusiness;
import com.idega.user.data.Group;

/**
 * 
 *  Last modified: $Date: 2008/02/18 12:01:34 $ by $Author: eiki $
 * 
 * @author <a href="mailto:tryggvil@idega.com">Tryggvi Larusson</a>
 * @version $Revision: 1.33 $
 */
public class IWBundleStarter implements IWBundleStartable{
	
	/**
	 * 
	 */
	public IWBundleStarter() {
		super();
	}

	public void start(IWBundle starterBundle) {
		addIWActionURIHandlers();
		addRSSProducers(starterBundle);
		addContentRoleGroups(starterBundle.getApplication().getIWApplicationContext());
		
		ContentViewManager cViewManager = ContentViewManager.getInstance(starterBundle.getApplication());
		cViewManager.initializeStandardNodes(starterBundle);
		GlobalIncludeManager.getInstance().addBundleStyleSheet(ContentConstants.IW_BUNDLE_IDENTIFIER, "/style/content.css");
		
		
		IWApplicationContext iwac = starterBundle.getApplication().getIWApplicationContext();
	    try {
	    	IWSlideService service = (IWSlideService) IBOLookup.getServiceInstance(iwac,IWSlideService.class);
	        service.addIWSlideChangeListeners((ThemesService) IBOLookup.getServiceInstance(iwac, ThemesService.class));
	    } catch (Exception e) {
	    	e.printStackTrace();
	    }

	    loadThemeValues(starterBundle);
	    IWMainApplication iwmain = starterBundle.getApplication();
	    
	    TemplatesLoader templatesLoader = TemplatesLoader.getInstance(iwmain);
	    templatesLoader.loadTemplatesFromBundles();
	}

	/**
	 * Auto generate groups for the editor and author roles so we can set them in the Lucid app
	 * @param iwac 
	 */
	protected void addContentRoleGroups(IWApplicationContext iwac) {
		try {
			GroupBusiness groupBiz = (GroupBusiness) IBOLookup.getServiceInstance(iwac, GroupBusiness.class);
			BuilderService builderService = (BuilderService) IBOLookup.getServiceInstance(iwac, BuilderService.class);
			boolean clearCache = false;
			
			
			Collection<Group> editorGroups =  groupBiz.getGroupsByGroupName(StandardRoles.ROLE_KEY_EDITOR);
			Collection<Group> authorGroups =  groupBiz.getGroupsByGroupName(StandardRoles.ROLE_KEY_AUTHOR);
			
			//only generate groups if none exist
			if(editorGroups.isEmpty()){
				Group editorGroup = groupBiz.createGroup(StandardRoles.ROLE_KEY_EDITOR, "This is the system group for content editors.", groupBiz.getGroupTypeHome().getPermissionGroupTypeString(), true);
				iwac.getIWMainApplication().getAccessController().addRoleToGroup(StandardRoles.ROLE_KEY_AUTHOR,editorGroup, iwac);
				clearCache = true;
			}
			
			if(authorGroups.isEmpty()){
				Group authorGroup = groupBiz.createGroup(StandardRoles.ROLE_KEY_AUTHOR, "This is the system group for content authors.", groupBiz.getGroupTypeHome().getPermissionGroupTypeString(), true);
				iwac.getIWMainApplication().getAccessController().addRoleToGroup(StandardRoles.ROLE_KEY_AUTHOR,authorGroup, iwac);
				clearCache = true;
			}
			
			if(clearCache){
				builderService.clearAllCaches();
			}
			
		} catch (IBOLookupException e) {
			e.printStackTrace();
		} catch (RemoteException e) {
			e.printStackTrace();
		} catch (CreateException e) {
			e.printStackTrace();
		}
		
		
	}

	public void stop(IWBundle starterBundle) {
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
				String filePath = sBundlesDirectory + File.separator + ContentConstants.IW_BUNDLE_IDENTIFIER + File.separator + ThemesConstants.THEME_SETTINGS;
				stream = new FileInputStream(filePath);
			}
			else {
				stream = bundle.getResourceInputStream(ThemesConstants.THEME_SETTINGS);
			}
		} catch (IOException e) {
			e.printStackTrace();
			return;
		}
		
		ThemesHelper helper = ThemesHelper.getInstance(false);
		try {
			helper.loadThemeSettings(stream);
		} catch(Exception e) {
			e.printStackTrace();
		} finally {
			closeInputStream(stream);
		}
		
		Map <String, Setting> settings = helper.getThemeSettings();
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
	
	private void addRSSProducers(IWBundle starterBundle) {
		RSSProducerRegistry registry = RSSProducerRegistry.getInstance();
		
		//ContentRSSProducer, also a IWSlideChangeListener
		
		ContentRSSProducer contentProducer = new ContentRSSProducer();
		registry.addRSSProducer("content", contentProducer);
		
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
