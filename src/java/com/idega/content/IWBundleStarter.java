/*
 * $Id: IWBundleStarter.java,v 1.49 2009/06/12 10:52:36 valdas Exp $
 * Created on 3.11.2004
 *
 * Copyright (C) 2004 Idega Software hf. All Rights Reserved.
 *
 * This software is the proprietary information of Idega hf.
 * Use is subject to license terms.
 */
package com.idega.content;

import java.io.FileInputStream;
import java.io.InputStream;
import java.rmi.RemoteException;
import java.util.Collection;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;

import com.idega.block.rss.business.RSSProducerRegistry;
import com.idega.builder.business.BuilderLogicWrapper;
import com.idega.business.IBOLookup;
import com.idega.business.IBOLookupException;
import com.idega.content.business.ContentIWActionURIHandler;
import com.idega.content.business.ContentRSSProducer;
import com.idega.content.themes.business.TemplatesLoader;
import com.idega.content.themes.business.ThemesService;
import com.idega.content.themes.helpers.bean.Setting;
import com.idega.content.themes.helpers.business.ThemesConstants;
import com.idega.content.themes.helpers.business.ThemesHelper;
import com.idega.content.view.ContentViewManager;
import com.idega.core.accesscontrol.business.StandardRoles;
import com.idega.core.uri.IWActionURIManager;
import com.idega.idegaweb.IWApplicationContext;
import com.idega.idegaweb.IWBundle;
import com.idega.idegaweb.IWBundleStartable;
import com.idega.idegaweb.IWMainApplication;
import com.idega.idegaweb.IWMainApplicationSettings;
import com.idega.servlet.filter.IWBundleResourceFilter;
import com.idega.slide.business.IWSlideService;
import com.idega.user.business.GroupBusiness;
import com.idega.user.data.Group;
import com.idega.util.IOUtil;
import com.idega.util.ListUtil;
import com.idega.util.expression.ELUtil;

/**
 * 
 *  Last modified: $Date: 2009/06/12 10:52:36 $ by $Author: valdas $
 * 
 * @author <a href="mailto:tryggvil@idega.com">Tryggvi Larusson</a>
 * @version $Revision: 1.49 $
 */
public class IWBundleStarter implements IWBundleStartable{
	
	@Autowired
	private ThemesHelper themesHelper;
	
	/**
	 * 
	 */
	public IWBundleStarter() {
		super();
	}

	public void start(IWBundle starterBundle) {
		ThemesConstants.initializeThemeConstants();
		
		addIWActionURIHandlers();
		addRSSProducers(starterBundle);
		addContentRoleGroups(starterBundle.getApplication().getIWApplicationContext());
		
		ContentViewManager cViewManager = ContentViewManager.getInstance(starterBundle.getApplication());
		cViewManager.initializeStandardNodes(starterBundle);
		
		IWApplicationContext iwac = starterBundle.getApplication().getIWApplicationContext();
	    try {
	    	IWSlideService service = IBOLookup.getServiceInstance(iwac,IWSlideService.class);
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
		boolean clearCache = false;
		try {
			GroupBusiness groupBiz = IBOLookup.getServiceInstance(iwac, GroupBusiness.class);
			
			@SuppressWarnings("unchecked")
			Collection<Group> editorGroups = groupBiz.getGroupsByGroupName(StandardRoles.ROLE_KEY_EDITOR);
			@SuppressWarnings("unchecked")
			Collection<Group> authorGroups = groupBiz.getGroupsByGroupName(StandardRoles.ROLE_KEY_AUTHOR);
			
			//	Only generate groups if none exist
			if (editorGroups == null || editorGroups.isEmpty()){
				Group editorGroup = groupBiz.createGroup(StandardRoles.ROLE_KEY_EDITOR, "This is the system group for content editors.", groupBiz.getGroupTypeHome().getPermissionGroupTypeString(), true);
				iwac.getIWMainApplication().getAccessController().addRoleToGroup(StandardRoles.ROLE_KEY_EDITOR,editorGroup, iwac);
				clearCache = true;
			}
			
			if (authorGroups == null || authorGroups.isEmpty()) {
				Group authorGroup = groupBiz.createGroup(StandardRoles.ROLE_KEY_AUTHOR, "This is the system group for content authors.", groupBiz.getGroupTypeHome().getPermissionGroupTypeString(), true);
				iwac.getIWMainApplication().getAccessController().addRoleToGroup(StandardRoles.ROLE_KEY_AUTHOR,authorGroup, iwac);
				clearCache = true;
			}
			
			if (clearCache) {
				BuilderLogicWrapper builderLogic = ELUtil.getInstance().getBean(BuilderLogicWrapper.SPRING_BEAN_NAME_BUILDER_LOGIC_WRAPPER);
				builderLogic.reloadGroupsInCachedDomain(iwac, null);
			}
		} catch(Exception e) {
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
		IWMainApplication app = bundle.getApplication();
		try {
			stream = new FileInputStream(IWBundleResourceFilter.copyResourceFromJarToWebapp(app, bundle.getResourcesPath() + "/themes/theme.xml"));
		} catch(Exception e) {
			e.printStackTrace();
		}
		if (stream == null) {
			return;
		}
		
		if (themesHelper == null) {
			ELUtil.getInstance().autowire(this);
		}
		try {
			themesHelper.loadThemeSettings(stream);
		} catch(Exception e) {
			e.printStackTrace();
		} finally {
			IOUtil.closeInputStream(stream);
		}
		
		List<Setting> settings = themesHelper.getThemeSettings();
		if (ListUtil.isEmpty(settings)) {
			return;
		}
		IWMainApplicationSettings applicationSettings = bundle.getApplication().getSettings();
		for (Setting setting : settings) {
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
		
		 IWApplicationContext iwac = starterBundle.getApplication().getIWApplicationContext();
	        try {
	            IWSlideService service = IBOLookup.getServiceInstance(iwac,IWSlideService.class);
	            service.addIWSlideChangeListeners(contentProducer);
	            
	        } catch (IBOLookupException e) {
	            e.printStackTrace();
	        } catch (RemoteException e) {
	            e.printStackTrace();
	        }
	}
}
