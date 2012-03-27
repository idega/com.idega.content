package com.idega.content.util.resources;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import com.idega.core.cache.IWCacheManager2;
import com.idega.idegaweb.IWBundle;
import com.idega.idegaweb.IWBundleStartable;
import com.idega.idegaweb.IWMainApplication;
import com.idega.util.ArrayUtil;
import com.idega.util.CoreConstants;
import com.idega.util.EventTimer;
import com.idega.util.ListUtil;
import com.idega.util.datastructures.map.MapUtil;
import com.idega.util.resources.ResourcesAdder;

public class ResourcesDaemon implements IWBundleStartable, ActionListener {

	private static final String CONCATENATED_WEB_PAGE_RESOURCES_CLEANER = "CONCATENATED_WEB_PAGE_RESOURCES_CLEANER";
	
	private EventTimer timer;
	
	public void actionPerformed(ActionEvent e) {
		if (CONCATENATED_WEB_PAGE_RESOURCES_CLEANER.equals(e.getActionCommand())) {
			IWMainApplication iwma = IWMainApplication.getDefaultIWMainApplication();
			Map<String, String> cache = IWCacheManager2.getInstance(iwma).getCache(ResourcesManagerImpl.getCachePrefix() + ResourcesManagerImpl.CONCATENATED_RESOURCES,
					IWCacheManager2.DEFAULT_CACHE_SIZE,	IWCacheManager2.DEFAULT_OVERFLOW_TO_DISK, IWCacheManager2.DEFAULT_ETERNAL, IWCacheManager2.DEFAULT_CACHE_TTL_IDLE_SECONDS,
					IWCacheManager2.DEFAULT_CACHE_TTL_SECONDS, true);
			List<String> usedResources = null;
			if (MapUtil.isEmpty(cache))
				usedResources = Collections.emptyList();
			else
				usedResources = new ArrayList<String>(cache.values());
			
			File resourcesFolder = new File(iwma.getBundle(CoreConstants.CORE_IW_BUNDLE_IDENTIFIER).getRealPath());
			if (resourcesFolder == null || !resourcesFolder.exists() || !resourcesFolder.isDirectory())
				return;
			
			String[] files = resourcesFolder.list();
			if (ArrayUtil.isEmpty(files))
				return;
			
			boolean nothingUsed = ListUtil.isEmpty(usedResources);
			String resourcesFolderRealPath = resourcesFolder.getPath();
			for (String file: files) {
				if (!file.startsWith(ResourcesAdder.OPTIMIZIED_RESOURCES))
					continue;
				
				boolean resourceIsUsed = !nothingUsed;
				String path = resourcesFolderRealPath.concat(File.separator).concat(file);
				if (resourceIsUsed) {
					boolean foundResource = false;
					for (Iterator<String> usedResourcesIter = usedResources.iterator(); (usedResourcesIter.hasNext() && !foundResource);) {
						foundResource = path.endsWith(usedResourcesIter.next());
					}
					resourceIsUsed = foundResource;
				}
				if (!resourceIsUsed) {
					File tmp = new File(path);
					if (tmp != null && tmp.exists() && tmp.isFile())
						tmp.delete();
				}
			}
		}
	}

	public void start(IWBundle starterBundle) {
		timer = new EventTimer(EventTimer.THREAD_SLEEP_5_MINUTES, CONCATENATED_WEB_PAGE_RESOURCES_CLEANER);
		timer.addActionListener(this);
		timer.start(900000);
	}

	public void stop(IWBundle starterBundle) {
		if (timer == null)
			return;
		timer.stop();
		timer = null;
	}
	
}