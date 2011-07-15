package com.idega.content.business;

import javax.jcr.RepositoryException;
import javax.jcr.observation.Event;
import javax.jcr.observation.EventIterator;

import com.idega.idegaweb.IWCacheManager;
import com.idega.idegaweb.IWMainApplication;
import com.idega.repository.event.RepositoryEventListener;
import com.idega.util.CoreConstants;

/**
 * A IWSlide listener that listens for a the given starting path and invalidates the given Block cache key when an update to that path happens
 * @author eiki
 *
 */
public class IWCacheInvalidatorIWSlideListener implements RepositoryEventListener {

	private final String startingPath;
	private final String cacheKey;

	public IWCacheInvalidatorIWSlideListener(String startingPath, String cacheKey){
		this.startingPath = startingPath.startsWith(CoreConstants.SLASH) ?startingPath : CoreConstants.SLASH.concat(startingPath);
		this.cacheKey = cacheKey;
	}

	@Override
	public void onEvent(EventIterator events) {
		try {
			for (EventIterator evIter = events; evIter.hasNext();) {
				Event event = evIter.nextEvent();
				if (event.getPath().startsWith(this.startingPath)){
					IWCacheManager.getInstance(IWMainApplication.getDefaultIWMainApplication()).invalidateCache(cacheKey);
				}
			}
		} catch (RepositoryException e) {
			e.printStackTrace();
		}
	}
}