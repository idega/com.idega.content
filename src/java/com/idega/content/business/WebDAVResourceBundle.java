package com.idega.content.business;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.util.Enumeration;
import java.util.Map;
import java.util.Properties;
import java.util.ResourceBundle;
import java.util.Set;

import org.apache.webdav.lib.util.WebdavStatus;

import com.idega.business.IBOLookup;
import com.idega.business.IBOLookupException;
import com.idega.io.MemoryFileBuffer;
import com.idega.io.MemoryInputStream;
import com.idega.io.MemoryOutputStream;
import com.idega.presentation.IWContext;
import com.idega.slide.business.IWSlideService;
import com.idega.slide.business.IWSlideSession;
import com.idega.slide.util.WebdavRootResource;
import com.idega.util.SortedProperties;

public class WebDAVResourceBundle extends ResourceBundle {

	private Properties props;

	private String path;

	public WebDAVResourceBundle(String path) {
		super();
		this.path = path;
		this.props = new SortedProperties();
		load();
	}

	public Enumeration getKeys() {
		return props.keys();
	}

	public Set keySet() {
		return props.keySet();
	}

	protected Object handleGetObject(String key) {
		return props.get(key);
	}

	protected void load() {
		try {
			String resourcePath = getSlideService().getURI(this.path);
			WebdavRootResource rootResource = getSlideSession().getWebdavRootResource();
			InputStream in = rootResource.getMethodData(resourcePath);

			if (rootResource.getStatusCode() != WebdavStatus.SC_OK) {
				System.err.println("Could not load from webdav: " + rootResource.getStatusMessage());
				throw new FileNotFoundException("Resource bundle not found " + resourcePath);
			}

			props.load(in);
		} catch (IOException e) {
			System.err.println("Error loading resource bundle " + this.path + ": " + e.getMessage());
		}
	}

	public synchronized void store() {
		try {
			props.list(System.out);

			MemoryFileBuffer buf = new MemoryFileBuffer();
			MemoryOutputStream out = new MemoryOutputStream(buf);
			props.store(out, null);
			out.close();
			
			String resourcePath = getSlideService().getURI(this.path);
			MemoryInputStream in = new MemoryInputStream(buf);
			System.out.println("Writing file to " + resourcePath);

			WebdavRootResource rootResource = getSlideSession().getWebdavRootResource();
			boolean putOK = rootResource.putMethod(resourcePath, in);
			if (!putOK) {
				System.err.println("Could not store to webdav: " + rootResource.getStatusMessage());
			}
			in.close();
		} catch (IOException e) {
			System.err.println("Error storing resource bundle " + this.path + ": " + e.getMessage());
		}
	}

	public void put(Object key, Object o) {
		props.put(key, o);
		store();
	}

	public void putAll(Map map) {
		props.putAll(map);
		store();
	}

	private static IWSlideSession getSlideSession() {
		try {
			IWContext iwc = IWContext.getInstance();
			IWSlideSession session = (IWSlideSession)IBOLookup.getSessionInstance(iwc,IWSlideSession.class);
			return session;
		} catch (IBOLookupException e) {
			throw new RuntimeException("Error getting IWSlideSession");
		}
	}

	private static IWSlideService getSlideService() {
		try {
			IWContext iwc = IWContext.getInstance();
			IWSlideService service = (IWSlideService)IBOLookup.getServiceInstance(iwc,IWSlideService.class);
			return service;
		} catch (IBOLookupException e) {
			throw new RuntimeException("Error getting IWSlideService");
		}
	}

}
