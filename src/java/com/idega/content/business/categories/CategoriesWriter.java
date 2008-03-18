package com.idega.content.business.categories;

import java.io.IOException;
import java.util.Iterator;
import java.util.Map;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.jdom.Document;
import org.jdom.Element;
import org.jdom.output.XMLOutputter;

import com.idega.content.business.ContentUtil;
import com.idega.content.data.ContentCategory;
import com.idega.io.MemoryFileBuffer;
import com.idega.io.MemoryInputStream;
import com.idega.io.MemoryOutputStream;
import com.idega.slide.business.IWSlideSession;
import com.idega.slide.util.WebdavRootResource;

public class CategoriesWriter implements Runnable {
	
	private static final Log log = LogFactory.getLog(CategoriesWriter.class);
	
	private Map<String, ContentCategory> categories = null;

	private String resourcePath = null;
	
	private IWSlideSession session = null;
	
	public CategoriesWriter(Map<String, ContentCategory> categories, String resourcePath, IWSlideSession session) {
		this.categories = categories;
		this.resourcePath = resourcePath;
		this.session = session;
	}

	public void run() {
		writeCategories();
	}
	
	protected boolean writeCategories() {
		try {
			Element root = new Element("categories");
			Iterator<String> keys = this.categories.keySet().iterator();
			while (keys.hasNext()) {
				String key = keys.next();
				ContentCategory category = this.categories.get(key);
				Element cat = category.getAsXML();
				root.addContent(cat);
			}
			Document document = new Document(root);
			
			MemoryFileBuffer buf = new MemoryFileBuffer();
			MemoryOutputStream out = new MemoryOutputStream(buf);
			XMLOutputter outputter = new XMLOutputter();
			outputter.output(document, out);
			out.close();
			
			MemoryInputStream in = new MemoryInputStream(buf);
	
			WebdavRootResource rootResource = session.getWebdavRootResource();
			boolean putOK = rootResource.putMethod(resourcePath, in);
			if (!putOK) {
				log.error("Could not store to webdav: " + rootResource.getStatusMessage());
			}
			
			in.close();
			rootResource.close();
			
			ContentUtil.removeCategoriesViewersFromCache();
		} catch (IOException e) {
			log.error("Error storing file " + resourcePath + ": " + e.getMessage());
			return false;
		}
		return true;
	}
}
