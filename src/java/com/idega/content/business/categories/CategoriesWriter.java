package com.idega.content.business.categories;

import java.io.InputStream;
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
import com.idega.repository.RepositoryService;
import com.idega.util.CoreConstants;
import com.idega.util.IOUtil;
import com.idega.util.expression.ELUtil;

public class CategoriesWriter implements Runnable {

	private static final Log log = LogFactory.getLog(CategoriesWriter.class);

	private Map<String, ContentCategory> categories = null;

	private String resourcePath = null;

	public CategoriesWriter(Map<String, ContentCategory> categories, String resourcePath) {
		this.categories = categories;
		this.resourcePath = resourcePath;
	}

	@Override
	public void run() {
		writeCategories();
	}

	protected boolean writeCategories() {
		InputStream stream = null;
		try {
			Element root = new Element("categories");
			for (String key: this.categories.keySet()) {
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

			stream = new MemoryInputStream(buf);

			String directory = resourcePath.substring(0, resourcePath.lastIndexOf(CoreConstants.SLASH) + 1);
			RepositoryService repository = ELUtil.getInstance().getBean(RepositoryService.class);
			if (repository.uploadFile(directory, CategoryBean.CATEGORIES_FILE, "text/xml", stream)) {
				ContentUtil.removeCategoriesViewersFromCache();
			}
		} catch (Exception e) {
			log.error("Error storing file " + resourcePath + ": " + e.getMessage());
			return false;
		} finally {
			IOUtil.closeInputStream(stream);
		}
		return true;
	}
}
