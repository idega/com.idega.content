package com.idega.content.bean;

import org.jdom.Document;
import org.jdom.Element;
import org.jdom.JDOMException;
import org.jdom.input.SAXBuilder;
import org.apache.myfaces.custom.tree2.TreeNode;

import com.idega.content.TemplatesLoader;
import com.idega.content.themes.helpers.ThemesHelper;
import com.idega.content.tree.PageTemplate;
import com.idega.core.cache.IWCacheManager2;
import com.idega.core.data.IWTreeNode;
import com.idega.idegaweb.IWMainApplication;
import com.idega.webface.WFTreeNode;

import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import javax.faces.context.FacesContext;

public class SiteTemplateBean {
// TO DO change IWTreeNode to PageTreeNode
	
	TreeNode siteTree = null;
	TreeNode pageTree = null;
	String path = null;
	Document siteDocument = null;
	private Map <String, PageTemplate> pageMap = null;
	
	public SiteTemplateBean() {
		super();
		Map pageTemplatesFromCache = null;
		FacesContext ctx = FacesContext.getCurrentInstance();
		IWMainApplication iwma = IWMainApplication.getIWMainApplication(ctx);		
		pageTemplatesFromCache = IWCacheManager2.getInstance(iwma).getCache("pageMap");

		if (!pageTemplatesFromCache.containsKey("pageMap")){
			new TemplatesLoader(iwma, true);
		}
		
		pageMap = (Map <String, PageTemplate>)pageTemplatesFromCache.get("pageMap");
	}
		
	public WFTreeNode getPage(WFTreeNode currNode){

		Iterator itr = pageMap.values().iterator();		
		while(itr.hasNext()){
			PageTemplate current = (PageTemplate)itr.next();
			WFTreeNode newNode = new WFTreeNode(new IWTreeNode(current.getName()));
			newNode.setIconURI(current.getIconFile());
			newNode.setPageType(current.getType());
			newNode.setTemplateURI(current.getTemplateFile());	
			currNode.addChild(newNode);
		}
		
		return currNode;
	}
	
	public TreeNode getPageTree(){		
		WFTreeNode rootNode = new WFTreeNode(new IWTreeNode("pageTemplatesRoot"));
		rootNode = getPage(rootNode);
		return rootNode;
	}

	public String getPath() {
		return ThemesHelper.getInstance().getWebRootWithoutContent() + "/idegaweb/bundles/com.idega.content.bundle/resources/templates/site-templates.xml";
	}

	public void setPath(String path) {
		this.path = path;
	}

	public void setSiteDocument(Document siteDocument) {
		this.siteDocument = siteDocument;
	}
	
	public Document getXMLDocument(String link) {
		URL url = null;
		try {
			url = new URL(link);
		} catch (MalformedURLException e) {
			return null;
		}
		SAXBuilder builder = new SAXBuilder();
		Document document = null;
		try {
			document = builder.build(url);
		} catch (JDOMException e) {
			return null;
		} catch (IOException e) {
			return null;
		}
		return document;
	}	
}
