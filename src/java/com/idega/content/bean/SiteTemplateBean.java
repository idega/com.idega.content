package com.idega.content.bean;

import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;

import javax.faces.context.FacesContext;

import org.apache.myfaces.custom.tree2.TreeNode;
import org.jdom2.Document;
import org.jdom2.JDOMException;
import org.jdom2.input.SAXBuilder;

import com.idega.content.business.ContentUtil;
import com.idega.content.themes.business.TemplatesLoader;
import com.idega.content.themes.helpers.business.ThemesHelper;
import com.idega.content.tree.PageTemplate;
import com.idega.content.tree.TemplateComparator;
import com.idega.core.data.IWTreeNode;
import com.idega.idegaweb.IWMainApplication;
import com.idega.util.expression.ELUtil;
import com.idega.webface.WFTreeNode;

public class SiteTemplateBean {
// TO DO change IWTreeNode to PageTreeNode

	TreeNode siteTree = null;
	TreeNode pageTree = null;

	String path = null;

	Document siteDocument = null;

	private Map <String, PageTemplate> pageMap = null;

	public SiteTemplateBean() {
		super();
		FacesContext ctx = FacesContext.getCurrentInstance();
		IWMainApplication iwma = IWMainApplication.getIWMainApplication(ctx);
		pageMap = TemplatesLoader.getInstance(iwma).getPageTemplates();
	}

	public WFTreeNode getPage(WFTreeNode currNode) {
		List<PageTemplate> pageTemplates = new ArrayList<PageTemplate>(pageMap.values());
		Collections.sort(pageTemplates, new TemplateComparator());

		for (int i = 0; i < pageTemplates.size(); i++) {
			PageTemplate current = pageTemplates.get(i);
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
		return getThemesHelper().getWebRootWithoutContent() + ContentUtil.getBundle().getVirtualPathWithFileNameString("templates/site-templates.xml");
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

	public ThemesHelper getThemesHelper() {
		return ELUtil.getInstance().getBean(ThemesHelper.class);
	}
}