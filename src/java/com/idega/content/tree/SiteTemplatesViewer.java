package com.idega.content.tree;

import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import javax.faces.component.html.HtmlOutputLink;
import javax.faces.component.html.HtmlOutputText;
import javax.faces.context.FacesContext;

import org.apache.myfaces.custom.tree2.TreeNode;
import org.jdom.Document;
import org.jdom.Element;
import org.jdom.JDOMException;
import org.jdom.input.SAXBuilder;

import com.idega.block.web2.presentation.Accordion;
import com.idega.content.themes.helpers.Theme;
import com.idega.content.themes.helpers.ThemesHelper;
import com.idega.core.cache.IWCacheManager2;
import com.idega.core.data.ICTreeNode;
import com.idega.core.data.IWTreeNode;
import com.idega.idegaweb.IWMainApplication;
import com.idega.presentation.IWBaseComponent;
import com.idega.presentation.text.Text;
import com.idega.webface.IWTree;
import com.idega.webface.WFTreeNode;
import com.idega.webface.WFUtil;

public class SiteTemplatesViewer extends IWBaseComponent {

//	private static final String SITE_LINK = ThemesHelper.getInstance().getWebRootWithoutContent()+"/idegaweb/bundles/com.idega.content.bundle/resources/templates/site-templates.xml";
//	private static final String PAGE_LINK = ThemesHelper.getInstance().getWebRootWithoutContent()+"/idegaweb/bundles/com.idega.content.bundle/resources/templates/page-templates.xml";	
	
	private Map <String, PageTemplate> pageMap = null;
	private Map <String, SiteTemplateStructure> siteMap = null;
	
	public SiteTemplatesViewer() {
		super();
		// TODO Auto-generated constructor stub
		Map pageTemplatesFromCache = null;
		Map siteTemplatesFromCache = null;
		FacesContext ctx = FacesContext.getCurrentInstance();
		IWMainApplication iwma = IWMainApplication.getIWMainApplication(ctx);
		
		pageTemplatesFromCache = IWCacheManager2.getInstance(iwma).getCache("pageMap");
		siteTemplatesFromCache = IWCacheManager2.getInstance(iwma).getCache("siteMap");
//		if (pageTemplatesFromCache.isEmpty()){
//			pageMap = getPageInfo(new HashMap <String, PageTemplate> ());
//			pageTemplatesFromCache.put("pageMap", pageMap);
//		}
//		else {
//			pageMap = (Map <String, PageTemplate>)pageTemplatesFromCache.get("pageMap");
//		}			
		
		
		
//		if (pageTemplatesFromCache.containsKey("pageMap")){
////System.out.println("******** GET PAGEMAP from cache");
//			pageMap = (Map <String, PageTemplate>)pageTemplatesFromCache.get("pageMap");
//		}
//		else {
////System.out.println("******** Read PAGEMAP from XML");			
//			pageMap = getPageInfo(new HashMap <String, PageTemplate> ());
//			pageTemplatesFromCache.put("pageMap", pageMap);
//		}

		pageMap = (Map <String, PageTemplate>)pageTemplatesFromCache.get("pageMap");
		
//		if (siteTemplatesFromCache.containsKey("siteMap")){
////System.out.println("******** GET SITEMAP from cache");
//			siteMap = (Map <String, SiteTemplateStructure>)siteTemplatesFromCache.get("siteMap");
////			siteMap = reverseMap(siteMap);
//		}
//		else {
////System.out.println("******** Read SITEMAP from XML");			
//			siteMap = getSiteInfo();
////			siteMap = reverseMap(siteMap);
//			siteTemplatesFromCache.put("siteMap", siteMap);
//		}

		siteMap = (Map <String, SiteTemplateStructure>)siteTemplatesFromCache.get("siteMap");
		
//		System.out.println("******** Read PAGEMAP from XML");			
//		pageMap = getPageInfo(new HashMap <String, PageTemplate> ());
//		pageTemplatesFromCache.put("pageMap", pageMap);
//		
//		System.out.println("******** Read SITEMAP from XML");			
//		siteMap = getSiteInfo();
//		pageTemplatesFromCache.put("siteMap", siteMap);
		
		
		
		
	}
//	protected void initializeComponent(FacesContext context) {	
//		
//		Document siteDocument = getXMLDocument(SITE_LINK);		
//		Element root = siteDocument.getRootElement();		
//		Collection siteRoot = root.getChildren();			
//		Iterator itr = siteRoot.iterator();
//		Accordion acc = new Accordion("site_templates");
//		getChildren().add(acc);
//		acc.setHeight("240");
//		int panelID = 0;
//			
////		pageMap = setPageInfo(new HashMap <String, PageTemplate> ());
//		while(itr.hasNext()){
//			panelID++;
//			Element currentSite = (Element)itr.next();
//			String panelName = currentSite.getAttributeValue("name");
//			Element structure = (Element)currentSite.getChildren().get(0);
//			WFTreeNode rootNode = new WFTreeNode(new IWTreeNode(structure.getAttributeValue("name")));
//
////			rootNode = getRootNode(currentSite);
//			
//			rootNode = getPage(pageMap, structure, rootNode);
//			
//			IWTree tree = new IWTree();
//			
//			tree.setValue(rootNode);
//			
//		    tree.setShowRootNode(false);	
//		    tree.setId("tree"+panelID);
//		    tree.setShowLines(false);
//		    tree.setVar("node");
//		    
//		    tree.setRendererType("com.idega.webface.IWTree");
//		    	
//		    HtmlOutputLink linki = new HtmlOutputLink();
//		    linki.setValue("#");
//		    linki.getAttributes().put("iconURI", "testValue");
//		        
//		    HtmlOutputText texti = new HtmlOutputText();		    
//		    texti.setValueBinding("value",WFUtil.createValueBinding("#{node.description}"));
//		    
//		    linki.getChildren().add(texti);		    
//		    tree.getFacets().put("IWTreeNode",linki);
//		    tree.getAttributes().put("sourceTree", "true");
//
//		    acc.addPanel("panel"+panelID, new Text(panelName), tree); 
//		    
//			StringBuffer b = new StringBuffer();
//			b.append("<script> \n")
//			.append("\ttreeObj"+panelID+" = new JSDragDropTree();\n")
//			.append("\ttreeObj"+panelID+".setTreeId('tree"+panelID+"');\n")
//			.append("\ttreeObj"+panelID+".initTree();\n")			
//			.append("\ttreeObj"+panelID+".expandAll();\n")
//			.append("</script>\n");
//			this.getChildren().add(new Text(b.toString()));		
//		}
//				
//	}
	
	protected void initializeComponent(FacesContext context) {	
		Iterator itrKeySet = siteMap.keySet().iterator();
		String mapKey = null;
		Accordion acc = new Accordion("site_templates");
		getChildren().add(acc);
		acc.setHeight("240");
		int panelID = 0;

		while(itrKeySet.hasNext()){
			panelID++;
			mapKey = itrKeySet.next().toString();
//System.out.println("mapKey "+mapKey);			
			SiteTemplateStructure currentSite = siteMap.get(mapKey);
			String panelName = mapKey;
//System.out.println("panelName "+panelName);			
			WFTreeNode rootNode = new WFTreeNode(new IWTreeNode(panelName));
			
//			rootNode = getPage(structure, rootNode);
			
			rootNode = getPage(currentSite, rootNode);			
			
			IWTree tree = new IWTree();
			
			tree.setValue(rootNode);
			
		    tree.setShowRootNode(false);	
		    tree.setId("tree"+panelID);
		    tree.setShowLines(false);
		    tree.setVar("node");
		    
		    tree.setRendererType("com.idega.webface.IWTree");
		    	
		    HtmlOutputLink linki = new HtmlOutputLink();
		    linki.setValue("#");
		    linki.getAttributes().put("iconURI", "testValue");
		        
		    HtmlOutputText texti = new HtmlOutputText();		    
		    texti.setValueBinding("value",WFUtil.createValueBinding("#{node.description}"));
		    
		    linki.getChildren().add(texti);		    
		    tree.getFacets().put("IWTreeNode",linki);
		    tree.getAttributes().put("sourceTree", "true");

		    acc.addPanel("panel"+panelID, new Text(panelName), tree); 
		    
			StringBuffer b = new StringBuffer();
			b.append("<script> \n")
			.append("\ttreeObj"+panelID+" = new JSDragDropTree();\n")
			.append("\ttreeObj"+panelID+".setTreeId('tree"+panelID+"');\n")
			.append("\ttreeObj"+panelID+".initTree();\n")			
			.append("\ttreeObj"+panelID+".expandAll();\n")
			.append("</script>\n");
			this.getChildren().add(new Text(b.toString()));		
		}
				
	}	
	
	public TreeNode getTree(IWTreeNode rootNode){
		ICTreeNode icnode = rootNode;
		return new WFTreeNode(icnode);
	}
	
//	public WFTreeNode getPage(Map <String, PageTemplate> pageMap, Element currElement, WFTreeNode currNode){
//		Iterator itr = (currElement.getChildren()).iterator();
//		String pageType = null;
//		String iconFile = null;
//		String templateFile = null;
//		while(itr.hasNext()){
//			Element current = (Element)itr.next();
//			WFTreeNode newNode = new WFTreeNode(new IWTreeNode(current.getAttributeValue("name")));
//			pageType = current.getAttributeValue("type");
//			newNode.setPageType(pageType);
//			
//			iconFile = current.getAttributeValue("iconfile");			
//			templateFile = current.getAttributeValue("templatefile");				
//			if (iconFile == null)
//				iconFile = pageMap.get(pageType).getIconFile();
//			if (templateFile == null)
//				templateFile = pageMap.get(pageType).getTemplateFile();				
//			newNode.setIconURI(iconFile);
//			newNode.setTemplateURI(templateFile);
////			if (pageMap.containsKey(pageType)){
////				newNode.setIconURI(pageMap.get(pageType).getIconFile());
////				newNode.setTemplateURI(pageMap.get(pageType).getTemplateFile());
////			}
////			else {	
////				newNode.setIconURI(current.getAttributeValue("iconfile"));
////				newNode.setTemplateURI(current.getAttributeValue("templatefile"));
////			}
//			
//			if(!current.getChildren().isEmpty()){
//				newNode = getPage(pageMap, current, newNode);
//			}
//			currNode.addChild(newNode);
//		}
//		return currNode;
//	}

	public WFTreeNode getPage(SiteTemplateStructure currElement, WFTreeNode currNode){
		Iterator itr = (currElement.getChildStructure()).iterator();
//		String pageName = null;
		String pageType = null;
		String iconFile = null;
		String templateFile = null;
		while(itr.hasNext()){
//			Element current = (Element)itr.next();
			SiteTemplateStructure current = (SiteTemplateStructure)itr.next();
			
			WFTreeNode newNode = new WFTreeNode(new IWTreeNode(current.getName()));
			pageType = current.getType();
			newNode.setPageType(pageType);
			
			iconFile = current.getIconFile();			
			templateFile = current.getTemplateFile();				
			if ((iconFile == null) && (pageType != null))
				iconFile = pageMap.get(pageType).getIconFile();
			if ((templateFile == null) && (pageType != null))
				templateFile = pageMap.get(pageType).getTemplateFile();				
			newNode.setIconURI(iconFile);
			newNode.setTemplateURI(templateFile);
			
			if(!current.getChildStructure().isEmpty()){
				newNode = getPage(current, newNode);
			}
			currNode.addChild(newNode);
		}
		return currNode;
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
	
//	public Map <String, PageTemplate> getPageInfo(Map <String, PageTemplate> pageMap) {		
//		String pageName = null;
//		String pageType = null;
//		String iconFile = null;
//		String templateFile = null;
//		
//		Document siteDocument = getXMLDocument(PAGE_LINK);		
//		Element root = siteDocument.getRootElement();		
//		Collection siteRoot = root.getChildren();			
//		Iterator itr = siteRoot.iterator();
//		while(itr.hasNext()){
//			Element current = (Element)itr.next();
//			pageName = current.getAttributeValue("name");
//			pageType = current.getAttributeValue("type");
//			iconFile = current.getAttributeValue("iconfile");
//			templateFile = current.getAttributeValue("templatefile");
//			pageMap.put(pageType, new PageTemplate(pageName, pageType, iconFile, templateFile));			
//		}		
//		return pageMap;
//	}
	
//	public Map <String, SiteTemplateStructure> getSiteInfo() {
//		
////		Document siteDocument = getXMLDocument(SITE_LINK);		
////		Element root = siteDocument.getRootElement();		
////		Collection siteRoot = root.getChildren();			
////		Iterator itr = siteRoot.iterator();
////		Accordion acc = new Accordion("site_templates");
////		getChildren().add(acc);
////		acc.setHeight("240");
////		int panelID = 0;
////			
//////		pageMap = setPageInfo(new HashMap <String, PageTemplate> ());
////		while(itr.hasNext()){
////			panelID++;
////			Element currentSite = (Element)itr.next();
////			String panelName = currentSite.getAttributeValue("name");
////			Element structure = (Element)currentSite.getChildren().get(0);
////			WFTreeNode rootNode = new WFTreeNode(new IWTreeNode(structure.getAttributeValue("name")));
////
//////			rootNode = getRootNode(currentSite);		
//		
//		
//		Map siteMap = new HashMap <String, SiteTemplateStructure> ();
//		Document siteDocument = getXMLDocument(SITE_LINK);
//		Element root = siteDocument.getRootElement();		
//		Collection siteRoot = root.getChildren();			
//		Iterator itr = siteRoot.iterator();
//		
//		while(itr.hasNext()){
//			SiteTemplateStructure siteStruct = new SiteTemplateStructure();
//			Element currentSite = (Element)itr.next();
//			
////			Element structure = (Element)currentSite.getChildren().get(0);
//			
//			String panelName = currentSite.getAttributeValue("name");
//			
//			Element structure = (Element)currentSite.getChildren().get(0);
//			
////			siteStruct = getNode(currentSite);
//			
////			structure = (Element)structure.getChildren().get(0);
//			
//			siteStruct = getNode(structure);
//			
////			siteStruct.setName(structure.getAttributeValue("name"));
////			siteStruct.setType(structure.getAttributeValue("type"));
////			siteStruct.setIconFile(structure.getAttributeValue("iconfile"));
////			siteStruct.setTemplateFile(structure.getAttributeValue("templatefile"));			
//			
//			siteMap.put(panelName, siteStruct);		
////System.out.println("panelName "+panelName);		
//		}		
//		return siteMap;
//	}
	
	public SiteTemplateStructure getNode(Element currElement){
//		Iterator itr = (currElement.getChildren()).iterator();
		
		String pageName = null;	
		String pageType = null;
		String iconFile = null;
		String templateFile = null;
		SiteTemplateStructure currNode = new SiteTemplateStructure();
//		
		pageType = currElement.getAttributeValue("type");
		currNode.setType(pageType);
		pageName = currElement.getAttributeValue("name");
		currNode.setName(pageName);

		iconFile = currElement.getAttributeValue("iconfile");			
		templateFile = currElement.getAttributeValue("templatefile");				
		if ((iconFile == null) && (pageType != null))
			iconFile = pageMap.get(pageType).getIconFile();
		if ((templateFile == null) && (pageType != null))
			templateFile = pageMap.get(pageType).getTemplateFile();				
		currNode.setIconFile(iconFile);
		currNode.setTemplateFile(templateFile);
		Iterator it = (currElement.getChildren()).iterator();
		while(it.hasNext()){
			currNode.addChild(getNode((Element)it.next()));
		}
		
//
		
		
//		while(itr.hasNext()){
//			Element current = (Element)itr.next();
//			SiteTemplateStructure newNode = new SiteTemplateStructure();
//			
//			pageType = current.getAttributeValue("type");
//			newNode.setType(pageType);
//			pageName = current.getAttributeValue("name");
//			newNode.setName(pageName);
//
////System.out.println("get name "+pageName);
////System.out.println("get type "+pageType);
//			
//			iconFile = current.getAttributeValue("iconfile");			
//			templateFile = current.getAttributeValue("templatefile");				
//			if (iconFile == null)
//				iconFile = pageMap.get(pageType).getIconFile();
//			if (templateFile == null)
//				templateFile = pageMap.get(pageType).getTemplateFile();				
//			newNode.setIconFile(iconFile);
//			newNode.setTemplateFile(templateFile);
//
//			if(!current.getChildren().isEmpty()){
//				SiteTemplateStructure temp = null;
//				temp = getNode(current);
//				if(temp != null)
//					newNode.addChild(temp);				
////				Iterator it = (current.getChildren()).iterator();
////				while(it.hasNext()){
////					SiteTemplateStructure temp = null;
////					temp = getNode((Element)it.next());
////					if(temp != null)
////						newNode.addChild(temp);
////					else
////						System.out.println("*******  GET NULL");
////				}				
//			}
//
//			currNode.addChild(newNode);
////System.out.println("name "+pageName);
////System.out.println("type "+pageType);
//		}
		return currNode;
	}
	
//	public Map reverseMap (Map primaryMap){
//		Map reversedMap = new HashMap();
//		Iterator itr = primaryMap.keySet().iterator();
//		Object mapKey = null;
//		while(itr.hasNext()){
//			mapKey = itr.next();
////System.out.println(mapKey.toString());
//			reversedMap.put(mapKey, primaryMap.get(mapKey));
//		}
//		
//		return reversedMap;
//	}
	
}
