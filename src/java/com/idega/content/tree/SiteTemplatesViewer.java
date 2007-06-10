package com.idega.content.tree;

import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
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
import com.idega.content.themes.business.TemplatesLoader;
import com.idega.core.data.ICTreeNode;
import com.idega.core.data.IWTreeNode;
import com.idega.idegaweb.IWMainApplication;
import com.idega.presentation.IWBaseComponent;
import com.idega.presentation.IWContext;
import com.idega.presentation.Script;
import com.idega.presentation.text.Text;
import com.idega.webface.IWTree;
import com.idega.webface.WFTreeNode;
import com.idega.webface.WFUtil;

public class SiteTemplatesViewer extends IWBaseComponent {
	
	private Map <String, PageTemplate> pageMap = null;
	private Map <String, SiteTemplateStructure> siteMap = null;
	
	public SiteTemplatesViewer() {
		super();
		// TODO Auto-generated constructor stub
		FacesContext ctx = FacesContext.getCurrentInstance();
		IWMainApplication iwma = IWMainApplication.getIWMainApplication(ctx);
		pageMap = (Map <String, PageTemplate>)TemplatesLoader.getInstance(iwma).getPageTemplates();		
		siteMap = (Map <String, SiteTemplateStructure>)TemplatesLoader.getInstance(iwma).getSiteTemplates();		
	}
	
	protected void initializeComponent(FacesContext context) {
		Iterator itrKeySet = null;
		if (siteMap.keySet() != null)
			itrKeySet = siteMap.keySet().iterator();
		String mapKey = null;
		Accordion acc = new Accordion("site_templates");
//		acc.setIncludeJavascript(false);
		
		getChildren().add(acc);

		int panelID = 0;
		Script script = new Script();
		
		while(itrKeySet.hasNext()){
			panelID++;
			mapKey = itrKeySet.next().toString();
			SiteTemplateStructure currentSite = siteMap.get(mapKey);
			String panelName = mapKey;
			WFTreeNode rootNode = new WFTreeNode(new IWTreeNode(panelName));
			
			rootNode = getPage(currentSite, rootNode);
			rootNode = settingIconURIsAndTemplateFiles(rootNode);
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
		    script.addScriptLine("appendIdOfTree(\'tree\'+"+panelID+");");
			
		}
		
		this.getChildren().add(script);		
				
	}	
	
	private WFTreeNode settingIconURIsAndTemplateFiles(WFTreeNode node){
		node.setIconURI(getIconUriByPageType(node.getPageType()));
		node.setTemplateURI(getTemplateFileByPageType(node.getPageType()));
		List<WFTreeNode> nodeChildren = node.getChildren();
		if (nodeChildren != null)
			for (int i = 0; i < nodeChildren.size(); i++){
				nodeChildren.set(i, settingIconURIsAndTemplateFiles(nodeChildren.get(i)));
			}
		
		return node;
	}
	private String getTemplateFileByPageType(String pageType){
		if (pageMap == null){
			IWContext iwc = IWContext.getInstance();
			IWMainApplication iwma = iwc.getApplicationContext().getIWMainApplication();
			TemplatesLoader loader = new TemplatesLoader(iwma);
			pageMap = loader.getPageMap();
			if(pageMap.isEmpty()){
				loader.loadTemplatesFromBundles();
				pageMap = loader.getPageMap();
			}			
//			IWContext iwc = IWContext.getInstance();
//			Map pageTemplatesFromCache = IWCacheManager2.getInstance(iwc.getApplicationContext().getIWMainApplication()).getCache(ContentConstants.PAGE_TYPES_CACHE_KEY);
//			
//			if (pageTemplatesFromCache.containsKey(ContentConstants.PAGES_MAP_KEY)){
//				pageMap = (Map <String, PageTemplate>)pageTemplatesFromCache.get(ContentConstants.PAGES_MAP_KEY);
//			}
//			else {
//				pageMap = new HashMap <String, PageTemplate> ();
//				pageTemplatesFromCache.put(ContentConstants.PAGES_MAP_KEY, pageMap);
//			}		
		}
	
		if(pageMap.get(pageType) != null)
			return pageMap.get(pageType).getTemplateFile();
		else {
			System.out.println("pageType not set");
			return "";
		}
	}
	private String getIconUriByPageType(String pageType){
		
		if (pageMap == null){
			IWContext iwc = IWContext.getInstance();
			IWMainApplication iwma = iwc.getApplicationContext().getIWMainApplication();
			TemplatesLoader loader = new TemplatesLoader(iwma);
			pageMap = loader.getPageMap();
			if(pageMap.isEmpty()){
				loader.loadTemplatesFromBundles();
				pageMap = loader.getPageMap();
			}			
//			IWContext iwc = IWContext.getInstance();
//			Map pageTemplatesFromCache = IWCacheManager2.getInstance(iwc.getApplicationContext().getIWMainApplication()).getCache(ContentConstants.PAGE_TYPES_CACHE_KEY);
//			
//			if (pageTemplatesFromCache.containsKey(ContentConstants.PAGES_MAP_KEY)){
//				pageMap = (Map <String, PageTemplate>)pageTemplatesFromCache.get(ContentConstants.PAGES_MAP_KEY);
//			}
//			else {
//				pageMap = new HashMap <String, PageTemplate> ();
//				pageTemplatesFromCache.put(ContentConstants.PAGES_MAP_KEY, pageMap);
//			}		
		}
		
		if(pageMap.get(pageType) != null)
			return pageMap.get(pageType).getIconFile();
		else {
			System.out.println("pageType not set");
			return "";
		}
	}
	
	public TreeNode getTree(IWTreeNode rootNode){
		ICTreeNode icnode = rootNode;
		return new WFTreeNode(icnode);
	}
	
	public WFTreeNode getPage(SiteTemplateStructure currElement, WFTreeNode currNode){
		Iterator itr = (currElement.getChildStructure()).iterator();
		String pageType = null;
		String iconFile = null;
		String templateFile = null;
		while(itr.hasNext()){
			SiteTemplateStructure current = (SiteTemplateStructure)itr.next();
			
			WFTreeNode newNode = new WFTreeNode(new IWTreeNode(current.getName()));
			pageType = current.getType();
			newNode.setPageType(pageType);
			
			iconFile = current.getIconFile();			
			templateFile = current.getTemplateFile();
			
			PageTemplate pageTemplate = pageMap.get(pageType);
			if ((iconFile == null) && (pageType != null)){
				if (pageTemplate != null)
					iconFile = pageMap.get(pageType).getIconFile();
			}
			if ((templateFile == null) && (pageType != null))
				if (pageTemplate != null)
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
		
	public SiteTemplateStructure getNode(Element currElement){
		
		String pageName = null;	
		String pageType = null;
		String iconFile = null;
		String templateFile = null;
		SiteTemplateStructure currNode = new SiteTemplateStructure();
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
		return currNode;
	}		
}
