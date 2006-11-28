package com.idega.content.bean;

import java.util.Iterator;

import org.apache.myfaces.custom.tree2.TreeNode;
import org.jdom.Document;
import org.jdom.Element;

import com.idega.content.themes.helpers.ThemesHelper;
import com.idega.core.data.IWTreeNode;
import com.idega.webface.WFTreeNode;

public class SiteTemplateBean {
// TO DO change IWTreeNode to PageTreeNode
	
	private static final String SITE_LINK = ThemesHelper.getInstance().getWebRootWithoutContent() + "/idegaweb/bundles/com.idega.content.bundle/resources/templates/site-templates.xml";
	private static final String PAGE_LINK = ThemesHelper.getInstance().getWebRootWithoutContent() + "/idegaweb/bundles/com.idega.content.bundle/resources/templates/page-templates.xml";
	
	TreeNode siteTree = null;
	TreeNode pageTree = null;
	String path = null;
	Document siteDocument = null;
	
	public TreeNode getSiteTree(){		
		return getSiteStructure();
	}

	public TreeNode getSiteStructure(){
		
		Document siteDocument = ThemesHelper.getInstance().getXMLDocument(SITE_LINK);
		Element root = siteDocument.getRootElement();
		Element siteRoot = root.getChild("site");
		
		Element currentElement = (Element)siteRoot.getChildren().get(0);
		
//		IWTreeNode rootNode = new IWTreeNode(currentElement.getAttributeValue("name"));
		WFTreeNode rootNode = new WFTreeNode(new IWTreeNode(currentElement.getAttributeValue("name")));
//		IWTreeNodeWithTypes rootNode = new IWTreeNodeWithTypes(currentElement.getAttributeValue("name"),"","");
		
		
		rootNode = getPage(currentElement, rootNode);
//		ICTreeNode icnode = rootNode;
////////////////////////////
//	    HtmlOutputText texti = new HtmlOutputText();
//	    texti.setValueBinding("value",WFUtil.createValueBinding("#{node.description}"));
//	    tree.getFacets().put("IWTreeNode",texti);
///////////////////////////
		
//		IWTreeNode node = rootNode;

//		return new WFTreeNode(icnode);
		return rootNode;

//		return rootNode;	
	}	
	
//	public IWTreeNode getPage(Element currElement, IWTreeNode currNode){
//		String name;
//		String type;
//		String iconfile;
//		Iterator itr = (currElement.getChildren()).iterator();
//		while(itr.hasNext()){
//			Element current = (Element)itr.next();
//			name = current.getAttributeValue("name");
//			type = current.getAttributeValue("type");
//			iconfile = current.getAttributeValue("iconfile"); 
//			IWTreeNode newNode = new IWTreeNode(name);
//			newNode.setParent(currNode);
//			currNode.addChild(newNode);
//			if(!current.getChildren().isEmpty()){
//				newNode = getPage(current, newNode);
//			}
//		}
//		return currNode;
//	}
	
	public WFTreeNode getPage(Element currElement, WFTreeNode currNode){
		//String name;
		//String type;
		//String iconfile;
		Iterator itr = (currElement.getChildren()).iterator();
		while(itr.hasNext()){
			Element current = (Element)itr.next();
			//name = current.getAttributeValue("name");
			//type = current.getAttributeValue("type");
			//iconfile = current.getAttributeValue("iconfile"); 
//			IWTreeNode newNode = new IWTreeNode(name);
			WFTreeNode newNode = new WFTreeNode(new IWTreeNode(current.getAttributeValue("name")));
			newNode.setIconURI(current.getAttributeValue("iconfile"));
			newNode.setPageType(current.getAttributeValue("type"));
//System.out.println("iconFile = "+iconfile);			
//			newNode.setParent(currNode);
			currNode.addChild(newNode);
			if(!current.getChildren().isEmpty()){
				newNode = getPage(current, newNode);
			}
		}
		return currNode;
	}
	
	public TreeNode getPageTree(){		
		Document siteDocument = ThemesHelper.getInstance().getXMLDocument(PAGE_LINK);
		Element root = siteDocument.getRootElement();
//		Element siteRoot = (Element)root.getChild("site");
//		root = (Element)root.getChild("site"); 
		
//		Element currentElement = (Element)siteRoot.getChildren().get(0);
		Element currentElement = root;
//		IWTreeNodeWithTypes rootNode = new IWTreeNodeWithTypes("","","");
		WFTreeNode rootNode = new WFTreeNode(new IWTreeNode(currentElement.getAttributeValue("name")));
//		IWTreeNode rootNode = new IWTreeNode("");
		rootNode = getPage(currentElement, rootNode);
//		ICTreeNode icnode = rootNode;
//		return new WFTreeNode(icnode);
		return rootNode;
		}

	public String getPath() {
		return ThemesHelper.getInstance().getWebRootWithoutContent() + "/idegaweb/bundles/com.idega.content.bundle/resources/templates/site-templates.xml";
	}

	public void setPath(String path) {
		this.path = path;
	}

	public Document getSiteDocument() {
		return ThemesHelper.getInstance().getXMLDocument(SITE_LINK);
	}

	public void setSiteDocument(Document siteDocument) {
		this.siteDocument = siteDocument;
	}
}
