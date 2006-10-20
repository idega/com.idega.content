package com.idega.content.bean;

import org.jdom.Document;
import org.jdom.Element;
import org.apache.myfaces.custom.tree2.TreeNode;
import com.idega.content.themes.helpers.ThemesHelper;
import com.idega.core.data.ICTreeNode;
import com.idega.core.data.IWTreeNode;
import com.idega.core.data.ICTreeNodeAddable;
import com.idega.presentation.PageTag;
import com.idega.webface.WFTreeNode;


import java.util.Iterator;

public class SiteTemplateBean {

	String siteTree = "testing data";
	
	ICTreeNodeAddable iwnode = null;
//	iwnode.addChild
	
	private static final String LINK = ThemesHelper.getInstance().getWebRootWithoutContent() + "/idegaweb/bundles/com.idega.content.bundle/resources/templates/site-templates.xml";
	private static final String SITE_NAME = "Software company";
	
	
//	public TreeNode getSiteTree(){
////	public String getSiteTree(){
////		TreeNode tree = null;
//		String root;
////		tree = getSiteStructure();
////		root = getSiteStructure();
//		
//		return getSiteStructure();
//	}

//	tree.addChild(tree);
	
//	public TreeNode getSiteStructure(){
//		
//		int nodeId = 0;
//		
//		ICTreeNodeAddable rootNode = null;
//		
//		Document siteDocument = ThemesHelper.getInstance().getXMLDocument(LINK);
//		Element root = siteDocument.getRootElement();
//		Element siteRoot = (Element)root.getChild("site");
//		
//		Element currentElement = null;
//		Element temp = null;
//		currentElement = (Element)siteRoot.getChildren().get(0);
//
//		rootNode = ;
//		
////		rootNode.setAsRootNode();
//		getPage(currentElement, rootNode);
//		
//	//		if (siteRoot != null)
//	//			System.out.println(siteRoot.getName());
//
//		
//		ICTreeNodeAddable icnode = (ICTreeNodeAddable)rootNode;
//		IWTreeNode node = rootNode;
//		return new WFTreeNode(icnode);		
////		return rootNode;	
//	}	
	
	public void getPage(Element currElement, ICTreeNodeAddable currNode){
		Iterator itr = (currElement.getChildren()).iterator();
		while(itr.hasNext()){
			Element current = (Element)itr.next();
//System.out.println("node name = " + current.getAttributeValue("name"));
			ICTreeNodeAddable newNode = null;
			newNode.setParent(currNode);
			currNode.addChild(currNode);
			if(!current.getChildren().isEmpty()){
				getPage(current, newNode);
			}
		}
	}
}
