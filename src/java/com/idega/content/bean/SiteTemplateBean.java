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

	private static final String LINK = ThemesHelper.getInstance().getWebRootWithoutContent() + "/idegaweb/bundles/com.idega.content.bundle/resources/templates/site-templates.xml";
	private static final String SITE_NAME = "Software company";

	TreeNode siteTree = null;	
	ICTreeNodeAddable iwnode = null;
	
	public TreeNode getSiteTree(){
		String root;
//		TreeNode result = getSiteStructure();
		return getSiteStructure();
	}

	public TreeNode getSiteStructure(){
		
		int nodeId = 0;
		
		ICTreeNodeAddable rootNode = null;
		
		Document siteDocument = ThemesHelper.getInstance().getXMLDocument(LINK);
		Element root = siteDocument.getRootElement();
		Element siteRoot = (Element)root.getChild("site");
		
		Element currentElement = null;
		Element temp = null;
		currentElement = (Element)siteRoot.getChildren().get(0);
		
		rootNode = (ICTreeNodeAddable)(new IWTreeNode(currentElement.getAttributeValue("name")));
		
		
		rootNode = getPage(currentElement, rootNode);
				
//		ICTreeNode icnode = rootNode.getParentNode();
		ICTreeNode icnode = rootNode;

//		IWTreeNode node = rootNode;

		return new WFTreeNode(icnode);		

//		return rootNode;	
	}	
	
	public ICTreeNodeAddable getPage(Element currElement, ICTreeNodeAddable currNode){
		Iterator itr = (currElement.getChildren()).iterator();
		while(itr.hasNext()){
			Element current = (Element)itr.next();
			ICTreeNodeAddable newNode = (ICTreeNodeAddable)(new IWTreeNode(current.getAttributeValue("name")));
System.out.println(current.getAttributeValue("name"));			
			newNode.setParent(currNode);
			currNode.addChild(newNode);
//			if(!current.getChildren().isEmpty()){
//				currNode = getPage(current, newNode);
//			}
		}
		return currNode;
	}
}
