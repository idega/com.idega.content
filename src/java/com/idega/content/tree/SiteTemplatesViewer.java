package com.idega.content.tree;

import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.Collection;
import java.util.Iterator;

import javax.faces.component.html.HtmlOutputLink;
import javax.faces.component.html.HtmlOutputText;
import javax.faces.context.FacesContext;

import org.apache.myfaces.custom.tree2.TreeNode;
import org.jdom.Document;
import org.jdom.Element;
import org.jdom.JDOMException;
import org.jdom.input.SAXBuilder;

import com.idega.block.web2.presentation.Accordion;
import com.idega.content.themes.helpers.ThemesHelper;
import com.idega.core.data.ICTreeNode;
import com.idega.core.data.IWTreeNode;
import com.idega.presentation.IWBaseComponent;
import com.idega.presentation.text.Text;
import com.idega.webface.IWTree;
import com.idega.webface.WFTreeNode;
import com.idega.webface.WFUtil;

//import com.idega.content.themes.helpers.ThemesHelper;

public class SiteTemplatesViewer extends IWBaseComponent {

	private static final String SITE_LINK = ThemesHelper.getInstance().getWebRootWithoutContent()+"/idegaweb/bundles/com.idega.content.bundle/resources/templates/site-templates.xml";

	protected void initializeComponent(FacesContext context) {
		Document siteDocument = getXMLDocument(SITE_LINK);		
		Element root = siteDocument.getRootElement();		
		Collection siteRoot = root.getChildren();			
		Iterator itr = siteRoot.iterator();
		Accordion acc = new Accordion("site_templates");
		getChildren().add(acc);
		acc.setHeight("240");
		int panelID = 0;
		while(itr.hasNext()){
			panelID++;
			Element currentSite = (Element)itr.next();
			String panelName = currentSite.getAttributeValue("name");
			Element structure = (Element)currentSite.getChildren().get(0);
			WFTreeNode rootNode = new WFTreeNode(new IWTreeNode(structure.getAttributeValue("name")));
			
//			IWTreeNode iwNode = null;
			
			rootNode = getPage(structure, rootNode);
			
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
//		    linki.setValueBinding("iconURI",WFUtil.createValueBinding("#{node.iconURI}"));
		    
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

//	public ICTreeNode getICNode(IWTreeNode rootNode){
//		ICTreeNode icnode = rootNode;
//		return icnode;		
//	}
	
	public WFTreeNode getPage(Element currElement, WFTreeNode currNode){
		Iterator itr = (currElement.getChildren()).iterator();
		while(itr.hasNext()){
			Element current = (Element)itr.next();
			WFTreeNode newNode = new WFTreeNode(new IWTreeNode(current.getAttributeValue("name")));
			newNode.setIconURI(current.getAttributeValue("iconfile"));
			newNode.setPageType(current.getAttributeValue("type"));
			newNode.setTemplateURI(current.getAttributeValue("templatefile"));
			if(!current.getChildren().isEmpty()){
				newNode = getPage(current, newNode);
			}
			currNode.addChild(newNode);
		}
		return currNode;
	}
	
	
//	public IWTreeNode getPage(Element currElement, IWTreeNode currNode){
//		Iterator itr = (currElement.getChildren()).iterator();
//		while(itr.hasNext()){
//			Element current = (Element)itr.next();
//			IWTreeNode newNode = new IWTreeNode(current.getAttributeValue("name"));
////			newNode.setIconURI(current.getAttributeValue("iconfile"));
////			newNode.setParent(currNode);
//			currNode.addChild(newNode);
//			if(!current.getChildren().isEmpty()){
//				newNode = getPage(current, newNode);
//			}
//		}
//		return currNode;
//	}

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
//	
//	 public void encodeEnd(FacesContext context) throws IOException { 
//	  ResponseWriter writer = context.getResponseWriter(); 
////	     writer.endElement("div"); 
//	 } 
}
