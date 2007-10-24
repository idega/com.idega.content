package com.idega.content.themes.presentation;

import javax.faces.component.html.HtmlOutputLink;
import javax.faces.component.html.HtmlOutputText;

import org.apache.myfaces.custom.tree2.TreeNode;

import com.idega.content.business.ContentConstants;
import com.idega.presentation.Block;
import com.idega.presentation.IWContext;
import com.idega.webface.IWTree;
import com.idega.webface.WFUtil;

public class SiteTreeViewer extends Block {
	
	private TreeNode rootNode = null;
	
	private String linkStyleClass = "pageTreeNames";
	private String iwTreeId = "current_structure_tree";
	private String rendererType = IWTree.class.getName();
	private String facetName = "PageTreeNode";
	private String varName = "node";
	
	private boolean isSourceTree = false;
	private boolean showRootNode = false;
	private boolean showLines = false;
	private boolean addStyleClassForLink = true;

	@SuppressWarnings({ "unchecked", "deprecation" })
	public void main(IWContext iwc) {
		if (rootNode == null) {
			return;
		}
		
		IWTree tree = new IWTree();
		tree.setValue(rootNode);
		tree.setShowRootNode(showRootNode);
	    tree.setId(iwTreeId);
	    tree.setShowLines(showLines);
	    tree.setVar(varName);
	    tree.setRendererType(rendererType);
	    
	    HtmlOutputLink link = new HtmlOutputLink();
	    link.setValue("javascript:void(0)");
	    if (addStyleClassForLink) {
	    	link.setStyleClass(linkStyleClass);
	    }
	    HtmlOutputText text = new HtmlOutputText();		    
	    text.setValueBinding("value", WFUtil.createValueBinding(new StringBuffer("#{").append(varName).append(".description}").toString()));
	    
	    link.getChildren().add(text);
	    
	    tree.getFacets().put(facetName, link);
	    tree.getAttributes().put("sourceTree", isSourceTree);
	    add(tree);
	}
	
	public TreeNode getRootNode() {
		return rootNode;
	}

	public void setRootNode(TreeNode rootNode) {
		this.rootNode = rootNode;
	}

	public String getLinkStyleClass() {
		return linkStyleClass;
	}

	public void setLinkStyleClass(String linkStyleClass) {
		this.linkStyleClass = linkStyleClass;
	}

	public String getIwTreeId() {
		return iwTreeId;
	}

	public void setIwTreeId(String iwTreeId) {
		this.iwTreeId = iwTreeId;
	}

	public String getRendererType() {
		return rendererType;
	}

	public void setRendererType(String rendererType) {
		this.rendererType = rendererType;
	}

	public String getFacetName() {
		return facetName;
	}

	public void setFacetName(String facetName) {
		this.facetName = facetName;
	}

	public String getVarName() {
		return varName;
	}

	public void setVarName(String varName) {
		this.varName = varName;
	}

	public boolean isSourceTree() {
		return isSourceTree;
	}

	public void setSourceTree(boolean isSourceTree) {
		this.isSourceTree = isSourceTree;
	}

	public boolean isShowRootNode() {
		return showRootNode;
	}

	public void setShowRootNode(boolean showRootNode) {
		this.showRootNode = showRootNode;
	}

	public boolean isShowLines() {
		return showLines;
	}

	public void setShowLines(boolean showLines) {
		this.showLines = showLines;
	}

	public boolean isAddStyleClassForLink() {
		return addStyleClassForLink;
	}

	public void setAddStyleClassForLink(boolean addStyleClassForLink) {
		this.addStyleClassForLink = addStyleClassForLink;
	}

	public String getBundleIdentifier() {
		return ContentConstants.IW_BUNDLE_IDENTIFIER;
	}
	
}
