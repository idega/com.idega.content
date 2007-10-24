package com.idega.content.themes.presentation;

import javax.faces.component.UIComponent;
import javax.faces.el.ValueBinding;
import javax.faces.webapp.UIComponentTag;

import org.apache.myfaces.custom.tree2.TreeNode;

import com.idega.presentation.IWContext;
import com.idega.util.CoreUtil;
import com.idega.webface.IWTree;

public class SiteTreeViewerTag extends UIComponentTag {

	private TreeNode rootNode = null;

	private String rootNodeExpression = null;
	private String linkStyleClass = "pageTreeNames";
	private String iwTreeId = "current_structure_tree";
	private String rendererType = IWTree.class.getName();
	private String facetName = "PageTreeNode";
	private String varName = "node";
	
	private boolean sourceTree = false;
	private boolean showRootNode = false;
	private boolean showLines = false;
	private boolean addStyleClassForLink = true;
	
	@Override
	public String getComponentType() {
		return "SiteTreeViewer";
	}

	@Override
	public String getRendererType() {
		return null;
	}
	
	protected void setProperties(UIComponent component) {
		if (component instanceof SiteTreeViewer) {
			if (rootNodeExpression == null) {
				new IllegalArgumentException("Provide root node");
			}
			
			Object o = null;
			IWContext iwc = CoreUtil.getIWContext();
			if (iwc == null) {
				return;
			}
			ValueBinding vb = iwc.getIWMainApplication().createValueBinding(rootNodeExpression);
			o = vb.getValue(iwc);
			if (!(o instanceof TreeNode)) {
				new IllegalArgumentException("Provide root node");
			}
			rootNode = (TreeNode) o;
			
			SiteTreeViewer treeViewer = (SiteTreeViewer) component;
		
			treeViewer.setRootNode(rootNode);
			treeViewer.setLinkStyleClass(linkStyleClass);
			treeViewer.setIwTreeId(iwTreeId);
			treeViewer.setRendererType(rendererType);
			treeViewer.setFacetName(facetName);
			treeViewer.setVarName(varName);
			treeViewer.setSourceTree(sourceTree);
			treeViewer.setShowRootNode(showRootNode);
			treeViewer.setShowLines(showLines);
			treeViewer.setAddStyleClassForLink(addStyleClassForLink);
			
		}
	}

	public String getRootNodeExpression() {
		return rootNodeExpression;
	}

	public void setRootNodeExpression(String rootNodeExpression) {
		this.rootNodeExpression = rootNodeExpression;
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
		return sourceTree;
	}

	public void setSourceTree(boolean sourceTree) {
		this.sourceTree = sourceTree;
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

	public void setRendererType(String rendererType) {
		this.rendererType = rendererType;
	}

}
