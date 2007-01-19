package com.idega.content;

import javax.faces.context.FacesContext;
import javax.faces.el.ValueBinding;

import com.idega.presentation.Image;
import com.idega.presentation.Table;
import com.idega.presentation.text.Link;
import com.idega.webface.WFBlock;
import com.idega.webface.WFMenu;
import com.idega.webface.WFUtil;

public class WFBlockWithToolbar extends WFBlock{
	private String collapseAllValue;
	private String expandAllValue;
	private String trashCanImage;
	
	public WFBlockWithToolbar() {
		super();
//		setToolbarForSiteMap();
		// TODO Auto-generated constructor stub
	}

	public WFBlockWithToolbar(String titleBarText, boolean titleIsVB) {
		super(titleBarText, titleIsVB);
		// TODO Auto-generated constructor stub
	}

	public WFBlockWithToolbar(String titleBarText) {
		super(titleBarText);
		// TODO Auto-generated constructor stub
	}

	public void setToolbarForSiteMap() {
		String title = null;
		ValueBinding vb = null;
		
		if (getToolbar() == null) {
			WFMenu toolbar = new WFMenu();
			this.setToolbar(toolbar);

			if(WFUtil.isValueBinding(expandAllValue)){
				vb = WFUtil.createValueBinding(expandAllValue);
				expandAllValue = (String)(vb.getValue(FacesContext.getCurrentInstance()));
			}
			Link expand = new Link(expandAllValue);
			expand.setNoURL();
			expand.setOnClick("if (treeObj != null) {treeObj.expandAll()}");
			
			if(WFUtil.isValueBinding(collapseAllValue)){
				vb = WFUtil.createValueBinding(collapseAllValue);
				collapseAllValue = (String)(vb.getValue(FacesContext.getCurrentInstance()));
			}

			if(WFUtil.isValueBinding("#{localizedStrings['com.idega.content']['drag_to_delete']}")){
				vb = WFUtil.createValueBinding("#{localizedStrings['com.idega.content']['drag_to_delete']}");
				title = (String)(vb.getValue(FacesContext.getCurrentInstance()));
			}
			
			
			Link collapse = new Link(collapseAllValue);
			
			collapse.setNoURL();
			collapse.setOnClick("if (treeObj != null) {treeObj.collapseAll()}");		

			Image recycleBinImage = new Image();
			Table head = new Table(3, 1);
			recycleBinImage.attributes.put("id", "trash");
			recycleBinImage.attributes.put("title", title);
			recycleBinImage.attributes.put("src", trashCanImage);			
			recycleBinImage.attributes.put("class", "recycleBin");		
			recycleBinImage.attributes.put("onmouseover", "treeObj.prepareToDelete();");
			recycleBinImage.attributes.put("onmouseout", "treeObj.prepareToDelete();");
			
			head.add(collapse, 1, 1);
			head.add(expand, 2, 1);
			head.add(recycleBinImage, 3, 1);
			toolbar.setMenuHeader(head);			
		}
	}

	public String getCollapseAllValue() {
		return collapseAllValue;
	}

	public void setCollapseAllValue(String collapseAllValue) {
		this.collapseAllValue = collapseAllValue;
	}

	public String getExpandAllValue() {
		return expandAllValue;
	}

	public void setExpandAllValue(String expandAllValue) {
		this.expandAllValue = expandAllValue;
	}

	public String getTrashCanImage() {
		return trashCanImage;
	}

	public void setTrashCanImage(String trashCanImage) {
		this.trashCanImage = trashCanImage;
	}
}
