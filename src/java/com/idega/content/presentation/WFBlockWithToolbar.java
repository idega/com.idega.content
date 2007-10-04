package com.idega.content.presentation;

import javax.faces.context.FacesContext;
import javax.faces.el.ValueBinding;

import com.idega.content.business.ContentUtil;
import com.idega.presentation.Image;
import com.idega.presentation.Table;
import com.idega.presentation.text.Link;
import com.idega.presentation.ui.GenericButton;
import com.idega.webface.WFBlock;
import com.idega.webface.WFMenu;
import com.idega.webface.WFUtil;

public class WFBlockWithToolbar extends WFBlock {
	
	private String collapseAllValue;
	private String expandAllValue;
	private String trashCanImage;
	
	private boolean addStartPageButton = false;
	
	public WFBlockWithToolbar() {
		super();
	}

	public WFBlockWithToolbar(String titleBarText, boolean titleIsVB) {
		super (titleBarText, titleIsVB);
	}

	public WFBlockWithToolbar(String titleBarText) {
		super(titleBarText);
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
			Link expand = new Link(expandAllValue, "javascript:void(0)");
			expand.setOnClick("if (treeObj != null) {treeObj.expandAll()}");
			
			if(WFUtil.isValueBinding(collapseAllValue)){
				vb = WFUtil.createValueBinding(collapseAllValue);
				collapseAllValue = (String)(vb.getValue(FacesContext.getCurrentInstance()));
			}

			if(WFUtil.isValueBinding("#{localizedStrings['com.idega.content']['drag_to_delete']}")){
				vb = WFUtil.createValueBinding("#{localizedStrings['com.idega.content']['drag_to_delete']}");
				title = (String)(vb.getValue(FacesContext.getCurrentInstance()));
			}
			
			
			Link collapse = new Link(collapseAllValue, "javascript:void(0)");
			collapse.setOnClick("if (treeObj != null) {treeObj.collapseAll()}");		

			Image recycleBinImage = new Image();
			Table head = null;
			if (isAddStartPageButton()) {
				head = new Table(4, 1);
			}
			else {
				head = new Table(3, 1);
			}
			
			recycleBinImage.setId("trash");
			recycleBinImage.setToolTip(title);
			recycleBinImage.setSrc(trashCanImage);
			recycleBinImage.setStyleClass("recycleBin");
//			recycleBinImage.setOnMouseOver("treeObj.prepareToDelete();");
//			recycleBinImage.setOnMouseOut("treeObj.prepareToDelete();");
			recycleBinImage.setOnMouseOver("treeObj.mouseOverRecycleBin();");
			recycleBinImage.setOnMouseOut("treeObj.mouseOutOfRecycleBin();");
			
			head.add(collapse, 1, 1);
			head.add(expand, 2, 1);
			head.add(recycleBinImage, 3, 1);
			
			if (isAddStartPageButton()) {
				GenericButton startPage = new GenericButton("makeStartPage", ContentUtil.getBundle().getLocalizedString("make_start_page", "Start Page"));
				startPage.setToolTip( ContentUtil.getBundle().getLocalizedString("make_this_page_start_page", "Make This Page As Start Page"));
				startPage.setOnClick("makePageAsStartPage();");
				startPage.setInputType("button");
				startPage.setId("makeStartPage");
				head.add(startPage, 4, 1);
			}
			
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

	public boolean isAddStartPageButton() {
		return addStartPageButton;
	}

	public void setAddStartPageButton(boolean addStartPageButton) {
		this.addStartPageButton = addStartPageButton;
	}

}
