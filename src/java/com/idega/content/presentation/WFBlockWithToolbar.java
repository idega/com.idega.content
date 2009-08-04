package com.idega.content.presentation;

import javax.el.ValueExpression;
import javax.faces.context.FacesContext;
import com.idega.presentation.Image;
import com.idega.presentation.Table;
import com.idega.presentation.text.Link;
import com.idega.util.StringUtil;
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
		FacesContext fc = FacesContext.getCurrentInstance();
		
		String title = null;
		
		if (getToolbar() == null) {
			WFMenu toolbar = new WFMenu();
			this.setToolbar(toolbar);

			expandAllValue = getValueFromExpression(fc, expandAllValue);
			Link expand = new Link(expandAllValue, "javascript:void(0)");
			expand.setOnClick("if (treeObj != null) {treeObj.expandAll()}");
			
			collapseAllValue = getValueFromExpression(fc, collapseAllValue);
			Link collapse = new Link(collapseAllValue, "javascript:void(0)");
			collapse.setOnClick("if (treeObj != null) {treeObj.collapseAll()}");	
			
			title = getValueFromExpression(fc, "#{localizedStrings['com.idega.content']['drag_to_delete']}");

			Image recycleBinImage = new Image();
			Table head = new Table(isAddStartPageButton() ? 4 : 3, 1);

			recycleBinImage.setId("trash");
			recycleBinImage.setTitle(title);
			recycleBinImage.setSrc(getValueFromExpression(fc, trashCanImage));
			recycleBinImage.setStyleClass("recycleBin");
			recycleBinImage.setOnMouseOver("treeObj.mouseOverRecycleBin();");
			recycleBinImage.setOnMouseOut("treeObj.mouseOutOfRecycleBin();");
			
			head.add(collapse, 1, 1);
			head.add(expand, 2, 1);
			head.add(recycleBinImage, 3, 1);
			
			toolbar.setMenuHeader(head);			
		}
	}
	
	private String getValueFromExpression(FacesContext fc, String expression) {
		if (StringUtil.isEmpty(expression) || !WFUtil.isValueBinding(expression)) {
			return expression;
		}
	
		ValueExpression ve = WFUtil.createValueExpression(fc.getELContext(), expression, String.class);
		return ve == null ? expression : (String) ve.getValue(fc.getELContext());
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
