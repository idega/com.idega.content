/*
 * $Id: WFBlockWithToolbarTag.java,v 1.2 2007/05/14 09:44:24 valdas Exp $
 * 
 * Copyright (C) 2004 Idega. All Rights Reserved.
 * 
 * This software is the proprietary information of Idega. Use is subject to
 * license terms.
 * 
 */
package com.idega.content.presentation;

import javax.faces.component.UIComponent;

import com.idega.webface.WFBlockTag;

public class WFBlockWithToolbarTag extends WFBlockTag {

	public WFBlockWithToolbarTag() {
		super();
	}

	private String collapseAllValue;
	private String expandAllValue;
	private String trashCanImage;
	
	private boolean addStartPageButton = false;

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

	public String getComponentType() {
		return "WFBlockWithToolbar";
	}

	public void release() {
		super.release();
		collapseAllValue = null;
		expandAllValue = null;
		trashCanImage = null;
		addStartPageButton = false;
	}

	protected void setProperties(UIComponent component) {
		super.setProperties(component);
		if (component instanceof WFBlockWithToolbar) {
			WFBlockWithToolbar blockToolbar = (WFBlockWithToolbar) component;
			if (collapseAllValue != null) {
				blockToolbar.setCollapseAllValue(collapseAllValue);
			}
			if (expandAllValue != null) {
				blockToolbar.setExpandAllValue(expandAllValue);
			}
			if (trashCanImage != null) {
				blockToolbar.setTrashCanImage(trashCanImage);
			}
			blockToolbar.setAddStartPageButton(isAddStartPageButton());
			
			blockToolbar.setToolbarForSiteMap();
		}
	}

}
