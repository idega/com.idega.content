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

import javax.el.ValueExpression;
import javax.faces.component.UIComponent;

import com.idega.webface.WFBlockTag;

public class WFBlockWithToolbarTag extends WFBlockTag {

	public WFBlockWithToolbarTag() {
		super();
	}

	private ValueExpression collapseAllValue;
	private ValueExpression expandAllValue;
	private ValueExpression trashCanImage;

	private boolean addStartPageButton;

	public ValueExpression getCollapseAllValue() {
		return collapseAllValue;
	}

	public void setCollapseAllValue(ValueExpression collapseAllValue) {
		this.collapseAllValue = collapseAllValue;
	}

	public ValueExpression getExpandAllValue() {
		return expandAllValue;
	}

	public void setExpandAllValue(ValueExpression expandAllValue) {
		this.expandAllValue = expandAllValue;
	}

	public ValueExpression getTrashCanImage() {
		return trashCanImage;
	}

	public void setTrashCanImage(ValueExpression trashCanImage) {
		this.trashCanImage = trashCanImage;
	}

	public boolean isAddStartPageButton() {
		return addStartPageButton;
	}

	public void setAddStartPageButton(boolean addStartPageButton) {
		this.addStartPageButton = addStartPageButton;
	}

	@Override
	public String getComponentType() {
		return "WFBlockWithToolbar";
	}

	@Override
	public void release() {
		super.release();
		collapseAllValue = null;
		expandAllValue = null;
		trashCanImage = null;
		addStartPageButton = false;
	}

	@Override
	protected void setProperties(UIComponent component) {
		super.setProperties(component);
		if (component instanceof WFBlockWithToolbar) {
			WFBlockWithToolbar blockToolbar = (WFBlockWithToolbar) component;

			String collapse = getValue(collapseAllValue);
			if (collapse != null) {
				blockToolbar.setCollapseAllValue(collapse);
			}

			String expand = getValue(expandAllValue);
			if (expand != null) {
				blockToolbar.setExpandAllValue(expand);
			}

			String trashIcon = getValue(trashCanImage);
			if (trashIcon != null) {
				blockToolbar.setTrashCanImage(trashIcon);
			}

			blockToolbar.setAddStartPageButton(isAddStartPageButton());
			blockToolbar.setToolbarForSiteMap();
		}
	}
}