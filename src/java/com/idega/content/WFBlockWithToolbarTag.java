/*
 * $Id: WFBlockWithToolbarTag.java,v 1.1 2007/01/16 10:13:24 justinas Exp $
 *
 * Copyright (C) 2004 Idega. All Rights Reserved.
 *
 * This software is the proprietary information of Idega.
 * Use is subject to license terms.
 *
 */
package com.idega.content;

import javax.faces.component.UIComponent;
import javax.faces.webapp.UIComponentTag;

import com.idega.webface.WFBlockTag;

public class WFBlockWithToolbarTag extends WFBlockTag {
	

	public WFBlockWithToolbarTag() {
		super();
		// TODO Auto-generated constructor stub
	}
	private String collapseAllValue;
	private String expandAllValue;
	private String trashCanImage;
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
	public String getComponentType() {
		return "WFBlockWithToolbar";
	}
	public void release() {  
		super.release(); 
		collapseAllValue = null;
		expandAllValue = null;
		trashCanImage = null;
	} 
	protected void setProperties(UIComponent component) {   
		super.setProperties(component); 
		if (component instanceof WFBlockWithToolbar) {
			WFBlockWithToolbar blockToolbar = (WFBlockWithToolbar) component;
		if(collapseAllValue != null) 
			blockToolbar.setCollapseAllValue(collapseAllValue);
			//component.getAttributes().put("collapseAllValue", collapseAllValue); 
		if(expandAllValue != null) 
			blockToolbar.setExpandAllValue(expandAllValue);
			//component.getAttributes().put("expandAllValue", expandAllValue); 
		if(trashCanImage != null) 
			blockToolbar.setTrashCanImage(trashCanImage);
			//component.getAttributes().put("trashCanImage", trashCanImage); 
		blockToolbar.setToolbarForSiteMap();
		}
	 } 
		  

}
