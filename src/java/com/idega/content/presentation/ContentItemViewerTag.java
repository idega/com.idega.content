/*
 * $Id: ContentItemViewerTag.java,v 1.1 2005/02/21 16:12:45 gummi Exp $
 * Created on 21.2.2005
 *
 * Copyright (C) 2005 Idega Software hf. All Rights Reserved.
 *
 * This software is the proprietary information of Idega hf.
 * Use is subject to license terms.
 */
package com.idega.content.presentation;

import javax.faces.component.UIComponent;
import javax.faces.webapp.UIComponentTag;


/**
 * 
 *  Last modified: $Date: 2005/02/21 16:12:45 $ by $Author: gummi $
 * 
 * @author <a href="mailto:gummi@idega.com">Gudmundur Agust Saemundsson</a>
 * @version $Revision: 1.1 $
 */
public abstract class ContentItemViewerTag extends UIComponentTag {

	private Boolean renderDetailsCommand = null;
	private Boolean showRequestedItem = null;

	/**
	 * 
	 */
	public ContentItemViewerTag() {
		super();
	}

	/* (non-Javadoc)
	 * @see javax.faces.webapp.UIComponentTag#getComponentType()
	 */
	public abstract String getComponentType();

	/* (non-Javadoc)
	 * @see javax.faces.webapp.UIComponentTag#getRendererType()
	 */
	public String getRendererType() {
		return ContentItemViewer.DEFAULT_RENDERER_TYPE;
	}
	
	
	public void release() {      
		super.release(); 
		renderDetailsCommand = null;
		showRequestedItem = null;
	}

	protected void setProperties(UIComponent component) {      
		super.setProperties(component);
		if (component instanceof ContentViewer) {
			ContentItemViewer viewer = (ContentItemViewer)component;
			
			if(renderDetailsCommand != null){
				viewer.setRenderDetailsCommand(renderDetailsCommand.booleanValue());
			}
			
			if(showRequestedItem != null){
				viewer.setShowRequestedItem(showRequestedItem.booleanValue());
			}
			
		}
	}
	/**
	 * @return Returns the renderDetailsCommand.
	 */
	public Boolean getRenderDetailsCommand() {
		return renderDetailsCommand;
	}
	/**
	 * @param renderDetailsCommand The renderDetailsCommand to set.
	 */
	public void setRenderDetailsCommand(Boolean renderDetailsCommand) {
		this.renderDetailsCommand = renderDetailsCommand;
	}
	/**
	 * @return Returns the showRequestedItem.
	 */
	public Boolean getShowRequestedItem() {
		return showRequestedItem;
	}
	/**
	 * @param showRequestedItem The showRequestedItem to set.
	 */
	public void setShowRequestedItem(Boolean showRequestedItem) {
		this.showRequestedItem = showRequestedItem;
	}
}
