/*
 * $Id: HTMLAreaDocumentImageChooser.java,v 1.1 2005/03/09 09:45:44 gimmi Exp $
 * Created on 8.3.2005
 *
 * Copyright (C) 2005 Idega Software hf. All Rights Reserved.
 *
 * This software is the proprietary information of Idega hf.
 * Use is subject to license terms.
 */
package com.idega.content.presentation;

import javax.faces.component.UIComponent;
import javax.faces.el.ValueBinding;
import com.idega.idegaweb.IWBundle;
import com.idega.webface.htmlarea.HTMLAreaImageType;


public class HTMLAreaDocumentImageChooser implements HTMLAreaImageType {

	public ValueBinding getLinkTypeName(IWBundle iwb) {
		return iwb.getValueBinding("image_type_document");
	}

	public UIComponent getCreationComponent() {
		ContentViewer list = new ContentViewer();
		list.setShowPermissionTab(false);
		list.setShowUploadComponent(true);
		list.setRootFolder("/files");
		list.setOnFileClickEvent("Set()");
		list.setColumnsToHide(WebDAVListManagedBean.COLUMN_DELETE+","+WebDAVListManagedBean.COLUMN_CHECKOUT+","+WebDAVListManagedBean.COLUMN_LOCK);
		return list;
	}

	public String getLinkType() {
		return "document";
	}
}
