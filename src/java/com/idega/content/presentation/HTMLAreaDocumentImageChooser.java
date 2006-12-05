/*
 * $Id: HTMLAreaDocumentImageChooser.java,v 1.3.2.1 2006/12/05 15:31:11 gimmi Exp $
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

	public UIComponent getCreationComponent(String url) {
		ContentViewer list = new ContentViewer();
		list.setShowPermissionTab(false);
		list.setShowUploadComponent(true);
		list.setRootPath("/files");
		if (url != null) {
			list.setStartPath(url);
		} else {
			list.setStartPath("/files/public");
		}
		list.setOnFileClickEvent("SelectDocument(this);onPreview();return false;");
		list.setColumnsToHide(WebDAVListManagedBean.COLUMN_DELETE+","+WebDAVListManagedBean.COLUMN_CHECKOUT+","+WebDAVListManagedBean.COLUMN_LOCK);
		return list;
	}

	public String getLinkType() {
		return "document";
	}
}
