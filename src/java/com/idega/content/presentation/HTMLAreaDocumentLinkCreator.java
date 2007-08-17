/*
 * $Id: HTMLAreaDocumentLinkCreator.java,v 1.5 2007/08/17 13:41:58 valdas Exp $
 * Created on 1.3.2005
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
import com.idega.util.CoreConstants;
import com.idega.webface.htmlarea.HTMLAreaLinkType;


public class HTMLAreaDocumentLinkCreator implements HTMLAreaLinkType {
	
	public ValueBinding getLinkTypeName(IWBundle bundle) {
		return bundle.getValueBinding("link_type_document");
	}
	
	public UIComponent getLinkCreation(Object param) {
		ContentViewer list = new ContentViewer();
		list.setShowPermissionTab(false);
		list.setShowUploadComponent(true);
		if (param != null) {
			list.setRootPath(param.toString());
		} else {
			list.setRootPath(CoreConstants.PATH_FILES_ROOT);
		}
		list.setOnFileClickEvent("SelectDocument(this);return false;");
		list.setColumnsToHide(WebDAVListManagedBean.COLUMN_DELETE+","+WebDAVListManagedBean.COLUMN_CHECKOUT+","+WebDAVListManagedBean.COLUMN_LOCK);
		return list;
	}

	public String getStartingURL() {
		return null;
	}

	public String getStartingTitle() {
		return null;
	}

	public String getStartingTarget() {
		return null;
	}

	public String getLinkType() {
		return "document";
	}

	public UIComponent getLinkCreation() {
		return getLinkCreation(null);
	}

}
