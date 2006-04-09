/*
 * $Id: WebDAVDocumentDeleterMB.java,v 1.2 2006/04/09 12:01:55 laddi Exp $
 * Created on 31.12.2004
 *
 * Copyright (C) 2004 Idega Software hf. All Rights Reserved.
 *
 * This software is the proprietary information of Idega hf.
 * Use is subject to license terms.
 */
package com.idega.content.presentation;


/**
 * 
 *  Last modified: $Date: 2006/04/09 12:01:55 $ by $Author: laddi $
 * 
 * @author <a href="mailto:gimmi@idega.com">gimmi</a>
 * @version $Revision: 1.2 $
 */
public class WebDAVDocumentDeleterMB {
	Boolean deleted = null;
	Boolean wasFolder = new Boolean(false);
	
	public Boolean getDeleted() {
		return this.deleted;
	}
	
	public void setDeleted(Boolean deleted) {
		this.deleted = deleted;
	}
	
	public Boolean getWasFolder() {
		return this.wasFolder;
	}
	public void setWasFolder(Boolean wasFolder) {
		this.wasFolder = wasFolder;
	}
}
