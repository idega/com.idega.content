/*
 * $Id: WebDAVDocumentDeleterMB.java,v 1.1 2004/12/31 02:49:48 gimmi Exp $
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
 *  Last modified: $Date: 2004/12/31 02:49:48 $ by $Author: gimmi $
 * 
 * @author <a href="mailto:gimmi@idega.com">gimmi</a>
 * @version $Revision: 1.1 $
 */
public class WebDAVDocumentDeleterMB {
	Boolean deleted = null;
	Boolean wasFolder = new Boolean(false);
	
	public Boolean getDeleted() {
		return deleted;
	}
	
	public void setDeleted(Boolean deleted) {
		this.deleted = deleted;
	}
	
	public Boolean getWasFolder() {
		return wasFolder;
	}
	public void setWasFolder(Boolean wasFolder) {
		this.wasFolder = wasFolder;
	}
}
