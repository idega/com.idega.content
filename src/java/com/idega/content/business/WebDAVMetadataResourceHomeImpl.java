/*
 * $Id: WebDAVMetadataResourceHomeImpl.java,v 1.1 2005/01/28 13:52:21 joakim Exp $
 *
 * Copyright (C) 2004 Idega. All Rights Reserved.
 *
 * This software is the proprietary information of Idega.
 * Use is subject to license terms.
 *
 */
package com.idega.content.business;

import com.idega.business.IBOHomeImpl;


/**
 * 
 * Last modified: $Date: 2005/01/28 13:52:21 $ by $Author: joakim $
 *
 * @author Joakim Johnson
 * @version $Revision: 1.1 $
 */
public class WebDAVMetadataResourceHomeImpl extends IBOHomeImpl implements WebDAVMetadataResourceHome {

	protected Class getBeanInterfaceClass() {
		return WebDAVMetadataResource.class;
	}

	public WebDAVMetadataResource create() throws javax.ejb.CreateException {
		return (WebDAVMetadataResource) super.createIBO();
	}
}
