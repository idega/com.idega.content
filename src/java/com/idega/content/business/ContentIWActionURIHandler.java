/*
 * $Id: ContentIWActionURIHandler.java,v 1.1 2005/02/28 13:35:25 eiki Exp $
 * Created on Jan 31, 2005
 *
 * Copyright (C) 2005 Idega Software hf. All Rights Reserved.
 *
 * This software is the proprietary information of Idega hf.
 * Use is subject to license terms.
 */
package com.idega.content.business;

import com.idega.content.presentation.ContentViewer;
import com.idega.core.uri.DefaultIWActionURIHandler;
import com.idega.core.uri.IWActionURI;
import com.idega.core.uri.IWActionURIHandler;




/**
 * 
 *  Last modified: $Date: 2005/02/28 13:35:25 $ by $Author: eiki $
 * 
 * An IWActionURIHandler handler that handles uri's to documents (webdav)
 * @author <a href="mailto:eiki@idega.com">eiki</a>
 * @version $Revision: 1.1 $
 */
public class ContentIWActionURIHandler extends DefaultIWActionURIHandler implements IWActionURIHandler {

	/**
	 * 
	 */
	public ContentIWActionURIHandler() {
		super();
	}

	/* (non-Javadoc)
	 * @see com.idega.core.uri.IWActionURIHandler#canHandleIWActionURI(com.idega.core.uri.IWActionURI)
	 */
	public boolean canHandleIWActionURI(IWActionURI uri) {
		//Todo get webservleturi
		return uri.toString().indexOf("/content/files/")>=0;
	}
	
	/* (non-Javadoc)
	 * @see com.idega.core.uri.IWActionURIHandler#getRedirectURI(com.idega.core.uri.IWActionURI)
	 */
	public String getRedirectURI(IWActionURI uri) {
		String redirectURI = uri.getContextURI()+"workspace/content/documents/"+uri.getActionPart()+"/?"+ContentViewer.PARAMETER_ACTION+"="+ContentViewer.ACTION_PREVIEW+"&"+ContentViewer.PARAMETER_CONTENT_RESOURCE+"="+uri.getPathPart();
		return redirectURI;
	}
}
