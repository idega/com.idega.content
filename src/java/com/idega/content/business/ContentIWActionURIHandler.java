/*
 * $Id: ContentIWActionURIHandler.java,v 1.3 2005/04/08 17:16:01 gummi Exp $
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
 *  Last modified: $Date: 2005/04/08 17:16:01 $ by $Author: gummi $
 * 
 * An IWActionURIHandler handler that handles uri's to documents (webdav)
 * @author <a href="mailto:eiki@idega.com">eiki</a>
 * @version $Revision: 1.3 $
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
		if(getHandlerIdentifier().equals(uri.getHandlerIdentifier())){
			return true;
		}
		//Todo get webservleturi
		return uri.toString().indexOf("/content/")>=0;
	}
	
	public String getHandlerIdentifier(){
		return "content";
	}
	
	/* (non-Javadoc)
	 * @see com.idega.core.uri.IWActionURIHandler#getRedirectURI(com.idega.core.uri.IWActionURI)
	 */
	public String getRedirectURI(IWActionURI uri) {
		
		String actionPart = uri.getActionPart();
		String actionParam = ContentViewer.ACTION_LIST;
		if("preview".equals(actionPart)){
			actionParam = ContentViewer.ACTION_PREVIEW;
		} else if("permission".equals(actionPart)){
			actionParam = ContentViewer.ACTION_PERMISSIONS;
		} 
		
		String redirectURI = uri.getContextURI()+"workspace/content/documents/"+actionPart+"/?"+ContentViewer.PARAMETER_ACTION+"="+actionParam+"&"+ContentViewer.PARAMETER_CONTENT_RESOURCE+"="+uri.getPathPart();
		return redirectURI;
	}
}
