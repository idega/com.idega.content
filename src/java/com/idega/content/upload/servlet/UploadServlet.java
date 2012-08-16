package com.idega.content.upload.servlet;

import com.idega.presentation.IWContext;


public interface UploadServlet {
	
	public static final String PARAMETER_UPLOAD_PATH = "idega-blueimp-upload-path";

	
	public String getServletPath();
	
	public Long getMaxFileSize(IWContext iwc);
	
	
}
