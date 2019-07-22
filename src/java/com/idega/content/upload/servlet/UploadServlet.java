package com.idega.content.upload.servlet;

import com.idega.presentation.IWContext;

public interface UploadServlet {

	public static final String PARAMETER_UPLOAD_PATH = "idega-blueimp-upload-path";
	
	public static final String PARAMETER_UPLOAD_ADD_THUMBNAIL = "idega-blueimp-upload-is-add-thumbnail";

	public String getServletPath();

	public Long getMaxFileSize(IWContext iwc);

}