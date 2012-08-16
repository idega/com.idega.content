package com.idega.content.upload.business;

import com.idega.presentation.IWContext;

public interface UploadAreaBean {

	public static final String BEAN_NAME = "uploadAreaBean";
	public Long getMaxFileSize(IWContext iwc);
	public String getServletPath();
}
