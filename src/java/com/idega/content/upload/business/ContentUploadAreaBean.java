package com.idega.content.upload.business;

import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.context.annotation.Scope;
import org.springframework.stereotype.Repository;

import com.idega.presentation.IWContext;

@Repository(UploadAreaBean.BEAN_NAME)
@Scope(BeanDefinition.SCOPE_SINGLETON)
public class ContentUploadAreaBean implements UploadAreaBean{

	private static final Long MAX_UPLOAD_SIZE = new Long(1024 * 1024) * 20;	//	20 MBs
	private static final String SERVLET_PATH = "/servlet/blueimp-upload";
	@Override
	public Long getMaxFileSize(IWContext iwc) {
		return MAX_UPLOAD_SIZE;
	}

	@Override
	public String getServletPath() {
		return SERVLET_PATH;
	}

}
