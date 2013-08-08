package com.idega.content.upload.business;

import org.springframework.context.annotation.Scope;
import org.springframework.stereotype.Service;

import com.idega.presentation.IWContext;

@Scope("session")
@Service(UploadAreaBean.BEAN_NAME)
public class ContentUploadAreaBean implements UploadAreaBean {

	private static final Long MAX_UPLOAD_SIZE = new Long(1024 * 1024) * 20;	//	20 MBs
	private static final String SERVLET_PATH = "/servlet/blueimp-upload";

	private Long maxFileSize = MAX_UPLOAD_SIZE;

	private String servletPath = SERVLET_PATH;

	private boolean addThumbnail = true;

	@Override
	public Long getMaxFileSize(IWContext iwc) {
		return maxFileSize;
	}

	@Override
	public String getServletPath() {
		return servletPath;
	}

	public Long getMaxFileSize() {
		return maxFileSize;
	}

	public void setMaxFileSize(Long maxFileSize) {
		this.maxFileSize = maxFileSize;
	}

	public void setServletPath(String servletPath) {
		this.servletPath = servletPath;
	}

	public boolean isAddThumbnail() {
		return addThumbnail;
	}

	public void setAddThumbnail(boolean addThumbnail) {
		this.addThumbnail = addThumbnail;
	}

}