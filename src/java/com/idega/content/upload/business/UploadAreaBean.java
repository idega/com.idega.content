package com.idega.content.upload.business;

import java.util.Map;

import com.idega.presentation.IWContext;

public interface UploadAreaBean {

	public static final String BEAN_NAME = "uploadAreaBean";

	public Long getMaxFileSize(IWContext iwc);
	public void setMaxFileSize(Long size);

	public String getServletPath();

	public void setAddThumbnail(boolean useThumbnail);
	public boolean isAddThumbnail();

	public Map<String, Object> getFileResponce(String fileName, long fileSize, String path);
	public Map<String, Object> getFileResponse(String fileName, long fileSize, String path, int thumbnailSize);
}