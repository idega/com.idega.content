package com.idega.content.upload.business;

import java.util.HashMap;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Scope;
import org.springframework.stereotype.Service;

import com.idega.content.business.ThumbnailService;
import com.idega.content.upload.servlet.BlueimpUploadServlet;
import com.idega.presentation.IWContext;
import com.idega.util.CoreConstants;

@Scope("session")
@Service(UploadAreaBean.BEAN_NAME)
public class ContentUploadAreaBean implements UploadAreaBean {

	private static final Long MAX_UPLOAD_SIZE = new Long(1024 * 1024) * 20;	//	20 MBs
	public static final String SERVLET_PATH = "/servlet/blueimp-upload";

	private Long maxFileSize = MAX_UPLOAD_SIZE;

	private String servletPath = SERVLET_PATH;

	private boolean addThumbnail = true;

	private String deleteUrlBase = null;

	@Autowired
	private ThumbnailService thumbnailService;

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

	@Override
	public void setMaxFileSize(Long maxFileSize) {
		this.maxFileSize = maxFileSize;
	}

	public void setServletPath(String servletPath) {
		this.servletPath = servletPath;
	}

	@Override
	public boolean isAddThumbnail() {
		return addThumbnail;
	}

	@Override
	public void setAddThumbnail(boolean addThumbnail) {
		this.addThumbnail = addThumbnail;
	}

	@Override
	public Map<String, Object> getFileResponce(String fileName, long fileSize, String path) {
		return getFileResponse(fileName, fileSize, path, ThumbnailService.THUMBNAIL_SMALL);
	}

	@Override
	public Map<String, Object> getFileResponse(String fileName, long fileSize, String path, int thumbnailSize) {
		return getFileResponse(fileName, fileSize, path, thumbnailSize, isAddThumbnail());
	}
	
	@Override
	public Map<String, Object> getFileResponse(String fileName, long fileSize, String path, int thumbnailSize, boolean isAddThumbnail) {
		Map<String, Object> fileData = new HashMap<String, Object>();
		fileData.put("name", fileName);
		fileData.put("size", fileSize);
		fileData.put("url", path);

		if (isAddThumbnail){
			try{
				fileData.put("thumbnail_url", thumbnailService.getThumbnail(path, thumbnailSize));
			}catch (Exception e) {
				Logger.getLogger(ContentUploadAreaBean.class.getName()).log(Level.WARNING, "Failed getting thumbnail for " + path, e);
			}
		}

		fileData.put("delete_url", getDeleteUrlBase() + path);
		fileData.put("delete_type", "DELETE");
		fileData.put("message", "");
		return fileData;
	}

	private String getDeleteUrlBase() {
		if (deleteUrlBase == null) {
			String servletPath = getServletPath();
			StringBuilder deleteUrl = new StringBuilder(servletPath);
			if (servletPath.contains(CoreConstants.QMARK)) {
				deleteUrl.append(CoreConstants.AMP);
			} else {
				deleteUrl.append(CoreConstants.QMARK);
			}
			deleteUrl.append(BlueimpUploadServlet.PARAMETER_UPLOAD_PATH).append(CoreConstants.EQ);
			deleteUrlBase = deleteUrl.toString();
		}
		return deleteUrlBase;
	}

}