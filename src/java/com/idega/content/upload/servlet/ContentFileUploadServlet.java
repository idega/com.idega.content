package com.idega.content.upload.servlet;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.fileupload.FileItem;
import org.apache.commons.fileupload.FileUpload;
import org.apache.commons.fileupload.FileUploadBase;
import org.apache.commons.fileupload.FileUploadException;
import org.apache.commons.fileupload.disk.DiskFileItemFactory;
import org.apache.commons.fileupload.servlet.ServletRequestContext;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.idega.business.SpringBeanLookup;
import com.idega.content.business.ContentConstants;
import com.idega.content.themes.helpers.business.ThemesConstants;
import com.idega.content.upload.bean.UploadFile;
import com.idega.content.upload.business.FileUploadProgressListener;
import com.idega.content.upload.business.FileUploader;
import com.idega.util.CoreConstants;
import com.idega.util.CoreUtil;

public class ContentFileUploadServlet extends HttpServlet {

	private static Log log = LogFactory.getLog(ContentFileUploadServlet.class);
	
	private static final long serialVersionUID = -6282517406996613536L;	
	private static final long MAX_UPLOAD_SIZE = 1024 * 1024 * 1024;

	@Override
	protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		ServletRequestContext src = new ServletRequestContext(request);
		if (!FileUploadBase.isMultipartContent(src)) {
			log.error("Request is not multipart content!");
			return;
		}
		
		String uploadPath = null;
		boolean zipFile = false;
		boolean themePack = false;
		boolean extractContent = false;
		
		FileUploadProgressListener uploadProgressListner = null;
		try {
			uploadProgressListner = SpringBeanLookup.getInstance().getSpringBean(request.getSession(), FileUploadProgressListener.class);
		} catch(Exception e) {
			e.printStackTrace();
			return;
		}
		
		DiskFileItemFactory factory = new DiskFileItemFactory();
		FileUploadBase fileUploadService = new FileUpload(factory);
		fileUploadService.setSizeMax(MAX_UPLOAD_SIZE);
		fileUploadService.setProgressListener(uploadProgressListner);

		List<FileItem> fileItems = null;
		try {
			fileItems = fileUploadService.parseRequest(src);
		} catch (FileUploadException e) {
			e.printStackTrace();
			return;
		}
		if (fileItems == null || fileItems.size() == 0) {
			log.info("No files to upload!");
        	return;
        }
        
        List<UploadFile> files = new ArrayList<UploadFile>();
        FileItem file = null;
        String fieldName = null;
        for (int i = 0; i < fileItems.size(); i++) {
        	file = fileItems.get(i);
        	fieldName = file.getFieldName();
        	if (fieldName != null) {
        		if (fieldName.equals(ContentConstants.UPLOAD_FIELD_NAME)) {
        			files.add(new UploadFile(file.getName(), file.getContentType(), file.getSize(), file.get()));
        		}
        		else if (fieldName.equals(ContentConstants.UPLOADER_PATH)) {
        			uploadPath = getValueFromBytes(file.get());
        		}
        		else if (fieldName.equals(ContentConstants.UPLOADER_UPLOAD_ZIP_FILE)) {
        			zipFile = getValueFromString(getValueFromBytes(file.get()));
        		}
        		else if (fieldName.equals(ContentConstants.UPLOADER_UPLOAD_THEME_PACK)) {
        			themePack = getValueFromString(getValueFromBytes(file.get()));
        		}
        		else if (fieldName.equals(ContentConstants.UPLOADER_UPLOAD_EXTRACT_ARCHIVED_FILE)) {
        			extractContent = getValueFromString(getValueFromBytes(file.get()));
        		}
        	}
        }
        
        if (files.size() > 0) {
        	//	Checking upload path
        	if (uploadPath == null) {
        		//	Using default upload path
        		uploadPath = CoreConstants.PUBLIC_PATH + CoreConstants.SLASH;
        	}
        	if (!uploadPath.startsWith(CoreConstants.SLASH)) {
    			uploadPath = CoreConstants.SLASH + uploadPath;
    		}
    		if (!uploadPath.endsWith(CoreConstants.SLASH)) {
    			uploadPath += CoreConstants.SLASH;
    		}
    		
        	boolean isIE = CoreUtil.isIE(request);
        	FileUploader uploader = SpringBeanLookup.getInstance().getSpringBean(request.getSession(), FileUploader.class);
        	
        	if (zipFile || themePack) {
        		if (themePack) {
        			uploadPath = ThemesConstants.THEMES_PATH;
        			uploader.uploadThemePack(files, uploadPath, isIE);
        		}
        		else {
        			uploader.uploadZipFile(files, uploadPath, extractContent, isIE);
        		}
        	}
        	else {
        		uploader.uploadFile(files, uploadPath, isIE);
        	}
        }
	}
	
	@Override
	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		super.doGet(request, response);
	}
	
	private String getValueFromBytes(byte[] bytes) {
		if (bytes == null) {
			return null;
		}
		
		return new String(bytes);
	}
	
	private boolean getValueFromString(String value) {
		return Boolean.TRUE.toString().equals(value);
	}
}
