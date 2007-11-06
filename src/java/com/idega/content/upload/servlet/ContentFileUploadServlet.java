package com.idega.content.upload.servlet;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.fileupload.FileItem;
import org.apache.commons.fileupload.disk.DiskFileItemFactory;
import org.apache.commons.fileupload.servlet.ServletFileUpload;

import com.idega.business.SpringBeanLookup;
import com.idega.content.business.ContentConstants;
import com.idega.content.themes.helpers.ThemesConstants;
import com.idega.content.upload.bean.UploadFile;
import com.idega.content.upload.business.FileUploader;
import com.idega.util.CoreUtil;

public class ContentFileUploadServlet extends HttpServlet {

	private static final long serialVersionUID = -6282517406996613536L;

	@SuppressWarnings("unchecked")
	protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		String uploadPath = null;
		boolean zipFile = false;
		boolean themePack = false;
		boolean extractContent = false;
		
		File location = new File(System.getProperty("java.io.tmpdir"));
        DiskFileItemFactory itemFactory = new DiskFileItemFactory(256 * 1024, location);
        ServletFileUpload fileUploader = new ServletFileUpload(itemFactory);
        
        //	TODO: implement ProgressListener - it is a base for progress bar. This needs file upload 1.2
        /*fileUploader.setProgressListener(new ProgressListener() {
			public void update(long bytesRead, long contentLength, int items) {
			}
		});*/

        List<FileItem> fileItems = null;
        try {
			fileItems = fileUploader.parseRequest(request);
		} catch (Exception e) {
			e.printStackTrace();
		}
        if (fileItems == null) {
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
        	boolean isIE = CoreUtil.isIE(request);
        	FileUploader uploader = (FileUploader) SpringBeanLookup.getInstance().getSpringBean(request.getSession(), FileUploader.class);
        	
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
