package com.idega.content.upload.servlet;

import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

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

import com.idega.content.business.ContentConstants;
import com.idega.content.themes.helpers.business.ThemesConstants;
import com.idega.content.upload.bean.UploadFile;
import com.idega.content.upload.business.FileUploadProgressListener;
import com.idega.content.upload.business.FileUploader;
import com.idega.idegaweb.IWApplicationContext;
import com.idega.idegaweb.IWMainApplication;
import com.idega.util.CoreConstants;
import com.idega.util.CoreUtil;
import com.idega.util.ListUtil;
import com.idega.util.StringUtil;
import com.idega.util.expression.ELUtil;

public class ContentFileUploadServlet extends HttpServlet {

	private static Logger LOGGER = Logger.getLogger(ContentFileUploadServlet.class.getName());
	
	private static final long serialVersionUID = -6282517406996613536L;	
	private static final long MAX_UPLOAD_SIZE = 1024 * 1024 * 1024;	//	1 GB

	@SuppressWarnings("unchecked")
	@Override
	protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		ServletRequestContext src = new ServletRequestContext(request);
		if (!FileUploadBase.isMultipartContent(src)) {
			LOGGER.log(Level.WARNING, "Request is not multipart content, terminating upload!");
			return;
		}
		
		String uploadPath = null;
		boolean zipFile = false;
		boolean themePack = false;
		boolean extractContent = false;
		
		FileUploadProgressListener uploadProgressListner = ELUtil.getInstance().getBean(FileUploadProgressListener.class);
		
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
		if (ListUtil.isEmpty(fileItems)) {
			LOGGER.log(Level.WARNING, "No files to upload, terminating upload!");
			return;
		}
		
		String uploadId = null;
		String fieldName = null;
		List<UploadFile> files = new ArrayList<UploadFile>();
		for (FileItem file: fileItems) {
			fieldName = file.getFieldName();
			if (!StringUtil.isEmpty(fieldName)) {
        		if (file.getSize() > 0 && fieldName.equals(ContentConstants.UPLOAD_FIELD_NAME)) {
        			files.add(new UploadFile(file.getName(), file.getContentType(), file.getSize(), file.get()));
        		} else if (fieldName.equals(ContentConstants.UPLOADER_PATH)) {
        			uploadPath = getValueFromBytes(file.get());
        		} else if (fieldName.equals(ContentConstants.UPLOADER_UPLOAD_ZIP_FILE)) {
        			zipFile = getValueFromString(getValueFromBytes(file.get()));
        		} else if (fieldName.equals(ContentConstants.UPLOADER_UPLOAD_THEME_PACK)) {
        			themePack = getValueFromString(getValueFromBytes(file.get()));
        		} else if (fieldName.equals(ContentConstants.UPLOADER_UPLOAD_EXTRACT_ARCHIVED_FILE)) {
        			extractContent = getValueFromString(getValueFromBytes(file.get()));
        		} else if (fieldName.equals(ContentConstants.UPLOADER_UPLOAD_IDENTIFIER)) {
        			uploadId = getValueFromBytes(file.get());
        		}
        	}
        }
        
        if (ListUtil.isEmpty(files)) {
        	LOGGER.log(Level.WARNING, "No files to upload, terminating upload!");
        	return;
        }
        
        IWApplicationContext iwac = IWMainApplication.getDefaultIWApplicationContext();
        if (!StringUtil.isEmpty(uploadId) && iwac != null) {
        	iwac.setApplicationAttribute(uploadId, Boolean.TRUE);
        }
        
        String errorMessage = null;
        boolean success = false;
        try {
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
	        FileUploader uploader = ELUtil.getInstance().getBean(FileUploader.class);
	        
	        if (zipFile || themePack) {
	        	if (themePack) {
	        		uploadPath = ThemesConstants.THEMES_PATH;
	        		success = uploader.uploadThemePack(files, uploadPath, isIE);
	        	} else {
	        		success = uploader.uploadZipFile(files, uploadPath, extractContent, isIE);
	        	}
	        } else {
	        	success = uploader.uploadFile(files, uploadPath, isIE);
	        }
	        
	        if (!success) {
	        	errorMessage = "Unable to upload files (" + files + ") to: " + uploadPath + ". Upload ID: " + uploadId;
	        	throw new RuntimeException(errorMessage);
	        }
        } catch(Exception e) {
        	LOGGER.log(Level.SEVERE, errorMessage == null ? "Files uploader failed! Unable to upload files: " + files + " to: " + uploadPath + ". Upload ID: " + uploadId :
        		errorMessage, e);
        	CoreUtil.sendExceptionNotification(e);
        } finally {
        	if (!StringUtil.isEmpty(uploadId)) {
        		uploadProgressListner.setUploadSuccessful(uploadId, success);
        		
        		if (iwac != null)
        			iwac.removeApplicationAttribute(uploadId);
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
		
		try {
			return new String(bytes, CoreConstants.ENCODING_UTF8);
		} catch (UnsupportedEncodingException e) {
			LOGGER.log(Level.WARNING, "Unable to use UTF-8", e);
			
			return new String(bytes);
		}
	}
	
	private boolean getValueFromString(String value) {
		return Boolean.TRUE.toString().equals(value);
	}
}