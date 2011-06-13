package com.idega.content.upload.servlet;

import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.Iterator;
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
import com.idega.content.business.ContentUtil;
import com.idega.content.themes.helpers.business.ThemesConstants;
import com.idega.content.upload.bean.UploadFile;
import com.idega.content.upload.business.FileUploadProgressListener;
import com.idega.content.upload.business.FileUploader;
import com.idega.idegaweb.IWApplicationContext;
import com.idega.idegaweb.IWResourceBundle;
import com.idega.presentation.IWContext;
import com.idega.util.CoreConstants;
import com.idega.util.CoreUtil;
import com.idega.util.FileUtil;
import com.idega.util.IOUtil;
import com.idega.util.ListUtil;
import com.idega.util.StringHandler;
import com.idega.util.StringUtil;
import com.idega.util.expression.ELUtil;

public class ContentFileUploadServlet extends HttpServlet {

	private static Logger LOGGER = Logger.getLogger(ContentFileUploadServlet.class.getName());
	
	private static final long serialVersionUID = -6282517406996613536L;
	
	public static final long MAX_UPLOAD_SIZE = 1024 * 1024 * 1024;	//	1 GB

	@SuppressWarnings("unchecked")
	@Override
	protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		boolean success = false;
		String uploadId = null;
		IWApplicationContext iwac = null;
		FileUploadProgressListener uploadProgressListener = null;
		try {
			IWContext iwc = new IWContext(request, response, getServletContext());
			iwac = iwc.getApplicationContext();
			
			ServletRequestContext src = new ServletRequestContext(request);
			if (!FileUploadBase.isMultipartContent(src)) {
				LOGGER.log(Level.WARNING, "Request is not multipart content, terminating upload!");
				return;
			}
			
			String uploadPath = null;
			boolean zipFile = false;
			boolean themePack = false;
			boolean extractContent = false;
			boolean stripNonRomanLetters = false;
			
			uploadProgressListener = ELUtil.getInstance().getBean(FileUploadProgressListener.class);
			
			DiskFileItemFactory factory = new DiskFileItemFactory();
			FileUploadBase fileUploadService = new FileUpload(factory);
			
			long maxUploadSize = uploadProgressListener.getMaxSize();
			maxUploadSize = maxUploadSize <= 0 ? MAX_UPLOAD_SIZE : maxUploadSize;
			maxUploadSize = maxUploadSize == MAX_UPLOAD_SIZE ? maxUploadSize : Double.valueOf(maxUploadSize * 1.1).longValue();	//	10% reserved for other data when file(s)
			fileUploadService.setSizeMax(maxUploadSize);
			fileUploadService.setProgressListener(uploadProgressListener);
	
			uploadId = uploadProgressListener.getUploadId();
			if (!StringUtil.isEmpty(uploadId)) {
				iwac.setApplicationAttribute(uploadId, Boolean.TRUE);
			}
			
			if (IOUtil.isUploadExceedingLimits(request, maxUploadSize)) {
				IWResourceBundle iwrb = ContentUtil.getBundle().getResourceBundle(iwc);
				writeToResponse(response, "error=".concat(iwrb.getLocalizedString("uploader_error_exceeds_max_size", "The file you are uploading is exceeding the limits"))
						.concat(": ").concat(FileUtil.getHumanReadableSize(maxUploadSize)));
				uploadProgressListener.markFailedUpload(uploadId);
				finishUpUpload(iwac, uploadProgressListener, uploadId, false);
				return;
			}
			
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
	        		} else if (fieldName.equals(ContentConstants.UPLOADER_STRIP_NON_ROMAN_LETTERS)) {
	        			stripNonRomanLetters = getValueFromString(getValueFromBytes(file.get()));
	        		}
	        	}
	        }
	        
	        if (ListUtil.isEmpty(files)) {
	        	LOGGER.log(Level.WARNING, "No files to upload, terminating upload!");
	        	return;
	        }
	        
	        if (!StringUtil.isEmpty(uploadId)) {
	        	iwac.setApplicationAttribute(uploadId, Boolean.TRUE);
	        }
	        
	        String errorMessage = null;
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
		    	
		    	if (stripNonRomanLetters) {
		    		uploadPath = getStripped(uploadPath);
		    		prepareFiles(files);
		    	}
		    	
		        boolean isIE = CoreUtil.isIE(request);
		        if (!(success = upload(files, uploadPath, zipFile, themePack, isIE, extractContent))) {
		        	success = upload(files, uploadPath, zipFile, themePack, isIE, extractContent);	//	Re-uploading in case of error
		        }
		        
		        if (success) {
		        	StringBuffer responseBuffer = new StringBuffer();
		        	for (Iterator<UploadFile> filesIter = files.iterator(); filesIter.hasNext();) {
		        		responseBuffer.append(filesIter.next().getName());
		        		if (filesIter.hasNext()) {
		        			responseBuffer.append(CoreConstants.COMMA);
		        		}
		        	}
		        	writeToResponse(response, responseBuffer.toString());
		        } else {
		        	errorMessage = "Unable to upload files (" + files + ") to: " + uploadPath + ". Upload ID: " + uploadId;
		        	throw new RuntimeException(errorMessage);
		        }
	        } catch(Exception e) {
	        	errorMessage = errorMessage == null ? "Files uploader failed! Unable to upload files: " + files + " to: " + uploadPath + ". Upload ID: " + uploadId : errorMessage;
	        	LOGGER.log(Level.SEVERE, errorMessage, e);
	        	CoreUtil.sendExceptionNotification(errorMessage, e);
	        } finally {
	        	finishUpUpload(iwac, uploadProgressListener, uploadId, success);
	        }
		} finally {
			if (!success && uploadProgressListener != null) {
				uploadProgressListener.markFailedUpload(uploadId);
				finishUpUpload(iwac, uploadProgressListener, uploadId, success);
			}
		}
	}
	
	private void finishUpUpload(IWApplicationContext iwac, FileUploadProgressListener uploadProgressListener, String uploadId, boolean success) {
		if (!StringUtil.isEmpty(uploadId)) {
			uploadProgressListener.setUploadSuccessful(uploadId, success);
    		
    		if (iwac != null)
    			iwac.removeApplicationAttribute(uploadId);
    	}
	}
	
	private void writeToResponse(HttpServletResponse response, String responseText) throws IOException {
		StringBuffer responseBuffer = new StringBuffer("web2FilesUploaderFilesListStarts").append(responseText);
		response.setCharacterEncoding(CoreConstants.ENCODING_UTF8);
    	response.getWriter().print(responseBuffer.toString());
	}
	
	private boolean upload(List<UploadFile> files, String uploadPath, boolean zipFile, boolean themePack, boolean isIE, boolean extractContent) {
		try {
			FileUploader uploader = ELUtil.getInstance().getBean(FileUploader.class);
			if (zipFile || themePack) {
	        	if (themePack) {
	        		uploadPath = ThemesConstants.THEMES_PATH;
	        		return uploader.uploadThemePack(files, uploadPath, isIE);
	        	} else {
	        		return uploader.uploadZipFile(files, uploadPath, extractContent, isIE);
	        	}
	        } else {
	        	return uploader.uploadFile(files, uploadPath, isIE);
	        }
		} catch (Exception e) {
			String message = "Error uploading files (" + files + ") to: " + uploadPath;
			LOGGER.log(Level.WARNING, message, e);
			CoreUtil.sendExceptionNotification(message, e);
		}
		return false;
	}
	
	private void prepareFiles(List<UploadFile> files) {
		for (UploadFile file: files) {
			file.setName(getStripped(file.getName()));
		}
	}
	
	private String getStripped(String input) {
		if (StringUtil.isEmpty(input))
			return input;
		
		return StringHandler.stripNonRomanCharacters(input, ContentConstants.UPLOADER_EXCEPTIONS_FOR_LETTERS);
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
			LOGGER.log(Level.WARNING, "Unable to use encoding " + CoreConstants.ENCODING_UTF8, e);
			
			return new String(bytes);
		}
	}
	
	private boolean getValueFromString(String value) {
		return Boolean.TRUE.toString().equals(value);
	}
}