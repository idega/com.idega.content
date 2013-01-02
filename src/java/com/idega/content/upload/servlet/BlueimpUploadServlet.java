package com.idega.content.upload.servlet;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.jcr.RepositoryException;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.fileupload.FileItem;
import org.apache.commons.fileupload.FileUploadException;
import org.apache.commons.fileupload.disk.DiskFileItemFactory;
import org.apache.commons.fileupload.servlet.ServletFileUpload;
import org.springframework.beans.factory.annotation.Autowired;

import com.google.gson.Gson;
import com.idega.content.business.ContentConstants;
import com.idega.content.business.ThumbnailService;
import com.idega.content.upload.business.UploadAreaBean;
import com.idega.idegaweb.IWResourceBundle;
import com.idega.presentation.IWContext;
import com.idega.repository.RepositoryService;
import com.idega.util.CoreConstants;
import com.idega.util.StringUtil;
import com.idega.util.expression.ELUtil;

public class BlueimpUploadServlet extends HttpServlet implements UploadServlet {

	private static final long serialVersionUID = 3816385155256905555L;

	private UploadAreaBean uploadAreaBean = null;

	@Autowired
	private RepositoryService repository;

	private String deleteUrlBase = null;

	public UploadAreaBean getUploadAreaBean() {
		if (uploadAreaBean == null)
			uploadAreaBean = ELUtil.getInstance().getBean(UploadAreaBean.BEAN_NAME);
		return uploadAreaBean;
	}

	private RepositoryService getRepositoryService() {
		if (repository == null)
			ELUtil.getInstance().autowire(this);
		return repository;
	}

	@Override
	public String getServletPath() {
		return getUploadAreaBean().getServletPath();
	}

	@Override
	public Long getMaxFileSize(IWContext iwc) {
		return getUploadAreaBean().getMaxFileSize(iwc);
	}

	@Override
	protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		IWContext iwc = new IWContext(request, response, getServletContext());
//		IWResourceBundle iwrb = iwc.getIWMainApplication().getBundle(ContentConstants.IW_BUNDLE_IDENTIFIER).getResourceBundle(iwc);
		response = iwc.getResponse();
		response.setContentType("application/json");
		PrintWriter responseWriter = response.getWriter();

		DiskFileItemFactory  fileItemFactory = new DiskFileItemFactory ();
		/*
		 *Set the size threshold, above which content will be stored on disk.
		 */
		fileItemFactory.setSizeThreshold(10*1024*1024); //10 MB
		/*
		 * Set the temporary directory to store the uploaded files of size above threshold.
		 */

		Long maxSize = getMaxFileSize(iwc);
		ServletFileUpload uploadHandler = new ServletFileUpload(fileItemFactory);
		if(maxSize != null){
			uploadHandler.setFileSizeMax(maxSize);
		}
		ArrayList<HashMap<String,Object>> responseMapArray = null;
		try {
//			Parse the request
			@SuppressWarnings("unchecked")
			List<FileItem> items = uploadHandler.parseRequest(request);

			// Parse parameters and files
			Map <String,String> parameters = new HashMap<String, String>(items.size());
			List<FileItem> files = new ArrayList<FileItem>(items.size());
			for(FileItem item : items){
				if(item.isFormField()) {
					parameters.put(item.getFieldName(), item.getString());
				} else {
					files.add(item);
				}
			}
			String uploadPath = parameters.get(PARAMETER_UPLOAD_PATH);
			if(StringUtil.isEmpty(uploadPath)){
				uploadPath = CoreConstants.PUBLIC_PATH + CoreConstants.SLASH;
			}else{
				if(!uploadPath.endsWith(CoreConstants.SLASH)){
					uploadPath = uploadPath + CoreConstants.SLASH;
				}
			}
			ThumbnailService thumbnailService = ELUtil.getInstance().getBean(ThumbnailService.BEAN_NAME);
			responseMapArray = new ArrayList<HashMap<String,Object>>(files.size());
			for(FileItem file : files){
				String fileName = file.getName();
				String pathAndName = uploadPath + fileName;
				getRepositoryService().uploadFile(uploadPath, fileName, file.getContentType(), file.getInputStream());

				HashMap<String,Object> fileData = new HashMap<String, Object>();
				fileData.put("name", fileName);
				fileData.put("size", file.getSize());
				fileData.put("url", "/content" + pathAndName);
				fileData.put("thumbnail_url", thumbnailService.getThumbnail(pathAndName, ThumbnailService.THUMBNAIL_SMALL));
				fileData.put("delete_url", getDeleteUrl(iwc, uploadPath, file));
				fileData.put("delete_type", "DELETE");
				fileData.put("message", "");
				fileData.put("status", "OK");
				responseMapArray.add(fileData);
			}
			Gson gson = new Gson();
			String jsonString =  gson.toJson(responseMapArray);

			responseWriter.write(jsonString);
		return;
		}catch(FileUploadException ex) {
			log("Error encountered while parsing the request",ex);
			response.sendError(500);
		} catch(Exception ex) {
			log("Error encountered while uploading file",ex);
			response.sendError(500);
		}
	}

	private String getDeleteUrlBase(){
		if(deleteUrlBase == null){
			String servletPath = getServletPath();
			StringBuilder deleteUrl  = new StringBuilder(servletPath);
			if(servletPath.contains(CoreConstants.QMARK)){
				deleteUrl.append(CoreConstants.AMP);
			}else{
				deleteUrl.append(CoreConstants.QMARK);
			}
			deleteUrl.append(PARAMETER_UPLOAD_PATH).append(CoreConstants.EQ);
			deleteUrlBase = deleteUrl.toString();
		}
		return deleteUrlBase;
	}

	protected String getDeleteUrl(IWContext iwc, String uploadPath, FileItem file) {
		return getDeleteUrlBase() + uploadPath + file.getName();
	}

	@Override
	protected void doDelete(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		IWContext iwc = new IWContext(request, response, getServletContext());
		response = iwc.getResponse();
		response.setContentType("application/json");
		PrintWriter responseWriter = response.getWriter();
		String filePath = iwc.getParameter(PARAMETER_UPLOAD_PATH);

		ArrayList<HashMap<String,Object>> responseMapArray = new ArrayList<HashMap<String,Object>>(1);
		IWResourceBundle iwrb = iwc.getIWMainApplication().getBundle(ContentConstants.IW_BUNDLE_IDENTIFIER).getResourceBundle(iwc);
		HashMap<String,Object> fileData = new HashMap<String, Object>();
		if(StringUtil.isEmpty(filePath)){
			fileData.put("message", iwrb.getLocalizedString("file_path_is_empty", "File path is empty"));
			fileData.put("status", "Bad Request");
			Gson gson = new Gson();
			String jsonString =  gson.toJson(responseMapArray);
			responseWriter.write(jsonString);
			return;
		}
		try {
			getRepositoryService().deleteAsRootUser(filePath);
			fileData.put("message", iwrb.getLocalizedString("file_deleted", "File deleted"));
			fileData.put("status", "OK");
			Gson gson = new Gson();
			String jsonString =  gson.toJson(responseMapArray);
			responseWriter.write(jsonString);
			return;
		} catch (Exception e) {
			log("Failed to delete file '" + filePath + "'",e);
		}
		try {
			getRepositoryService().deleteAsRootUser(filePath);
		} catch (RepositoryException e) {
			e.printStackTrace();
		}
		fileData.put("message", iwrb.getLocalizedString("error", "error"));
		fileData.put("status", "Internal Server Error");
		Gson gson = new Gson();
		String jsonString =  gson.toJson(responseMapArray);
		responseWriter.write(jsonString);
		return;
	}

}
