package com.idega.content.upload.servlet;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import javax.jcr.RepositoryException;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.fileupload.FileItem;
import org.apache.commons.fileupload.FileUploadBase.FileSizeLimitExceededException;
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
import com.idega.util.IWTimestamp;
import com.idega.util.StringHandler;
import com.idega.util.StringUtil;
import com.idega.util.expression.ELUtil;

public class BlueimpUploadServlet extends HttpServlet implements UploadServlet {

	private static final long serialVersionUID = 3816385155256905555L;

	private UploadAreaBean uploadAreaBean = null;

	@Autowired
	private RepositoryService repository;

	private String deleteUrlBase = null;

	public UploadAreaBean getUploadAreaBean() {
		if (uploadAreaBean == null) {
			uploadAreaBean = ELUtil.getInstance().getBean(UploadAreaBean.BEAN_NAME);
		}
		return uploadAreaBean;
	}

	private RepositoryService getRepositoryService() {
		if (repository == null) {
			ELUtil.getInstance().autowire(this);
		}
		return repository;
	}

	@Override
	public String getServletPath() {
		return getUploadAreaBean().getServletPath();
	}

	@Override
	public Long getMaxFileSize(IWContext iwc) {
		Long maxSize = getUploadAreaBean().getMaxFileSize(iwc);
		return maxSize;
	}

	@Override
	protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		doPost(request, response);
	}

	@Override
	protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		IWContext iwc = new IWContext(request, response, getServletContext());
		response = iwc.getResponse();
		response.setContentType("application/json");
		PrintWriter responseWriter = response.getWriter();

		DiskFileItemFactory fileItemFactory = new DiskFileItemFactory();
		fileItemFactory.setSizeThreshold(getMaxFileSize(iwc).intValue());

		Long maxSize = getMaxFileSize(iwc);
		ServletFileUpload uploadHandler = new ServletFileUpload(fileItemFactory);
		if (maxSize != null) {
			uploadHandler.setFileSizeMax(maxSize);
		}
		List<Map<String, Object>> responseMapArray = null;
		try {
			@SuppressWarnings("unchecked")
			List<FileItem> items = uploadHandler.parseRequest(request);

			// Parse parameters and files
			Map<String, String> parameters = new HashMap<>(items.size());
			List<FileItem> files = new ArrayList<>(items.size());
			for (FileItem item: items) {
				if (item.isFormField()) {
					parameters.put(item.getFieldName(), item.getString());
				} else {
					files.add(item);
				}
			}

			responseMapArray = uploadFiles(files, parameters, iwc);

			Gson gson = new Gson();
			String jsonString =  gson.toJson(responseMapArray);

			responseWriter.print(jsonString);
			response.setStatus(HttpServletResponse.SC_OK);
			response.flushBuffer();
			return;
		} catch (FileSizeLimitExceededException e){
			log("File is too large", e);
			response.sendError(413);
		} catch (FileUploadException ex) {
			log("Error encountered while parsing the request", ex);
			response.sendError(500);
		} catch (Exception ex) {
			log("Error encountered while uploading file", ex);
			response.sendError(500);
		}
	}

	protected List<Map<String, Object>> uploadFiles(List<FileItem> files, Map<String, String> parameters, IWContext iwc) throws Exception {
		List<Map<String, Object>> responseMapArray = null;
		String uploadPath = parameters.get(PARAMETER_UPLOAD_PATH);
		if (StringUtil.isEmpty(uploadPath)) {
			IWTimestamp now = IWTimestamp.RightNow();
			uploadPath = CoreConstants.WEBDAV_SERVLET_URI + CoreConstants.PUBLIC_PATH + CoreConstants.SLASH + "images/" +
					now.getDateString(IWTimestamp.DATE_PATTERN) + CoreConstants.SLASH + now.getDateString("HHmmssS") + CoreConstants.SLASH;
		}
		String thumbnailSizeParam = parameters.get("idega-blueimp-thumbnails-size");
		String isAddThumbnail = parameters.get(PARAMETER_UPLOAD_ADD_THUMBNAIL);
		int thumbnailSize = StringHandler.isNumeric(thumbnailSizeParam) ? Integer.valueOf(thumbnailSizeParam) : ThumbnailService.THUMBNAIL_SMALL;
		if (StringUtil.isEmpty(uploadPath)) {
			uploadPath = CoreConstants.WEBDAV_SERVLET_URI + CoreConstants.PUBLIC_PATH + CoreConstants.SLASH;
		} else {
			if (!uploadPath.endsWith(CoreConstants.SLASH)) {
				uploadPath = uploadPath + CoreConstants.SLASH;
			}
			if (!uploadPath.startsWith(CoreConstants.SLASH)) {
				uploadPath = CoreConstants.SLASH + uploadPath;
			}
			if (!uploadPath.startsWith(CoreConstants.WEBDAV_SERVLET_URI)) {
				uploadPath = CoreConstants.WEBDAV_SERVLET_URI + uploadPath;
			}
		}

		char[] exceptions = new char[] {'-', '_', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9','.'};
		responseMapArray = new ArrayList<>();
		boolean addPrefix = iwc.getApplicationSettings().getBoolean("blue_imp_upload.add_prefix", false);
		for (FileItem file: files) {
			String originalName = file.getName();
			String fileName = originalName;
			fileName = StringHandler.stripNonRomanCharacters(fileName, exceptions);
			if (addPrefix) {
				fileName = UUID.randomUUID().toString().concat(CoreConstants.UNDER).concat(fileName);
			}
			String pathAndName = uploadPath + fileName;
			boolean success = getRepositoryService().uploadFile(uploadPath, fileName, file.getContentType(), file.getInputStream());

			Map<String, Object> fileData = null;
			if (!StringUtil.isEmpty(isAddThumbnail) && isAddThumbnail.equalsIgnoreCase(Boolean.FALSE.toString())) {
				fileData = getUploadAreaBean().getFileResponse(fileName, file.getSize(), pathAndName, thumbnailSize, false);
			} else {
				fileData = getUploadAreaBean().getFileResponse(fileName, file.getSize(), pathAndName, thumbnailSize);
			}
			fileData.put("originalName", originalName);
			fileData.put("status", success ? "OK" : "FAILURE");
			responseMapArray.add(fileData);
		}
		return responseMapArray;

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
			deleteUrl.append(PARAMETER_UPLOAD_PATH).append(CoreConstants.EQ);
			deleteUrlBase = deleteUrl.toString();
		}
		return deleteUrlBase;
	}

	protected String getDeleteUrl(IWContext iwc, String path) {
		return getDeleteUrlBase() + path;
	}

	@Override
	protected void doDelete(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
		IWContext iwc = new IWContext(request, response, getServletContext());
		response = iwc.getResponse();
		response.setContentType("application/json");
		PrintWriter responseWriter = response.getWriter();
		String filePath = iwc.getParameter(PARAMETER_UPLOAD_PATH);

		List<Map<String, Object>> responseMapArray = new ArrayList<>(1);
		IWResourceBundle iwrb = iwc.getIWMainApplication().getBundle(ContentConstants.IW_BUNDLE_IDENTIFIER).getResourceBundle(iwc);
		Map<String,Object> fileData = new HashMap<>();
		if (StringUtil.isEmpty(filePath)) {
			fileData.put("message", iwrb.getLocalizedString("file_path_is_empty", "File path is empty"));
			fileData.put("status", "Bad Request");
			Gson gson = new Gson();
			String jsonString =  gson.toJson(responseMapArray);
			responseWriter.write(jsonString);
			return;
		}
		try {
			boolean success = getRepositoryService().deleteAsRootUser(filePath);
			fileData.put("message", success ?
										iwrb.getLocalizedString("file_deleted", "File deleted") :
										iwrb.getLocalizedString("failed_to_delete_file", "Failed to delete file")
			);
			fileData.put("status", success ? "OK" : "Failure");
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
