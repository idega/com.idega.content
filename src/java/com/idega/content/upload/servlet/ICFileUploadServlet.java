package com.idega.content.upload.servlet;

import java.io.IOException;
import java.io.InputStream;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;
import java.util.logging.Level;
import java.util.logging.Logger;

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
import com.idega.core.file.data.ICFile;
import com.idega.core.file.data.ICFileHome;
import com.idega.core.file.util.MimeTypeUtil;
import com.idega.data.IDOLookup;
import com.idega.idegaweb.IWMainApplication;
import com.idega.presentation.IWContext;
import com.idega.repository.RepositoryService;
import com.idega.util.CoreConstants;
import com.idega.util.IOUtil;
import com.idega.util.IWTimestamp;
import com.idega.util.StringHandler;
import com.idega.util.expression.ELUtil;

public class ICFileUploadServlet extends HttpServlet {

	private static final long serialVersionUID = -1438789624273727352L;

	private static final Logger LOGGER = Logger.getLogger(ICFileUploadServlet.class.getName());

	private Long getMaxFileSize(IWContext iwc){
		return new Long(1024 * 1024) * 200;
	}

	@Autowired
	private RepositoryService repositoryService;

	private RepositoryService getRepositoryService() {
		if (repositoryService == null) {
			ELUtil.getInstance().autowire(this);
		}
		return repositoryService;
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
			responseMapArray = new ArrayList<>();
			ICFileHome icFileHome = (ICFileHome) IDOLookup.getHome(ICFile.class);
			InputStream stream = null;
			boolean saveInDB = IWMainApplication.getDefaultIWMainApplication().getSettings().getBoolean("ic_file_uploader.save_in_db", true);
			char[] exceptions = new char[] {'0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '.', '-', '_'};
			IWTimestamp now = IWTimestamp.RightNow();
			String basePath = CoreConstants.PUBLIC_PATH.concat("/uploads/")
								.concat(String.valueOf(now.getYear())).concat(CoreConstants.SLASH)
								.concat(String.valueOf(now.getMonth())).concat(CoreConstants.SLASH)
								.concat(String.valueOf(now.getDay())).concat(CoreConstants.SLASH)
								.concat(String.valueOf(now.getHour())).concat(CoreConstants.MINUS).concat(String.valueOf(now.getMinute())).concat(CoreConstants.SLASH)
								.concat(UUID.randomUUID().toString()).concat(CoreConstants.SLASH);
			for (FileItem file: files) {
				String fileName = file.getName();
				ICFile icFile = icFileHome.create();
				stream = file.getInputStream();

				boolean result = false;
				if (!saveInDB) {
					fileName = StringHandler.stripNonRomanCharacters(fileName, exceptions);
					String mimeType = MimeTypeUtil.resolveMimeTypeFromFileName(fileName);
					result = false;
					try {
						result = getRepositoryService().uploadFile(basePath, fileName, mimeType, stream);
					} catch (Exception e) {
						result = false;
					} finally {
						IOUtil.close(stream);
					}
					if (result) {
						icFile.setFileUri(new StringBuffer(CoreConstants.WEBDAV_SERVLET_URI).append(basePath).append(fileName).toString());
					}
				}
				if (!result || saveInDB) {
					icFile.setFileValue(stream);
				}

				icFile.setName(fileName);
				icFile.setModificationDate(IWTimestamp.getTimestampRightNow());
				icFile.setFileSize(Long.valueOf(file.getSize()).intValue());
				icFile.store();
				Map<String, Object> fileData = getFileResponce(icFile);
				responseMapArray.add(fileData);

				IOUtil.close(stream);
			}
			Gson gson = new Gson();
			String jsonString =  gson.toJson(responseMapArray);
			responseWriter.write(jsonString);
			return;
		} catch (FileSizeLimitExceededException e) {
			LOGGER.log(Level.WARNING, "File is too large", e);
			response.sendError(413);
		}
		catch (FileUploadException ex) {
			LOGGER.log(Level.WARNING, "Error encountered while parsing the request",ex);
			response.sendError(500);
		} catch (Throwable ex) {
			LOGGER.log(Level.WARNING, "Error encountered while uploading file",ex);
			response.sendError(500);
		}
	}

	public static Map<String, Object> getFileResponce(ICFile file){
		Map<String, Object> fileData = new HashMap<>();
		fileData.put("name", file.getName());
		fileData.put("size", file.getFileSize());
		fileData.put("id", file.getUniqueId());
		fileData.put("token", file.getToken());
		return fileData;
	}

}