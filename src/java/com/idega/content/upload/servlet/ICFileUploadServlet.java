package com.idega.content.upload.servlet;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.fileupload.FileItem;
import org.apache.commons.fileupload.FileUploadBase.FileSizeLimitExceededException;
import org.apache.commons.fileupload.FileUploadException;
import org.apache.commons.fileupload.disk.DiskFileItemFactory;
import org.apache.commons.fileupload.servlet.ServletFileUpload;

import com.google.gson.Gson;
import com.idega.core.file.data.ICFile;
import com.idega.core.file.data.ICFileHome;
import com.idega.data.IDOLookup;
import com.idega.presentation.IWContext;
import com.idega.util.IWTimestamp;

public class ICFileUploadServlet  extends HttpServlet{
	private static final long serialVersionUID = -1438789624273727352L;
	
	private Long getMaxFileSize(IWContext iwc){
		return new Long(1024 * 1024) * 200;
	}
	
	private String getServletPath(){
		return "/servlet/ic-file-upload";
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
			Map<String, String> parameters = new HashMap<String, String>(items.size());
			List<FileItem> files = new ArrayList<FileItem>(items.size());
			for (FileItem item: items) {
				if (item.isFormField()) {
					parameters.put(item.getFieldName(), item.getString());
				} else {
					files.add(item);
				}
			}
			responseMapArray = new ArrayList<Map<String, Object>>();
			ICFileHome icFileHome = (ICFileHome) IDOLookup.getHome(ICFile.class);
			for (FileItem file: files) {
				String fileName = file.getName();
				ICFile icFile = icFileHome.create();
				icFile.setFileValue(file.getInputStream());
				icFile.setName(fileName);
				icFile.setModificationDate(IWTimestamp.getTimestampRightNow());
				icFile.setFileSize(Long.valueOf(file.getSize()).intValue());
				icFile.store();
				Map<String, Object> fileData = getFileResponce(icFile);
				responseMapArray.add(fileData);
			}
			Gson gson = new Gson();
			String jsonString =  gson.toJson(responseMapArray);
			responseWriter.write(jsonString);
			return;
		} catch(FileSizeLimitExceededException e){
			log("File is too large",e);
			response.sendError(413);
		}
		catch (FileUploadException ex) {
			log("Error encountered while parsing the request",ex);
			response.sendError(500);
		} catch (Exception ex) {
			log("Error encountered while uploading file",ex);
			response.sendError(500);
		}
	}
	public static Map<String, Object> getFileResponce(ICFile file){
		Map<String, Object> fileData = new HashMap<String, Object>();
		fileData.put("name", file.getName());
		fileData.put("size", file.getFileSize());
		fileData.put("id", file.getPrimaryKey());
		return fileData;
	}
}
