package com.idega.content.business;

import java.io.IOException;
import net.sourceforge.myfaces.custom.fileupload.UploadedFile;
import org.apache.commons.httpclient.HttpURL;
import org.apache.webdav.lib.WebdavResource;
import com.idega.business.IBOLookup;
import com.idega.idegaweb.IWApplicationContext;
import com.idega.idegaweb.IWUserContext;
import com.idega.presentation.IWContext;
import com.idega.slide.business.IWSlideService;

public class WebDAVUploadBean{

	private static String DEFAULT_PATH = "/files/documents/";
	private UploadedFile uploadFile;
	private String name = "";
	private String uploadFolderPath = DEFAULT_PATH;
	private String downloadPath = null;
	private String imagePath = null;
	
	public UploadedFile getUploadFile() {
		return uploadFile;
	}

	public void setUploadFile(UploadedFile uploadFile){
		this.uploadFile = uploadFile;
	}

	public String getFileName(){
		return name;
	}

	public void setFileName(String name) {
		this.name = name;
	}
	
	public String getUploadFilePath(){
		return uploadFolderPath;
	}

	public void setUploadFilePath(String uploadFolderPath) {
		this.uploadFolderPath = uploadFolderPath;
	}

	public String upload() throws IOException{
		
		IWUserContext iwuc = IWContext.getInstance();
		IWApplicationContext iwac = iwuc.getApplicationContext();
		
		//IWSlideSession session = (IWSlideSession)IBOLookup.getSessionInstance(iwuc,IWSlideSession.class);

		IWSlideService service = (IWSlideService)IBOLookup.getServiceInstance(iwac,IWSlideService.class);
	
		System.out.println("webdavServerURL = "+service.getWebdavServerURL());
		System.out.println("webdavServletURL = "+service.getWebdavServletURL());
	
		HttpURL root = service.getWebdavServerURL();
		root.setUserinfo("root","root");
		WebdavResource webdavResource = new WebdavResource(root);
		String filePath = service.getWebdavServletURL()+getUploadFilePath();
		String uploadName = uploadFile.getName();
		String fileName = uploadName;
		if(!"".equals(name)){
			fileName = name;
			int lastDot = uploadName.lastIndexOf(".");
			if(lastDot>0){
				//just add the suffix if it is missing
				String suffix = uploadName.substring(lastDot);
				if(!fileName.endsWith(suffix)){
					fileName+=suffix;
				}
			}
		}
		
		downloadPath = filePath+fileName;
		String contextUri = iwac.getIWMainApplication().getApplicationContextURI();
		imagePath = downloadPath.substring(downloadPath.indexOf(contextUri)+contextUri.length());
		
		boolean success = webdavResource.mkcolMethod(filePath);
		System.out.println("Creating folder success "+success);
		success = webdavResource.putMethod(filePath+fileName,uploadFile.getInputStream());
		System.out.println("Uploading file success "+success);
		
		
		return "ok";

	}

	/**
	 * This does a webdav query
	 * @return
	 */
	public boolean getIsUploaded(){
		
		if(downloadPath!=null){
			IWSlideService service;
			try {
				service = (IWSlideService)IBOLookup.getServiceInstance(IWContext.getInstance(),IWSlideService.class);
			
				HttpURL root = service.getWebdavServerURL();
				root.setUserinfo("root","root");
				WebdavResource webdavResource = new WebdavResource(root);
				webdavResource.setPath(downloadPath);
				return webdavResource.getExistence();
			}
			catch (Exception e){
				e.printStackTrace();
			}
		}
		
		return false;
		
	}
	
	
	
    protected static final String WEBDAV_SERVLET_URI = "/servlet/webdav";
	/**
	 * @return Returns the downloadPath.
	 */
	public String getDownloadPath() {
		return downloadPath;
	}
	/**
	 * @param downloadPath The downloadPath to set.
	 */
	public void setDownloadPath(String downloadPath) {
		this.downloadPath = downloadPath;
	}
	/**
	 * @return Returns the imagePath.
	 */
	public String getImagePath() {
		return imagePath;
	}
	/**
	 * @param imagePath The imagePath to set.
	 */
	public void setImagePath(String imagePath) {
		this.imagePath = imagePath;
	}
}