package com.idega.content.business;

import java.io.IOException;
import net.sourceforge.myfaces.custom.fileupload.UploadedFile;
import com.idega.business.IBOLookup;
import com.idega.presentation.IWContext;
import com.idega.slide.business.IWSlideService;
import com.idega.slide.business.IWSlideSession;
import com.idega.slide.util.WebdavRootResource;
import com.idega.webface.WFUtil;

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
		if(!uploadFolderPath.endsWith("/")){
			uploadFolderPath+="/";
		}
		return uploadFolderPath;
	}

	public void setUploadFilePath(String uploadFolderPath) {
		this.uploadFolderPath = uploadFolderPath;
	}

	public String upload() throws IOException{
		
		if(uploadFile!=null){
			IWContext iwc = IWContext.getInstance();
//			Map parameters = ((HttpServletRequest) FacesContext.getCurrentInstance().getExternalContext().getRequest()).getParameterMap();
//			uploadFolderPath = ((String[])parameters.get("uploadForm:uploadPath"))[0];
			
			String tempUploadFolderPath = (String) WFUtil.invoke("WebDAVListBean","getWebDAVPath");
			if(tempUploadFolderPath!=null){
				uploadFolderPath = tempUploadFolderPath;
			}
			
			IWSlideSession session = (IWSlideSession)IBOLookup.getSessionInstance(iwc,IWSlideSession.class);
			IWSlideService service = (IWSlideService)IBOLookup.getServiceInstance(iwc,IWSlideService.class);
	
			WebdavRootResource rootResource = session.getWebdavRootResource();
			String filePath = service.getWebdavServerURI()+getUploadFilePath();
			String uploadName = uploadFile.getName();
			//String contentType = uploadFile.getContentType();
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
			
			
			boolean success = rootResource.mkcolMethod(filePath);
			System.out.println("Creating folder success "+success);
			success = rootResource.putMethod(filePath+fileName,uploadFile.getInputStream());
			System.out.println("Uploading file success "+success);
			
			downloadPath = filePath+fileName;
			imagePath = iwc.getIWMainApplication().getURIFromURL(downloadPath);	
			
			if(success){
				WFUtil.invoke("WebDAVListBean","refresh");
			}
			else{
				return "upload_failed";
			}
		
		}
		
		return "ok";
	}

	/**
	 * This does a webdav query
	 * @return
	 */
	public boolean getIsUploaded(){
		
		if(downloadPath!=null){
			IWSlideSession session;
			try {
				session = (IWSlideSession)IBOLookup.getSessionInstance(IWContext.getInstance(),IWSlideSession.class);

				return session.getExistence(downloadPath);
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