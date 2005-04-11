package com.idega.content.business;

import java.io.IOException;
import javax.faces.component.UIComponent;
import javax.faces.event.ActionEvent;
import net.sourceforge.myfaces.custom.fileupload.UploadedFile;
import org.apache.commons.httpclient.HttpException;
import org.apache.webdav.lib.PropertyName;
import com.idega.business.IBOLookup;
import com.idega.core.file.util.MimeTypeUtil;
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
	private String comment = null;
	
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
	
	public String getComment(){
		return comment;
	}

	public void setComment(String comment) {
		this.comment = comment;
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

	public String upload(ActionEvent event) throws IOException{
		
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
			
			//FIXME THIS IS A BUG IN THE MYFACES UPLOADER I THINK
			//The problem is that in IE 6 the filename actually contains the full file path!
			//example I'm uploading test.txt from c:\myfolder\test.txt to the folder /files/public
			//then the variable filePath+fileName = /files/public/C:/myfolder/test.txt
			//workaround
			int lastBloodySlash = uploadName.lastIndexOf("\\");
			if(lastBloodySlash>-1){
				uploadName = uploadName.substring(lastBloodySlash+1);
			}
			
			lastBloodySlash = uploadName.lastIndexOf("/");
			if(lastBloodySlash>-1){
				uploadName = uploadName.substring(lastBloodySlash+1);
			}	
			//workaround ends
			
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
			
		
//			boolean createFolderSuccess = rootResource.mkcolMethod(filePath);
//			System.out.println("Creating folder success "+createFolderSuccess);
			
			boolean uploadFileSuccess = false;
			try {
				uploadFileSuccess = rootResource.putMethod(filePath+fileName,uploadFile.getInputStream());
			}
			catch (HttpException e) {
				e.printStackTrace();
			}
			catch (IOException e) {
				e.printStackTrace();
			}
			System.out.println("Uploading file success "+uploadFileSuccess);
			
			
			
			// Always refreshing/keeping status
			WFUtil.invoke("WebDAVListBean","refresh", event.getSource(), UIComponent.class);

			if(uploadFileSuccess){
				String contentType = uploadFile.getContentType();
				downloadPath = filePath+fileName;
				if(contentType!=null && MimeTypeUtil.getInstance().isImage(contentType)){
					imagePath = iwc.getIWMainApplication().getURIFromURL(downloadPath);	
				}
				
				if(comment!=null && !"".equals(comment)){
					
					rootResource.proppatchMethod(filePath+fileName,new PropertyName("DAV:","comment"),comment,true);
					
				}
			}
			else{
				System.err.println("Error code :"+rootResource.getStatusMessage()+", message: "+rootResource.getStatusMessage());
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