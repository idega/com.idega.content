package com.idega.content.business;

import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.Serializable;
import java.rmi.RemoteException;
import java.util.zip.ZipInputStream;

import javax.faces.component.UIComponent;
import javax.faces.component.html.HtmlCommandButton;
import javax.faces.event.ActionEvent;

import org.apache.myfaces.custom.fileupload.UploadedFile;
import org.apache.commons.httpclient.HttpException;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.webdav.lib.PropertyName;
import com.idega.business.IBOLookup;
import com.idega.business.IBOLookupException;
import com.idega.content.themes.helpers.ThemesHelper;
import com.idega.core.file.util.MimeTypeUtil;
import com.idega.presentation.IWContext;
import com.idega.slide.business.IWSlideService;
import com.idega.slide.business.IWSlideSession;
import com.idega.slide.util.WebdavRootResource;
import com.idega.webface.WFUtil;

public class WebDAVUploadBean implements Serializable{

	private static final long serialVersionUID = -1760819218959402747L;
	private static final Log log = LogFactory.getLog(WebDAVUploadBean.class);
	private static String DEFAULT_PATH = "/files/documents/";
	private UploadedFile uploadFile;
	private String name = "";
	private String uploadFilePath = DEFAULT_PATH;
	private String downloadPath = null;
	private String imagePath = null;
	private String comment = null;
	private Boolean uploadSuccessful = null;
	private String uploadMessage = null;
	private String redirectOnSuccessURI = null;
	
	public UploadedFile getUploadFile() {
		return this.uploadFile;
	}

	public void setUploadFile(UploadedFile uploadFile){
		this.uploadFile = uploadFile;
	}

	public String getFileName(){
		return this.name;
	}

	public void setFileName(String name) {
		this.name = name;
	}
	
	public String getComment(){
		return this.comment;
	}

	public void setComment(String comment) {
		this.comment = comment;
	}
	
	public String getUploadFilePath(){
		if(!this.uploadFilePath.endsWith("/")){
			this.uploadFilePath+="/";
		}
		return this.uploadFilePath;
	}

	public void setUploadFilePath(String uploadFolderPath) {
		this.uploadFilePath = uploadFolderPath;
	}

	public String upload(ActionEvent event) throws IOException{
		
		if(this.uploadFile!=null){
			IWContext iwc = IWContext.getInstance();
//			Map parameters = ((HttpServletRequest) FacesContext.getCurrentInstance().getExternalContext().getRequest()).getParameterMap();
//			uploadFolderPath = ((String[])parameters.get("uploadForm:uploadPath"))[0];
			
			String tempUploadFolderPath = (String) WFUtil.invoke("WebDAVListBean","getWebDAVPath");
			if(tempUploadFolderPath!=null && !tempUploadFolderPath.equals("")){
				this.uploadFilePath = tempUploadFolderPath;
			}
			
			IWSlideSession session = (IWSlideSession)IBOLookup.getSessionInstance(iwc,IWSlideSession.class);
			IWSlideService service = (IWSlideService)IBOLookup.getServiceInstance(iwc,IWSlideService.class);
	
			WebdavRootResource rootResource = session.getWebdavRootResource();
			String filePath = service.getWebdavServerURI()+getUploadFilePath();
			String uploadName = this.uploadFile.getName();
			
			//FIXME THIS IS A BUG IN THE MYFACES UPLOADER I THINK
			//The problem is that in IE 6 the filename actually contains the full file path!
			//example I'm uploading test.txt from c:\myfolder\test.txt to the folder /files/public
			//then the variable filePath+fileName = /files/public/C:/myfolder/test.txt
			//workaround
			int lastBloodySlash = uploadName.lastIndexOf("\\");
			if(lastBloodySlash>-1){
				uploadName = uploadName.substring(lastBloodySlash+1);
			}
			//workaround ends
			
			String fileName = uploadName;
			
			if(!"".equals(this.name)){
				fileName = this.name;
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
				uploadFileSuccess = rootResource.putMethod(filePath+fileName,this.uploadFile.getInputStream());
			}
			catch (HttpException e) {
				e.printStackTrace();
			}
			catch (IOException e) {
				e.printStackTrace();
			}
			log.info("Uploading file success "+uploadFileSuccess);
			
			
			
			// Always refreshing/keeping status
			WFUtil.invoke("WebDAVListBean","refresh", event.getSource(), UIComponent.class);

			if(uploadFileSuccess){
				String contentType = this.uploadFile.getContentType();
				this.downloadPath = filePath+fileName;
				if(contentType!=null && MimeTypeUtil.getInstance().isImage(contentType)){
					this.imagePath = iwc.getIWMainApplication().getURIFromURL(this.downloadPath);	
				}
				
				if(this.comment!=null && !"".equals(this.comment)){
					
					rootResource.proppatchMethod(filePath+fileName,new PropertyName("DAV:","comment"),this.comment,true);
					
				}
				uploadSuccessful = new Boolean(true);
				uploadMessage = rootResource.getStatusMessage();
			}
			else{
				uploadSuccessful = new Boolean(false);
				uploadMessage = rootResource.getStatusMessage();
				log.error("Error code :"+rootResource.getStatusMessage()+", message: "+rootResource.getStatusMessage());
				return "upload_failed";
			}
			
			if (uploadFileSuccess && redirectOnSuccessURI != null) {
				IWContext.getInstance().sendRedirect(redirectOnSuccessURI);
			}
		
		}
		
		return "ok";
	}

	/**
	 * This does a webdav query
	 * @return
	 */
	public boolean getIsUploaded(){
		
		if(this.downloadPath!=null){
			IWSlideSession session;
			try {
				session = (IWSlideSession)IBOLookup.getSessionInstance(IWContext.getInstance(),IWSlideSession.class);

				return session.getExistence(this.downloadPath);
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
		return this.downloadPath;
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
		return this.imagePath;
	}
	/**
	 * @param imagePath The imagePath to set.
	 */
	public void setImagePath(String imagePath) {
		this.imagePath = imagePath;
	}
	
	public String getUploadMessage() {
		return uploadMessage;
	}

	public void setUploadMessage(String uploadMessage) {
		this.uploadMessage = uploadMessage;
	}

	public Boolean isUploadSuccessful() {
		return uploadSuccessful;
	}

	public boolean wasUploadAttemped() {
		return uploadSuccessful != null;
	}
	
	public void setRedirectOnSuccessURI(String uri) {
		this.redirectOnSuccessURI = uri;
	}
	
	/**
	 * Uploads zip file's contents to slide. Note: only *.zip file allowed!
	 * @param event: ActionEvent
	 * @return result: success (true) or failure (false) while uploading file
	 * @throws IOException
	 */
	public boolean uploadZipFileContents(ActionEvent event) throws IOException {
		if (canUploadZipFile()) {
			return doUploading(isBeingUploadedTheme(event));
		}		
		log.info("Unable to upload zip file's contents");
		return false;
	}
	
	private boolean isBeingUploadedTheme(ActionEvent event) {
		boolean theme = false;
		if (event == null) {
			return theme;
		}
		if (event.getSource() instanceof HtmlCommandButton) {
			HtmlCommandButton button = (HtmlCommandButton) event.getSource();
			if (button.getId() != null) {
				if (button.getId().toLowerCase().indexOf("uploadtheme") != -1) {
					theme = true;
				}
			}
		}
		return theme;
	}
	
	private boolean canUploadZipFile() {
		// Got a file to upload?
		boolean result = uploadFile == null ? false : true;
		if (!result) {
			log.error("No file to upload");
			return result;
		}
		// Is it a *.zip file?
		result = uploadFile.getName().toLowerCase().endsWith(".zip");
		if (!result) {
			log.error("Only zip file accepting!");
			return result;
		}
		return result;
	}
	
	private boolean doUploading (boolean uploadingTheme) throws IOException {
		IWSlideService service = null;
		try {
			service = (IWSlideService) IBOLookup.getServiceInstance(IWContext.getInstance(), IWSlideService.class);
		} catch (IBOLookupException e) {
			log.info("Unable to get IWSlideServiceBean instance.");
			log.error(e);
			return false;
		}
		
		String resultInfo = null;
		boolean result;
		
		changeFileUploadPath(service);
		String path = getUploadFilePath();
		if (uploadingTheme) {
			ThemesHelper.getInstance(false).addThemeToQueue(path);
		}
		
		if (service.uploadZipFileContents(new ZipInputStream(new BufferedInputStream(uploadFile.getInputStream())), path)) {
			resultInfo = "Success uploading zip file's contents";
			result = true;
		}
		else {
			resultInfo = "Unable to upload zip file's contents";
			result = false;
		}
		
		if (uploadingTheme) {
			ThemesHelper.getInstance(false).removeThemeFromQueue(path);
		}
		
		log.info(resultInfo);
		return result;
	}
	
	private void changeFileUploadPath(IWSlideService service) {
		String changedPath = getUploadFilePath() + uploadFile.getName().substring(0, uploadFile.getName().lastIndexOf("."));
		try {
			if (service.getExistence(changedPath)) {
				int i = 1;
				String tempPath = changedPath + i;
				while (service.getExistence(tempPath)) {
					i++;
					tempPath = changedPath + i;
				}
				changedPath = tempPath;
			}
		} catch (RemoteException e) {
			log.error(e);
		} catch (HttpException e) {
			log.error(e);
		} catch (IOException e) {
			log.error(e);
		}
		uploadFilePath = ThemesHelper.getInstance(false).removeSpaces(changedPath);
	}

}