package com.idega.content.business;

import java.io.BufferedInputStream;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.Serializable;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

import javax.faces.component.UIComponent;
import javax.faces.event.ActionEvent;

import org.apache.myfaces.custom.fileupload.UploadedFile;
import org.apache.commons.httpclient.HttpException;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.webdav.lib.PropertyName;
import com.idega.business.IBOLookup;
import com.idega.business.IBOLookupException;
import com.idega.core.file.util.MimeTypeUtil;
import com.idega.io.ZipInstaller;
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
			if(tempUploadFolderPath!=null){
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
			}
			else{
				log.error("Error code :"+rootResource.getStatusMessage()+", message: "+rootResource.getStatusMessage());
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
	
	/**
	 * Uploads zip file's contents to slide. Note: only *.zip file allowed!
	 * @param event: ActionEvent
	 * @return result: success (true) or failure (false) while uploading file
	 * @throws IOException
	 */
	public boolean uploadZipFileContents(ActionEvent event) throws IOException {
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
		result = doZipUploading(new ZipInputStream(new BufferedInputStream(uploadFile.getInputStream())), getUploadFilePath());
		log.info("Success uploading zip file's contents: " + result);
		return result;
	}
	
	/**
	 * Uploads zip file's contents to slide. Note: only *.zip file allowed!
	 * @param zipInputStream: a stream to read the file and its content from
	 * @param uploadPath: a path in slide where to store files (for example: "/files/public/")
	 * @return result: success (true) or failure (false) while uploading file
	 * @throws IOException
	 */
	public boolean doZipUploading(ZipInputStream zipInputStream, String uploadPath) throws IOException {
		// Checking if parameters are valid
		boolean result = (uploadPath == null || "".equals(uploadPath)) ? false : true;
		if (!result) {
			log.error("Invalid upload path!");
			return result;
		}
		result = zipInputStream == null ? false : true;
		if (!result) {
			log.error("ZipInputStream is closed!");
			return result;
		}
		
		IWSlideService service = null;
		try {
			service = (IWSlideService) IBOLookup.getServiceInstance(IWContext.getInstance(), IWSlideService.class);
		} catch (IBOLookupException e) {
			log.error(e);
		}
		result = service == null ? false : true;
		if (!result) {
			log.info("Unable to get IWSlideServiceBean instance.");
			return result;
		}
		ZipEntry entry = null;
		ZipInstaller zip = new ZipInstaller();
		ByteArrayOutputStream memory = null;
		InputStream is = null;
		try {
			while ((entry = zipInputStream.getNextEntry()) != null && result) {
				memory = new ByteArrayOutputStream();
				zip.writeFromStreamToStream(zipInputStream, memory);
				is = new ByteArrayInputStream(memory.toByteArray());
				result = service.uploadFileAndCreateFoldersFromStringAsRoot(uploadPath, entry.getName(), is, null, true);
				memory.close();
				is.close();
				zip.closeEntry(zipInputStream);
			}
		} catch (IOException e) {
			log.error(e);
			result = false;
		}
		zip.closeEntry(zipInputStream);
		return result;
	}

}