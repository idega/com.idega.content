package com.idega.content.business;

import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.Serializable;
import java.rmi.RemoteException;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.zip.ZipInputStream;

import javax.faces.component.UIComponent;
import javax.faces.component.html.HtmlCommandButton;
import javax.faces.event.ActionEvent;

import org.apache.myfaces.custom.fileupload.UploadedFile;
import org.apache.webdav.lib.PropertyName;
import org.apache.webdav.lib.WebdavResource;

import com.idega.business.IBOLookup;
import com.idega.business.IBOLookupException;
import com.idega.content.bean.ContentPathBean;
import com.idega.content.presentation.WebDAVList;
import com.idega.content.themes.helpers.business.ThemesHelper;
import com.idega.core.file.util.MimeTypeUtil;
import com.idega.presentation.IWContext;
import com.idega.slide.business.IWSlideService;
import com.idega.slide.business.IWSlideSession;
import com.idega.util.CoreConstants;
import com.idega.util.CoreUtil;
import com.idega.util.FileUtil;
import com.idega.util.IOUtil;
import com.idega.util.StringUtil;
import com.idega.util.expression.ELUtil;
import com.idega.webface.WFUtil;

public class WebDAVUploadBean implements Serializable{

	private static final long serialVersionUID = -1760819218959402747L;
	private static final Logger LOGGER = Logger.getLogger(WebDAVUploadBean.class.getName());
	
	private static String DEFAULT_PATH = CoreConstants.PATH_FILES_ROOT.concat(CoreConstants.SLASH);
	private UploadedFile uploadFile;
	private String name = CoreConstants.EMPTY;
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
		if(!this.uploadFilePath.endsWith(CoreConstants.SLASH)){
			this.uploadFilePath = this.uploadFilePath.concat(CoreConstants.SLASH);
		}
		return this.uploadFilePath;
	}

	public void setUploadFilePath(String uploadFolderPath) {
		this.uploadFilePath = uploadFolderPath;
	}

	public String upload(ActionEvent event) throws IOException{
		String uploadFailed = "file_upload_failed";
		String uploadSucceeded = "file_uploaded_successfully";
		IWContext iwc = CoreUtil.getIWContext();
		if (iwc == null) {
			return uploadFailed;
		}
		if (uploadFile == null) {
			return uploadFailed;
		}
		
		String tempUploadFolderPath = (String) WFUtil.invoke(WebDAVList.WEB_DAV_LIST_BEAN_ID,"getWebDAVPath");
		if (tempUploadFolderPath != null && !tempUploadFolderPath.equals(ContentConstants.EMPTY)) {
			uploadFilePath = tempUploadFolderPath;
		}
		
		IWSlideService service = IBOLookup.getServiceInstance(iwc, IWSlideService.class);
	
		String filePath = service.getWebdavServerURI().concat(getUploadFilePath());
		String uploadName = uploadFile.getName();
		String contentType = uploadFile.getContentType();
		
		//FIXME THIS IS A BUG IN THE MYFACES UPLOADER I THINK
		//The problem is that in IE 6 the filename actually contains the full file path!
		//example I'm uploading test.txt from c:\myfolder\test.txt to the folder /files/public
		//then the variable filePath+fileName = /files/public/C:/myfolder/test.txt
		//workaround
		int lastBloodySlash = uploadName.lastIndexOf("\\");
		if(lastBloodySlash > -1) {
			uploadName = uploadName.substring(lastBloodySlash+1);
		}
		//workaround ends
			
		String fileName = uploadName;
		
		if (!ContentConstants.EMPTY.equals(this.name)) {
			fileName = this.name;
			int lastDot = uploadName.lastIndexOf(ContentConstants.DOT);
			if (lastDot > 0) {
				//just add the suffix if it is missing
				String suffix = uploadName.substring(lastDot);
				if (!fileName.endsWith(suffix)) {
					fileName += suffix;
				}
			}
		}

		long end;
		long start = System.currentTimeMillis();
		boolean uploadFileSuccess = false;
		InputStream stream = null;
		try {
			stream = uploadFile.getInputStream();
			uploadFileSuccess = service.uploadFile(filePath, fileName, contentType, stream);
		} catch (RemoteException e) {
			e.printStackTrace();
		} finally {
			end = System.currentTimeMillis();
			IOUtil.close(stream);
		}
		LOGGER.info("Uploaded (file: '".concat(filePath).concat(fileName).concat("', size: ").concat(FileUtil.getHumanReadableSize(uploadFile.getSize()))
				.concat(") successfully: ").concat(String.valueOf(uploadFileSuccess)).concat(". It took time to upload: ").concat(String.valueOf(end - start))
				.concat(" ms."));
		
		//	Setting current path to reload
		WFUtil.invoke(ContentPathBean.BEAN_ID, "setPath", uploadFilePath);
		//	Always refreshing/keeping status
		WFUtil.invoke(WebDAVList.WEB_DAV_LIST_BEAN_ID, "refresh", event.getSource(), UIComponent.class);

		downloadPath = filePath.concat(fileName);
		if (uploadFileSuccess) {
			if (contentType != null && MimeTypeUtil.getInstance().isImage(contentType)) {
				imagePath = iwc.getIWMainApplication().getURIFromURL(downloadPath);	
			}
			
			if (!StringUtil.isEmpty(comment)) {
				IWSlideSession session = IBOLookup.getSessionInstance(iwc, IWSlideSession.class);
				WebdavResource resource = session.getWebdavResource(downloadPath);
				resource.proppatchMethod(new PropertyName("DAV:", "comment"), comment, true);
			}
			
			uploadSuccessful = Boolean.TRUE;
		} else {
			uploadSuccessful = Boolean.FALSE;
			LOGGER.warning("Error uploading file '" + fileName + "' to " + filePath);
			downloadPath = null;
			return uploadFailed;
		}
		
		uploadMessage = uploadSuccessful ? uploadSucceeded : uploadFailed;
		
		if (uploadFileSuccess && redirectOnSuccessURI != null) {
			CoreUtil.getIWContext().sendRedirect(redirectOnSuccessURI);
		}
		
		return uploadMessage;
	}

	/**
	 * This does a webdav query
	 * @return
	 */
	public boolean getIsUploaded() {
		if (this.downloadPath != null) {
			try {
				IWSlideSession session = (IWSlideSession)IBOLookup.getSessionInstance(IWContext.getInstance(), IWSlideSession.class);
				return session.getExistence(this.downloadPath);
			} catch (Exception e){
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
	
	public void setWasUploadAttempted(Boolean attempt) {
		uploadSuccessful = attempt;
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
			return uploadZipFile(isBeingUploadedTheme(event));
		}		
		LOGGER.info("Unable to upload contents of zip file");
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
			LOGGER.warning("No file to upload");
			return result;
		}
		// Is it a *.zip file?
		result = uploadFile.getName().toLowerCase().endsWith(".zip");
		if (!result) {
			LOGGER.warning("Only zip file accepting!");
			return result;
		}
		return result;
	}
	
	private boolean uploadZipFile(boolean uploadingTheme) throws IOException {
		IWSlideService service = null;
		try {
			service = IBOLookup.getServiceInstance(IWContext.getInstance(), IWSlideService.class);
		} catch (IBOLookupException e) {
			LOGGER.log(Level.WARNING, "Unable to get IWSlideServiceBean instance.", e);
			return false;
		}
		
		return uploadZipFile(uploadingTheme, null, new BufferedInputStream(uploadFile.getInputStream()), service);
	}
	
	public boolean uploadZipFile(boolean uploadingTheme, String fileName, InputStream stream, IWSlideService slide) throws IOException {
		String resultInfo = null;
		boolean result = false;
		
		if (uploadingTheme) {
			String uploadFileName = fileName;
			if (uploadFileName == null) {
				uploadFileName = getUploadFileName();
			}
			if (!uploadFileName.equals(CoreConstants.EMPTY)) {
				uploadFilePath = getThemesHelper().changeFileUploadPath(getUploadFilePath() + uploadFileName);
			}
		}
		String path = getUploadFilePath();
		
		ZipInputStream zipStream = null;
		try {
			zipStream = new ZipInputStream(stream);
			if (slide.uploadZipFileContents(zipStream, path)) {
				resultInfo = "Success uploading contents of file: " + path;
				result = true;
			}
			else {
				resultInfo = "Unable to upload contents of file: " + path;
				result = false;
			}
		} catch(Exception e) {
			e.printStackTrace();
		} finally {
			IOUtil.close(stream);
			IOUtil.close(zipStream);
		}
		
		if (uploadingTheme) {
			getThemesHelper().removeThemeFromQueue(path);
		}
		
		LOGGER.info(resultInfo);
		return result;
	}
	
	private String getUploadFileName() {
		if (uploadFile == null) {
			return CoreConstants.EMPTY;
		}
		
		String name = uploadFile.getName();
		if (name == null) {
			return CoreConstants.EMPTY;
		}
		
		if (name.indexOf(CoreConstants.BACK_SLASH) != -1) {
			int nameLength = name.length();
			int lastIndexOfBackSlash = name.lastIndexOf(CoreConstants.BACK_SLASH);
			if (nameLength > lastIndexOfBackSlash) {
				name = name.substring(lastIndexOfBackSlash + 1);
			}
		}
		
		if (name.indexOf(ContentConstants.DOT) != -1) {
			name = name.substring(0, name.lastIndexOf(ContentConstants.DOT));
		}
		
		return name;
	}

	public ThemesHelper getThemesHelper() {
		return ELUtil.getInstance().getBean(ThemesHelper.class);
	}

}