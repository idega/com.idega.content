package com.idega.content.upload.business;


import java.util.List;

import org.jdom2.Document;

import com.idega.builder.bean.AdvancedProperty;
import com.idega.business.SpringBeanName;
import com.idega.content.upload.bean.UploadFile;
import com.idega.idegaweb.IWResourceBundle;
import com.idega.presentation.IWContext;
import com.idega.presentation.Layer;

@SpringBeanName(FileUploader.SPRING_BEAN_IDENTIFIER)
public interface FileUploader {

	public static final String SPRING_BEAN_IDENTIFIER = "fileUploader";

	/**
	 * @see com.idega.content.upload.business.FileUploaderBean#uploadFile
	 */
	public boolean uploadFile(List<UploadFile> files, String uploadPath, boolean isIE);

	/**
	 * @see com.idega.content.upload.business.FileUploaderBean#uploadThemePack
	 */
	public boolean uploadThemePack(List<UploadFile> files, String uploadPath, boolean isIE);

	/**
	 * @see com.idega.content.upload.business.FileUploaderBean#uploadZipFile
	 */
	public boolean uploadZipFile(List<UploadFile> files, String uploadPath, boolean extractContent, boolean isIE);

	/**
	 * @see com.idega.content.upload.business.FileUploaderBean#getFileInput
	 */
	public Layer getFileInput(IWContext iwc, String id, boolean addRemoveImage, boolean showProgressBar, boolean addjQuery, boolean autoAddFileInput,
			boolean allowMultipleFiles, boolean autoUpload, String style);

	/**
	 * @see com.idega.content.upload.business.FileUploaderBean#getRenderedFileInput
	 */
	public Document getRenderedFileInput(String id, Boolean showProgressBar,
			Boolean addjQuery, Boolean autoAddFileInput,
			Boolean allowMultipleFiles, Boolean autoUpload, String style);

	public void initializeUploader(IWContext iwc);

	public String getAddFileInputJavaScriptAction(String containerId, IWResourceBundle iwrb, boolean showProgressBar, boolean addjQuery, boolean autoAddFileInput,
			boolean autoUpload);

	public String getRenderedComponent(String id);

	public String getActionToLoadFilesAndExecuteCustomAction(String customAction, boolean showProgressBar, boolean addjQuery);

	public String getUploadAction(IWContext iwc, String id, String progressBarId, String uploadId, boolean showProgressBar, boolean showLoadingMessage,
			boolean zipFile, String formId, String actionAfterUpload, String actionAfterCounterReset, boolean autoUpload, boolean showUploadedFiles,
			String componentToRerenderId, boolean fakeFileDeletion, String actionAfterUploadedToRepository, boolean stripNonRomanLetters, String maxUploadSize,
			String onFail);

	public String getPropertiesAction(IWContext iwc, String id, String progressBarId, String uploadId, boolean showProgressBar, boolean showLoadingMessage,
			boolean zipFile, String formId, String actionAfterUpload, String actionAfterCounterReset, boolean autoUpload, boolean showUploadedFiles,
			String componentToRerenderId, boolean fakeFileDeletion, String actionAfterUploadedToRepository, boolean stripNonRomanLetters, String maxUploadSize,
			String onFail
	);

	public List<String> getUploadedFilesList(List<String> files, String uploadPath, Boolean fakeFileDeletion, Boolean stripNonRomanLetters, String uploadId);

	public AdvancedProperty deleteFile(String fileInRepository, String uploadId, Boolean fakeFileDeletion);

	public AdvancedProperty deleteFiles(List<String> filesInRepository, String uploadId, Boolean fakeFileDeletion);
	
	public Boolean addPreviouslyUploadedFiles(String uploadId, String oldUploadId);

	public List<String> getUploadedFilesListById(String uploadId, String uploadPath, Boolean fakeFileDeletion, Boolean stripNonRomanLetters);

}