package com.idega.content.upload.business;


import java.util.List;

import org.jdom.Document;

import com.idega.business.SpringBeanName;
import com.idega.content.upload.bean.UploadFile;
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
	public Layer getFileInput(IWContext iwc, boolean addRemoveImage);
	
	/**
	 * @see com.idega.content.upload.business.FileUploaderBean#getRenderedFileInput
	 */
	public Document getRenderedFileInput();
	
	public void initializeUploader(IWContext iwc);
}