package com.idega.content.upload.business;

import org.apache.commons.fileupload.ProgressListener;

import com.idega.business.SpringBeanName;

@SpringBeanName("fileUploadProgressListener")
public interface FileUploadProgressListener extends ProgressListener {
	
	public String getFileUploadStatus();
	
	public boolean resetFileUploaderCounters();

	public boolean isUploadInProgress(String id);
	
	public Boolean isUploadSuccessful(String id);
	public void setUploadSuccessful(String id, boolean success);
}
