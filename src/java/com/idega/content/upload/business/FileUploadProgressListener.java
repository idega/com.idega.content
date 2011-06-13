package com.idega.content.upload.business;

import org.apache.commons.fileupload.ProgressListener;

import com.idega.business.SpringBeanName;

@SpringBeanName("fileUploadProgressListener")
public interface FileUploadProgressListener extends ProgressListener {
	
	public String getFileUploadStatus(String id);
	
	public boolean resetFileUploaderCounters(String id, long maxSize);

	public boolean isUploadInProgress(String id);
	
	public Boolean isUploadSuccessful(String id);
	public void setUploadSuccessful(String id, boolean success);
	
	public void markFailedUpload(String id);
	
	public long getMaxSize();
	public String getUploadId();
}