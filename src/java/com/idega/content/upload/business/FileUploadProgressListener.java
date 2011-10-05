package com.idega.content.upload.business;

import java.util.Collection;

import org.apache.commons.fileupload.ProgressListener;

import com.idega.builder.bean.AdvancedProperty;
import com.idega.business.SpringBeanName;
import com.idega.content.upload.bean.UploadFile;

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
	
	public void addUploadedFiles(String uploadId, Collection<UploadFile> files);
	public Collection<AdvancedProperty> getUploadedFiles(String uploadId);
}