package com.idega.content.upload.business;

import java.util.Collection;
import java.util.List;

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

	public void removeUploadedFiles(String uploadId, List<String> filesInRepo);
	
	public Collection<AdvancedProperty> getUploadedFiles(String uploadId);

	public void setFileNumberInUploadSequence(int number);

	public void appendFiles(String uploadId, String oldUploadId);
}