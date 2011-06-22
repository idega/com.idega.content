package com.idega.content.upload.business;

import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import com.idega.builder.bean.AdvancedProperty;
import com.idega.content.upload.bean.UploadFile;
import com.idega.idegaweb.IWApplicationContext;
import com.idega.idegaweb.IWMainApplication;
import com.idega.util.ListUtil;
import com.idega.util.StringUtil;

public class FileUploadProgressListenerBean implements FileUploadProgressListener {
	
	private long bytesTransferred, fileSize = 0,
				maxSize = -1;
	
	private String uploadId;

	private Map<String, Boolean> uploadInfo = new HashMap<String, Boolean>();
	private Map<String, List<UploadFile>> uploadedFiles = new HashMap<String, List<UploadFile>>();
	
	private List<String> failedUploads = new ArrayList<String>();
	
	public void update(long bytesRead, long contentLength, int items) {
		bytesTransferred = bytesRead;
		fileSize = contentLength;
	}
	
	public void markFailedUpload(String id) {
		if (!StringUtil.isEmpty(id))
			failedUploads.add(id);
	}
	
	public String getFileUploadStatus(String id) {
		if (!StringUtil.isEmpty(id) && failedUploads.contains(id)) {
			failedUploads.remove(id);
			return String.valueOf(-1);
		}
		
		if (fileSize == 0) {
			return null;
		}
		
		String status = NumberFormat.getPercentInstance(Locale.ENGLISH).format((double) bytesTransferred / (double) fileSize);
		return status.substring(0, status.length() - 1);
	}
	
	public boolean resetFileUploaderCounters(String id, long maxSize) {
		bytesTransferred = 0;
		fileSize = 0;
		
		this.uploadId = id;
		this.maxSize = maxSize;
		
		return true;
	}

	public boolean isUploadInProgress(String id) {
		if (StringUtil.isEmpty(id)) {
			return false;
		}
		
		IWApplicationContext iwac = IWMainApplication.getDefaultIWApplicationContext();
		if (iwac == null) {
			return false;
		}
		
		Object o = iwac.getApplicationAttribute(id);
		if (o instanceof Boolean) {
			return (Boolean) o;
		}
		
		return false;
	}

	public Boolean isUploadSuccessful(String id) {
		if (StringUtil.isEmpty(id)) {
			return Boolean.FALSE;
		}
		
		return uploadInfo.remove(id);
	}

	public void setUploadSuccessful(String id, boolean success) {
		if (StringUtil.isEmpty(id)) {
			return;
		}
		
		uploadInfo.put(id, success);
	}

	public long getMaxSize() {
		return maxSize;
	}

	public String getUploadId() {
		return uploadId;
	}

	@Override
	public void addUploadedFiles(String uploadId, Collection<UploadFile> files) {
		if (StringUtil.isEmpty(uploadId) || ListUtil.isEmpty(files))
			return;
		
		List<UploadFile> uploadFiles = uploadedFiles.get(uploadId);
		if (uploadFiles == null) {
			uploadFiles = new ArrayList<UploadFile>();
			uploadedFiles.put(uploadId, uploadFiles);
		}
		for (UploadFile file: files) {
			if (!uploadFiles.contains(file))
				uploadFiles.add(file);
		}
	}

	@Override
	public Collection<AdvancedProperty> getUploadedFiles(String uploadId) {
		if (StringUtil.isEmpty(uploadId))
			return null;
		
		List<UploadFile> files = uploadedFiles.remove(uploadId);
		if (ListUtil.isEmpty(files))
			return null;
		
		Collection<AdvancedProperty> uploadedFiles = new ArrayList<AdvancedProperty>(files.size());
		for (UploadFile file: files) {
			uploadedFiles.add(new AdvancedProperty(file.getPath(), file.getName()));
		}
		return uploadedFiles;
	}
	
}