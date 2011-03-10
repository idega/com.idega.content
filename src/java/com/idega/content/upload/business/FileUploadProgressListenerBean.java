package com.idega.content.upload.business;

import java.text.NumberFormat;
import java.util.HashMap;
import java.util.Locale;
import java.util.Map;

import com.idega.idegaweb.IWApplicationContext;
import com.idega.idegaweb.IWMainApplication;
import com.idega.util.StringUtil;

public class FileUploadProgressListenerBean implements FileUploadProgressListener {
	
	private long bytesTransferred, fileSize = 0;

	private Map<String, Boolean> uploadInfo = new HashMap<String, Boolean>();
	
	public void update(long bytesRead, long contentLength, int items) {
		bytesTransferred = bytesRead;
		fileSize = contentLength;
	}
	
	public String getFileUploadStatus() {
		if (fileSize == 0) {
			return null;
		}
		
		String status = NumberFormat.getPercentInstance(Locale.ENGLISH).format((double) bytesTransferred / (double) fileSize);
		return status.substring(0, status.length() - 1);
	}
	
	public boolean resetFileUploaderCounters() {
		bytesTransferred = 0;
		fileSize = 0;
		
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
	
}