package com.idega.content.upload.business;

import java.text.NumberFormat;


public class FileUploadProgressListenerBean implements FileUploadProgressListener {
	
	private long bytesTransferred = 0;
	private long fileSize = 0;

	public void update(long bytesRead, long contentLength, int items) {
		bytesTransferred = bytesRead;
		fileSize = contentLength;
	}
	
	public String getFileUploadStatus() {
		if (bytesTransferred == 0 || fileSize == 0) {
			return null;
		}
		
		String status = NumberFormat.getPercentInstance().format((double) bytesTransferred / (double) fileSize);
		System.out.println("Upload status: " + status);
		return status.substring(0, status.length() - 1);
	}
	
	public boolean resetFileUploaderCounters() {
		bytesTransferred = 0;
		fileSize = 0;
		
		return true;
	}
	
}
