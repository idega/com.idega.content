package com.idega.content.upload.bean;


public class UploadFile {
	
	private String name = null;
	private String type = null;
	private long size = 0;
	private byte[] bytes = null;
	
	public UploadFile(String name, String type, long size, byte[] bytes) {
		this.name = name;
		this.type = type;
		this.size = size;
		this.bytes = bytes;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getType() {
		return type;
	}

	public void setType(String type) {
		this.type = type;
	}

	public long getSize() {
		return size;
	}

	public void setSize(long size) {
		this.size = size;
	}

	public byte[] getBytes() {
		return bytes;
	}

	public void setBytes(byte[] bytes) {
		this.bytes = bytes;
	}
	
}
