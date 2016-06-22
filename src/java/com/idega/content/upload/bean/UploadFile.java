package com.idega.content.upload.bean;

import java.io.Serializable;
import java.util.Arrays;
import java.util.Random;

public class UploadFile implements Serializable {

	private static final long serialVersionUID = 237119286592662435L;

	private String name, type, path;
	private long size = 0;
	private byte[] bytes = null;
	private int hash;

	public UploadFile(String name, String type, long size, byte[] bytes) {
		this.name = name;
		this.type = type;
		this.size = size;
		this.bytes = bytes;

		hash = Arrays.hashCode(bytes);
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

	@Override
	public boolean equals(Object obj) {
		if (!(obj instanceof UploadFile))
			return false;

		UploadFile file = (UploadFile) obj;
		return getName().equals(file.getName()) && getPath().equals(file.getPath()) && getSize() == file.getSize() && hashCode() == file.hashCode();
	}

	public String getPath() {
		return path;
	}

	public void setPath(String path) {
		this.path = path;
	}

	@Override
	public int hashCode() {
		return hash;
	}

	@Override
	public String toString() {
		return "Name: " + getName() + ", path: " + getPath() + ", type: " + getType() + ", size: " + getSize() + ", bytes: " + getBytes();
	}
}