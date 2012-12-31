package com.idega.content.repository.stream.bean;

import java.io.InputStream;
import java.io.Serializable;

import com.idega.util.FileUtil;
import com.idega.util.IOUtil;

public class StreamData implements Serializable {

	private static final long serialVersionUID = 6336792151836691809L;

	private String	name,
					destinationDirectory,
					uuid;

	private byte[] bytes;

	public StreamData() {
		super();
	}

	public StreamData(String name, String destinationDirectory, String uuid) {
		this();

		this.name = name;
		this.destinationDirectory = destinationDirectory;
		this.uuid = uuid;
	}

	public StreamData(String name, String destinationDirectory, String uuid, byte[] bytes) {
		this(name, destinationDirectory, uuid);

		this.bytes = bytes;
	}

	public StreamData(String name, String destinationDirectory, String uuid, InputStream stream) {
		this(name, destinationDirectory, uuid, IOUtil.getBytesFromInputStream(stream));
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getDestinationDirectory() {
		return destinationDirectory;
	}

	public void setDestinationDirectory(String destinationDirectory) {
		this.destinationDirectory = destinationDirectory;
	}

	public byte[] getBytes() {
		return bytes;
	}

	public void setBytes(byte[] bytes) {
		this.bytes = bytes;
	}

	public String getUuid() {
		return uuid;
	}

	public void setUuid(String uuid) {
		this.uuid = uuid;
	}

	@Override
	public String toString() {
		return getDestinationDirectory() + getName() + ", uuid: " + getUuid() + " size: " + (getBytes() == null ? "unknown" : FileUtil.getHumanReadableSize(getBytes().length));
	}

}