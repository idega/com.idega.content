package com.idega.content.repository.stream.bean;

import java.io.Serializable;

public class StreamResult implements Serializable {

	private static final long serialVersionUID = 2752140310441067450L;

	private String	name,
					destinationDirectory,
					uuid;

	private long size, time;

	private boolean success;

	public StreamResult() {
		super();
	}

	public StreamResult(String name, String	destinationDirectory, String uuid, boolean success) {
		this.name = name;
		this.destinationDirectory = destinationDirectory;
		this.uuid = uuid;

		this.success = success;
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

	public boolean isSuccess() {
		return success;
	}

	public void setSuccess(boolean success) {
		this.success = success;
	}

	public String getUuid() {
		return uuid;
	}

	public void setUuid(String uuid) {
		this.uuid = uuid;
	}

	public long getSize() {
		return size;
	}

	public void setSize(long size) {
		this.size = size;
	}

	public long getTime() {
		return time;
	}

	public void setTime(long time) {
		this.time = time;
	}

	@Override
	public String toString() {
		return getDestinationDirectory() + getName() + ", uuid: " + getUuid() + ", streamed: " + isSuccess();
	}
}