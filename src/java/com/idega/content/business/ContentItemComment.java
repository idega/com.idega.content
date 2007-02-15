package com.idega.content.business;

public class ContentItemComment {
	
	private String user = null;
	private String subject = null;
	private String comment = null;
	private String posted = null;
	
	public String getComment() {
		return comment;
	}
	public void setComment(String comment) {
		this.comment = comment;
	}
	public String getPosted() {
		return posted;
	}
	public void setPosted(String posted) {
		this.posted = posted;
	}
	public String getSubject() {
		return subject;
	}
	public void setSubject(String subject) {
		this.subject = subject;
	}
	public String getUser() {
		return user;
	}
	public void setUser(String user) {
		this.user = user;
	}

}
