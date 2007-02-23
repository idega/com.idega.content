package com.idega.content.business;

import java.util.List;

import javax.mail.MessagingException;

import com.idega.util.SendMail;

public class CommentsNotificationSender implements Runnable {
	
	private List<String> emails = null;
	private String emailFieldFrom = null;
	private String emailFieldSubject = null;
	private String emailFieldBody = null;
	private String mailServerHost = null;
	
	public CommentsNotificationSender(List<String> emails, String emailFieldFrom, String emailFieldSubject, String emailFieldBody,
			String mailServerHost) {
		this.emails = emails;
		this.emailFieldFrom = emailFieldFrom;
		this.emailFieldSubject = emailFieldSubject;
		this.emailFieldBody = emailFieldBody;
		this.mailServerHost = mailServerHost;
	}

	public void run() {
		if (emails == null) {
			return;
		}
		for (int i = 0; i < emails.size(); i++) {
			try {
				SendMail.send(emailFieldFrom, emails.get(i),  null, null, mailServerHost, emailFieldSubject, emailFieldBody);
			} catch (MessagingException e) {
				e.printStackTrace();
			}
		}
	}

}