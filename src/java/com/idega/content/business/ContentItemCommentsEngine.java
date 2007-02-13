package com.idega.content.business;


import com.idega.business.IBOService;
import java.rmi.RemoteException;

public interface ContentItemCommentsEngine extends IBOService {
	/**
	 * @see com.idega.content.business.ContentItemCommentsEngineBean#addComment
	 */
	public String addComment(String user, String subject, String body, String uri, boolean existsCommentsFile) throws RemoteException;
}