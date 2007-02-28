package com.idega.content.business;


import com.idega.business.IBOService;
import java.util.List;
import java.rmi.RemoteException;

public interface CommentsEngine extends IBOService {
	/**
	 * @see com.idega.content.business.CommentsEngineBean#addComment
	 */
	public boolean addComment(String cacheKey, String user, String subject, String email, String body, String uri, boolean notify) throws RemoteException;

	/**
	 * @see com.idega.content.business.CommentsEngineBean#getCommentsForAllPages
	 */
	public boolean getCommentsForAllPages(String uri) throws RemoteException;

	/**
	 * @see com.idega.content.business.CommentsEngineBean#getComments
	 */
	public List<ContentItemComment> getComments(String uri) throws RemoteException;

	/**
	 * @see com.idega.content.business.CommentsEngineBean#getCommentsCount
	 */
	public int getCommentsCount(String uri) throws RemoteException;
	
	/**
	 * @see com.idega.content.business.CommentsEngineBean#setModuleProperty
	 */
	public boolean setModuleProperty(String pageKey, String moduleId, String propName, String propValue, String cacheKey) throws RemoteException;
}