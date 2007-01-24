/*
 * $Id: WebDAVMetadataResource.java,v 1.2.2.1 2007/01/24 13:35:58 gediminas Exp $
 *
 * Copyright (C) 2004 Idega. All Rights Reserved.
 *
 * This software is the proprietary information of Idega.
 * Use is subject to license terms.
 *
 */
package com.idega.content.business;

import java.io.IOException;
import java.rmi.RemoteException;
import java.util.Collection;
import com.idega.business.IBOSession;
import com.idega.content.data.MetadataValueBean;

/**
 * 
 * Last modified: $Date: 2007/01/24 13:35:58 $ by $Author: gediminas $
 *
 * @author Joakim Johnson
 * @version $Revision: 1.2.2.1 $
 */
public interface WebDAVMetadataResource extends IBOSession {
	/**
	 * @see com.idega.content.business.WebDAVMetadataResourceBean#clear
	 */
	public void clear() throws RemoteException;

	/**
	 * @see com.idega.content.business.WebDAVMetadataResourceBean#getMetadataBeans
	 */
	public Collection getMetadataBeans(String resourcePath) throws RemoteException, IOException, RemoteException;

	/**
	 * @see com.idega.content.business.WebDAVMetadataResourceBean#getCategories
	 */
	public Collection getCategories(String resourcePath) throws RemoteException, IOException, RemoteException;

	/**
	 * @see com.idega.content.business.WebDAVMetadataResourceBean#getMetadata
	 */
	public MetadataValueBean[] getMetadata(String resourcePath) throws RemoteException, IOException, RemoteException;

	/**
	 * @see com.idega.content.business.WebDAVMetadataResourceBean#storeCategories
	 */
	public void setCategories(String resourcePath, String categories, boolean setOnParent) throws RemoteException;

	/**
	 * @see com.idega.content.business.WebDAVMetadataResourceBean#storeMetadata
	 */
	public void setMetadata(String resourcePath, String type, String val) throws RemoteException;
}