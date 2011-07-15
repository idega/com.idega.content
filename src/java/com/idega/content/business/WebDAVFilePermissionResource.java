/*
 * $Id: WebDAVFilePermissionResource.java,v 1.1 2005/01/07 19:46:49 gummi Exp $
 * Created on 7.1.2005
 *
 * Copyright (C) 2005 Idega Software hf. All Rights Reserved.
 *
 * This software is the proprietary information of Idega hf.
 * Use is subject to license terms.
 */
package com.idega.content.business;

import java.util.Collection;

import javax.jcr.RepositoryException;

import com.idega.business.IBOSession;
import com.idega.content.data.ACEBean;


/**
 *
 *  Last modified: $Date: 2005/01/07 19:46:49 $ by $Author: gummi $
 *
 * @author <a href="mailto:gummi@idega.com">Gudmundur Agust Saemundsson</a>
 * @version $Revision: 1.1 $
 */
public interface WebDAVFilePermissionResource extends IBOSession {

	/**
	 * @see com.idega.content.business.WebDAVFilePermissionResourceBean#clear
	 */
	public void clear() throws RepositoryException;

	/**
	 * @see com.idega.content.business.WebDAVFilePermissionResourceBean#getAllAces
	 */
	public Collection<ACEBean> getAllAces(String path) throws RepositoryException;

	/**
	 * @see com.idega.content.business.WebDAVFilePermissionResourceBean#getRoleAces
	 */
	public Collection<ACEBean> getRoleAces(String path) throws RepositoryException;

	/**
	 * @see com.idega.content.business.WebDAVFilePermissionResourceBean#getStandardAces
	 */
	public Collection<ACEBean> getStandardAces(String path) throws RepositoryException;

	/**
	 * @see com.idega.content.business.WebDAVFilePermissionResourceBean#getGroupAces
	 */
	public Collection<ACEBean> getGroupAces(String path) throws RepositoryException;

	/**
	 * @see com.idega.content.business.WebDAVFilePermissionResourceBean#getUserAces
	 */
	public Collection<ACEBean> getUserAces(String path) throws RepositoryException;

	/**
	 * @see com.idega.content.business.WebDAVFilePermissionResourceBean#store
	 */
	public void store(String path) throws RepositoryException;
}