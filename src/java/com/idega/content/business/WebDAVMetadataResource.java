/*
 * $Id: WebDAVMetadataResource.java,v 1.1 2005/01/28 13:52:21 joakim Exp $
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
import com.idega.content.data.MetadataValueBean;

/**
 * 
 * Last modified: $Date: 2005/01/28 13:52:21 $ by $Author: joakim $
 *
 * @author Joakim Johnson
 * @version $Revision: 1.1 $
 */
public interface WebDAVMetadataResource {

	public abstract void clear();

	public abstract Collection getMetadataBeans(String resourcePath) throws RemoteException, IOException;

	public abstract MetadataValueBean[] getMetadata(String resourcePath) throws RemoteException, IOException;
}