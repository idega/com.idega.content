/*
 * $Id: WebDAVFilePermissionResourceHome.java,v 1.1 2005/01/07 19:46:49 gummi Exp $
 * Created on 7.1.2005
 *
 * Copyright (C) 2005 Idega Software hf. All Rights Reserved.
 *
 * This software is the proprietary information of Idega hf.
 * Use is subject to license terms.
 */
package com.idega.content.business;

import com.idega.business.IBOHome;


/**
 * 
 *  Last modified: $Date: 2005/01/07 19:46:49 $ by $Author: gummi $
 * 
 * @author <a href="mailto:gummi@idega.com">Gudmundur Agust Saemundsson</a>
 * @version $Revision: 1.1 $
 */
public interface WebDAVFilePermissionResourceHome extends IBOHome {

	public WebDAVFilePermissionResource create() throws javax.ejb.CreateException, java.rmi.RemoteException;
}
