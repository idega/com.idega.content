/*
 * $Id: CategoryUtil.java,v 1.1 2005/03/17 17:27:00 joakim Exp $
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
import java.util.ArrayList;
import java.util.Collection;
import java.util.StringTokenizer;
import org.apache.commons.httpclient.HttpException;
import org.apache.webdav.lib.WebdavResource;
import com.idega.business.IBOLookup;
import com.idega.business.IBOLookupException;
import com.idega.presentation.IWContext;
import com.idega.slide.business.IWSlideService;


/**
 * <p>Utility functions for category. 
 * You can get a collection of all the available categories</p>
 *  Last modified: $Date: 2005/03/17 17:27:00 $ by $Author: joakim $
 * 
 * @author <a href="mailto:Joakim@idega.com">Joakim</a>
 * @version $Revision: 1.1 $
 */
public class CategoryUtil {
	private static final String CATEORY_FILE_PATH = ".properties";
	
	/**
	 * <p> Get a collection of categories </p>
	 * @return collection of strings
	 */
	public Collection getCategories() {
		Collection ret = new ArrayList();
		
		IWContext iwc = IWContext.getInstance();
		
		try {
//			IWSlideSession session = (IWSlideSession)IBOLookup.getSessionInstance(iwc,IWSlideSession.class);
			IWSlideService service = (IWSlideService)IBOLookup.getServiceInstance(iwc,IWSlideService.class);

//			WebdavRootResource rootResource = session.getWebdavRootResource();

			String filePath = service.getURI(CATEORY_FILE_PATH);
			WebdavResource webdavResource = new WebdavResource(filePath);
			String categories = webdavResource.getMethodDataAsString();
			StringTokenizer st = new StringTokenizer(categories,",");
			while(st.hasMoreTokens()) {
				ret.add(st.nextToken().trim());
			}
		}
		catch (IBOLookupException e) {
			e.printStackTrace();
		}
		catch (HttpException e) {
			e.printStackTrace();
		}
		catch (RemoteException e) {
			e.printStackTrace();
		}
		catch (IOException e) {
			e.printStackTrace();
		}
		
		return ret;
	}
}
