/*
 * $Id: WebDAVMetadataResourceBean.java,v 1.2 2005/02/06 16:25:47 laddi Exp $
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
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import org.apache.webdav.lib.PropertyName;
import com.idega.business.IBOLookup;
import com.idega.business.IBOSessionBean;
import com.idega.content.data.MetadataValueBean;
import com.idega.presentation.IWContext;
import com.idega.slide.business.IWSlideService;
import com.idega.slide.business.IWSlideSession;
import com.idega.slide.util.WebdavRootResource;


/**
 * 
 * Last modified: $Date: 2005/02/06 16:25:47 $ by $Author: laddi $
 *
 * @author Joakim Johnson
 * @version $Revision: 1.2 $
 */
public class WebDAVMetadataResourceBean extends IBOSessionBean 
implements WebDAVMetadataResource
{
	//TODO change to use a map so that many metadata blocks can be displayed on one page.
	private Map map = new HashMap();
	private Collection metadataBeans = null;

	private MetadataValueBean[] data;
	private String currentPath = null;
	
	public WebDAVMetadataResourceBean() {
		super();
	}
	
	public void clear() {
		metadataBeans = null;
	}
	
	private void setMetadataBeans(String resourcePath, Collection c) {
		metadataBeans = c;
		currentPath = resourcePath;
	}
	
	public Collection getMetadataBeans(String resourcePath) throws RemoteException, IOException {
		if(metadataBeans == null || !checkPath(resourcePath)) {
			setMetadataBeans(resourcePath,getMetadataFromRepository(resourcePath));
		}
		return metadataBeans;
	}
	
	protected boolean checkPath(String path){
		//PATCH-HACK
		if(currentPath.startsWith(path)) {
			return true;
		}
		if(!path.equalsIgnoreCase(currentPath)) {
			currentPath=path;
			return false;
		}
//		if(currentPath!=null){ 
//			if(!currentPath.equalsIgnoreCase(path)){
//				System.out.println("path "+path+" is different from "+currentPath);
//				return false;
////				throw new ConcurrentModificationException("Asking for ACL for path '"+path+"' while current path is '"+currentPath+"'. The #clear() method needs to be invoked first.");
//			}
//		}
		return true;
	}
	
	public MetadataValueBean[] getMetadata(String resourcePath) throws RemoteException, IOException {
		return (MetadataValueBean[])getMetadataBeans(resourcePath).toArray(new MetadataValueBean[metadataBeans.size()]);
	}
	
	protected Collection getMetadataFromRepository(String resourcePath) throws RemoteException, IOException {
		ArrayList metadataBeans = new ArrayList();
	
		IWContext iwc = IWContext.getInstance();
		IWSlideSession session = (IWSlideSession)IBOLookup.getSessionInstance(iwc,IWSlideSession.class);
		IWSlideService service = (IWSlideService)IBOLookup.getServiceInstance(iwc,IWSlideService.class);

		WebdavRootResource rootResource = session.getWebdavRootResource();

		String filePath = service.getURI(resourcePath);
		
		Iterator iter = MetadataUtil.getMetadataTypes().iterator();
		while(iter.hasNext()) {
			String type = (String)iter.next();

			Enumeration enumerator = rootResource.propfindMethod(filePath,new PropertyName("DAV",type).toString());

			StringBuffer value = new StringBuffer();
			while(enumerator.hasMoreElements()) {
				value.append(enumerator.nextElement());
			}
//				System.out.println("Value is "+value);
			if(value.length()>0) {
				MetadataValueBean mvb = new MetadataValueBean(type, value.toString());
				metadataBeans.add(mvb);
			}
		}
			
		return metadataBeans;
	}

}
