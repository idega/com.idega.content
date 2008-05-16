/*
 * $Id: WebDAVMetadataResourceBean.java,v 1.15 2008/05/16 14:58:43 valdas Exp $
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
import java.util.Iterator;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.apache.commons.httpclient.HttpException;
import org.apache.webdav.lib.PropertyName;

import com.idega.business.IBOLookup;
import com.idega.business.IBOSessionBean;
import com.idega.content.business.categories.CategoryBean;
import com.idega.content.data.MetadataValueBean;
import com.idega.presentation.IWContext;
import com.idega.slide.business.IWSlideService;
import com.idega.slide.business.IWSlideSession;
import com.idega.slide.util.WebdavRootResource;
import com.idega.util.CoreUtil;


/**
 * A resource bean that holds metadata info for the selected resouce
 * 
 * Last modified: $Date: 2008/05/16 14:58:43 $ by $Author: valdas $
 *
 * @author Joakim Johnson
 * @version $Revision: 1.15 $
 */
public class WebDAVMetadataResourceBean extends IBOSessionBean implements WebDAVMetadataResource {

	private static final long serialVersionUID = -4731482043715283036L;
	
	//TODO change to use a map so that many metadata blocks can be displayed on one page.
//	private Map map = new HashMap();
	private Collection metadataBeans = null;	//Holding MetadataValueBean
	private Collection<String> selectedCategories = null;

//	private MetadataValueBean[] data;
	private String currentPath = null;
	
	public WebDAVMetadataResourceBean() {
		super();
	}
	
	/**
	 * Clears the metadata beans (cashe) 
	 */
	public void clear() {
		this.metadataBeans = null;
		this.selectedCategories=null;
		this.currentPath=null;
	}
	
	private void setMetadataBeans(String resourcePath, Collection meta) {
		this.metadataBeans = meta;
		this.currentPath = resourcePath;
	}
	
	private void setSelectedCategories(String resourcePath, Collection<String> categories) {
		this.selectedCategories = categories;
		this.currentPath = resourcePath;
	}
	
	/**
	 * returns metadata key - value pairs for the article specified by the given resourcePath
	 * @return a collection of MetadataValueBeans
	 */
	public Collection getMetadataBeans(String resourcePath) throws RemoteException, IOException {
		if(this.metadataBeans == null || !checkPath(resourcePath)) {
			setMetadataBeans(resourcePath,getMetadataFromRepository(resourcePath));
		}
		return this.metadataBeans;
	}
	

	/**
	 * returns categories selected for the article specified by the given resourcePath
	 * @return a collection of Strings
	 */
	public Collection<String> getCategories(String resourcePath) throws RemoteException, IOException {
		if(selectedCategories == null || !checkPath(resourcePath)) {
			setSelectedCategories(resourcePath, getCategoriesFromRepository(resourcePath));
		}
		return this.selectedCategories;
	}
	
	public MetadataValueBean[] getMetadata(String resourcePath) throws RemoteException, IOException {
		return (MetadataValueBean[])getMetadataBeans(resourcePath).toArray(new MetadataValueBean[this.metadataBeans.size()]);
	}
	
	/**
	 * <p> returns a collection of metadata for the given resource</p>
	 * @param resourcePath
	 * @return collection of MetadataValueBean
	 * @throws RemoteException
	 * @throws IOException
	 */
	protected Collection getMetadataFromRepository(String resourcePath) throws RemoteException, IOException {
		this.metadataBeans = new ArrayList();
	
		IWContext iwc = IWContext.getInstance();
		IWSlideSession session = (IWSlideSession)IBOLookup.getSessionInstance(iwc,IWSlideSession.class);
		IWSlideService service = (IWSlideService)IBOLookup.getServiceInstance(iwc,IWSlideService.class);

		WebdavRootResource rootResource = session.getWebdavRootResource();

		String filePath = resourcePath;
		String serverURI = service.getWebdavServerURI();
		if(!resourcePath.startsWith(serverURI)) {
			filePath = service.getURI(resourcePath);
		}
		
		Iterator iter = MetadataUtil.getMetadataTypes().iterator();
		while(iter.hasNext()) {
			String type = (String)iter.next();

//			System.out.println("Getting metadata '"+type+"' for "+filePath);
			
			try {
				Enumeration enumerator = rootResource.propfindMethod(filePath,new PropertyName("DAV",type).toString());

				StringBuffer value = new StringBuffer();
				while(enumerator.hasMoreElements()) {
					value.append(enumerator.nextElement());
				}
//				System.out.println("Value is "+value);
				if(value.length()>0) {
					MetadataValueBean mvb = new MetadataValueBean(type, value.toString());
					this.metadataBeans.add(mvb);
				}
			}catch (HttpException e) {
				System.out.println("Warning could not load metadata '"+type+"' for "+filePath);
			}

		}
			
		return this.metadataBeans;
	}

	/**
	 * <p> Get all the selected categories for the selected resource. 
	 * This functionallity might be moved to a sepparate class</p>
	 * @param resourcePath
	 * @return Collections of strings
	 * @throws RemoteException
	 * @throws IOException
	 */
	protected Collection<String> getCategoriesFromRepository(String resourcePath) throws RemoteException, IOException {
		IWContext iwc = CoreUtil.getIWContext();
		if (iwc == null) {
			return null;
		}
		
		String filePath = resourcePath;
		try {
			IWSlideSession session = (IWSlideSession)IBOLookup.getSessionInstance(iwc,IWSlideSession.class);
			IWSlideService service = (IWSlideService)IBOLookup.getServiceInstance(iwc,IWSlideService.class);
	
			WebdavRootResource rootResource = session.getWebdavRootResource();
	
			String serverURI = service.getWebdavServerURI();
			if(!resourcePath.startsWith(serverURI)) {
				filePath = service.getURI(resourcePath);
			}
		
			Enumeration enumerator = rootResource.propfindMethod(filePath, new PropertyName("DAV","categories").toString());
	
			StringBuffer value = new StringBuffer();
			while(enumerator.hasMoreElements()) {
				value.append(enumerator.nextElement());
			}
			
			this.selectedCategories=CategoryBean.getCategoriesFromString(value.toString());
			
		} catch (Exception e) {
			e.printStackTrace();
			Logger.getLogger(WebDAVMetadataResourceBean.class.getName()).log(Level.SEVERE, "Warning could not load categories for "+ filePath);
		}
		
		return this.selectedCategories;
	}

	protected boolean checkPath(String path){
		//PATCH-HACK
		if(null==this.currentPath) {
			this.currentPath=path;
			return false;
		}
		if(this.currentPath.startsWith(path)) {
			return true;
		}
		if(!path.equalsIgnoreCase(this.currentPath)) {
			this.currentPath=path;
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

	public void setCategories(String resourcePath, String categories, boolean setOnParent) throws IOException {
		IWContext iwc = IWContext.getInstance();
		IWSlideSession session = (IWSlideSession)IBOLookup.getSessionInstance(iwc,IWSlideSession.class);
		IWSlideService service = (IWSlideService)IBOLookup.getServiceInstance(iwc,IWSlideService.class);

		String filePath = resourcePath;
		String serverURI = service.getWebdavServerURI();
		if(!resourcePath.startsWith(serverURI)) {
			filePath = service.getURI(resourcePath);
		}
		WebdavRootResource rootResource = session.getWebdavRootResource();

		if(categories.length()>0) {
			rootResource.proppatchMethod(filePath,new PropertyName("DAV:","categories"),categories,true);
			if(setOnParent){
				rootResource.proppatchMethod(getParentResource(filePath),new PropertyName("DAV:","categories"),categories,true);
			}
		}   

		// clear cached values so that they are reloaded
		clear();
	}

	public void setMetadata(String resourcePath, String type, String val) throws IOException {
		IWContext iwc = IWContext.getInstance();
		IWSlideSession session = (IWSlideSession)IBOLookup.getSessionInstance(iwc,IWSlideSession.class);
		IWSlideService service = (IWSlideService)IBOLookup.getServiceInstance(iwc,IWSlideService.class);

		WebdavRootResource rootResource = session.getWebdavRootResource();

		String filePath = resourcePath;
		String serverURI = service.getWebdavServerURI();
		if(!resourcePath.startsWith(serverURI)) {
			filePath = service.getURI(resourcePath);
		}

		//Store new settings
		if(type.length()>0) {
//				System.out.println("Proppatch: filepath="+filePath+" type="+type+" value="+val);
			rootResource.proppatchMethod(filePath,new PropertyName("DAV:",type),val,true);
			//Also set the metadata on the parent folder
			rootResource.proppatchMethod(getParentResource(filePath),new PropertyName("DAV:",type),val,true);
		}

		// clear cached values so that they are reloaded
		clear();
	}

	/**
	 * <p>
	 * Gets the URI to the parent resource of resource with URI resourceUri
	 * </p>
	 * @param resourceUri
	 * @return
	 */
	private static String getParentResource(String resourceUri) {
		int begin = 0;
		int end = Math.max(resourceUri.lastIndexOf("/"),resourceUri.lastIndexOf("\\"));
		resourceUri = resourceUri.substring(begin,end);
		return resourceUri;
	}

}
