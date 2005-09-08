/*
 * $Id: CategoryBean.java,v 1.1 2005/09/08 23:08:41 tryggvil Exp $
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
import java.util.Iterator;
import java.util.StringTokenizer;
import org.apache.commons.httpclient.HttpException;
import org.apache.webdav.lib.util.WebdavStatus;
import com.idega.business.IBOLookup;
import com.idega.business.IBOLookupException;
import com.idega.idegaweb.IWMainApplication;
import com.idega.idegaweb.IWUserContext;
import com.idega.presentation.IWContext;
import com.idega.slide.business.IWSlideService;
import com.idega.slide.business.IWSlideSession;
import com.idega.slide.util.WebdavRootResource;


/**
 * <p>
 * Class for manipulating Categories that are stored in slide.<br/>
 * Includes functions for getting and setting all the available categories
 * </p>
 *  Last modified: $Date: 2005/09/08 23:08:41 $ by $Author: tryggvil $
 * 
 * @author <a href="mailto:Joakim@idega.com">Joakim</a>,<a href="mailto:tryggvi@idega.com">Tryggvi Larusson</a>
 * @version $Revision: 1.1 $
 */
public class CategoryBean {
	
	private static String BEAN_KEY="ContentCategoryBean";
	
	//private static final String CATEORY_FIX_PREFIX = "/agura-intra/content";
	private static final String CATEORY_CONFIG_PATH = "/files/cms/";
	private static final String CATEORY_CONFIG_FILE = CATEORY_CONFIG_PATH+"categories.prop";
//	private static final String CATEORY_CONFIG_FILE = CATEORY_CONFIG_PATH+"cat.prp";
	private IWMainApplication iwma;

	protected CategoryBean(IWMainApplication iwma){
		this.iwma=iwma;
	}
	
	/**
	 * <p>
	 * Get the instance of the bean for this application.
	 * </p>
	 * @return
	 */
	public static CategoryBean getInstance(){
		IWMainApplication iwma = IWMainApplication.getDefaultIWMainApplication();
		CategoryBean bean = (CategoryBean) iwma.getAttribute(BEAN_KEY);
		if(bean==null){
			bean = new CategoryBean(iwma);
			iwma.setAttribute(BEAN_KEY,bean);
		}
		return bean;
	}
	
	public IWSlideService getSlideService(){
		try {
			return (IWSlideService)IBOLookup.getServiceInstance(iwma.getIWApplicationContext(),IWSlideService.class);
		}
		catch (IBOLookupException e) {
			throw new RuntimeException("Error getting IWSlideService");
		}
	}
	
	/**
	 * <p> Get a collection of categories </p>
	 * @return collection of strings
	 */
	public Collection getCategories() {
		Collection ret = new ArrayList();
		
		String categories = getCategoriesAsString();
		if( categories != null){
			StringTokenizer st = new StringTokenizer(categories,",");
			while(st.hasMoreTokens()) {
				ret.add(st.nextToken().trim());
			}
		}
		return ret;
	}
	
	/**
	 * <p>Getts all the categories as one String (comma separated)</p>
	 * @return categories
	 */
	public String getCategoriesAsString() {
		IWUserContext iwuc = IWContext.getInstance();
		String categories = null;
		try {
			IWSlideSession session = (IWSlideSession)IBOLookup.getSessionInstance(iwuc,IWSlideSession.class);
			WebdavRootResource rootResource = session.getWebdavRootResource();
//			String filePath = service.getURI(CATEORY_FILE_PATH);
//			System.out.println("Loading categories for "+filePath);
			
			String path = getSlideService().getURI(CATEORY_CONFIG_FILE);
			
			categories = rootResource.getMethodDataAsString(path);
			if(rootResource.getStatusCode() != WebdavStatus.SC_OK){
				return "";
			}
		}
		catch (IBOLookupException e) {
			e.printStackTrace();
		}
		catch (RemoteException e) {
			e.printStackTrace();
		}
		catch (HttpException e) {
			e.printStackTrace();
		}
		catch (IOException e) {
			e.printStackTrace();
		}
		return categories;
	}
	
	/**
	 * <p>Stores all the given categories</p>
	 * @param categories
	 */
	public void storeCategories(Collection categories) {
		//Create a string out of the collection
		StringBuffer sb = new StringBuffer();
		boolean first = true;
		Iterator iter = categories.iterator();
		while(iter.hasNext()) {
			if(!first) {
				sb.append(",");
			}
			first=false;
			sb.append(iter.next().toString());
		}
		storeCategories(sb.toString());
	}

	/**
	 * <p>Store categories</p>
	 * @param categories
	 */
	public void storeCategories(String categories) {
		IWContext iwc = IWContext.getInstance();
		
		try {
			IWSlideSession session = (IWSlideSession)IBOLookup.getSessionInstance(iwc,IWSlideSession.class);

			WebdavRootResource rootResource = session.getWebdavRootResource();
			//TODO make sure that the folder exists
			//boolean hadToCreate = 
			String path = getSlideService().getURI(CATEORY_CONFIG_PATH);
			String filePath = getSlideService().getURI(CATEORY_CONFIG_FILE);
			session.createAllFoldersInPath(path);
//			System.out.println("Had to create folder "+hadToCreate);
//			System.out.println("Storing categories "+categories+" to file "+CATEORY_CONFIG_FILE);
			boolean putOK = rootResource.putMethod(CATEORY_CONFIG_FILE, categories);
//			System.out.println("Put to "+CATEORY_CONFIG_FILE+" was "+putOK);
			if(!putOK) {
				putOK = rootResource.putMethod(filePath, categories);
				System.out.println("Put to "+ filePath +" was "+putOK);
			}
//			IWSlideService service = (IWSlideService)IBOLookup.getServiceInstance(iwc,IWSlideService.class);
//			String filePath = service.getURI(CATEORY_FILE_PATH);
//			WebdavResource webdavResource = new WebdavResource(filePath);
//			webdavResource.putMethod(categories);
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
	}
	
	/**
	 * <p>Adds a category to the available categories</p>
	 * @param category
	 */
	public void addCategory(String category) {
		if(getCategories().contains(category)) {
			return;
		}
		StringBuffer sb = new StringBuffer(
				getCategoriesAsString()
				);
		if(sb.length()>0 && !sb.toString().endsWith(",")) {
			sb.append(",");
		}
		sb.append(category);
		System.out.println("New category string to store:"+sb.toString()+"  New category:"+category);
		storeCategories(sb.toString());
	}

}
