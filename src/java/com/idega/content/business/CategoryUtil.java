/*
 * $Id: CategoryUtil.java,v 1.2 2005/03/18 16:10:57 joakim Exp $
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
import com.idega.business.IBOLookup;
import com.idega.business.IBOLookupException;
import com.idega.idegaweb.IWUserContext;
import com.idega.presentation.IWContext;
import com.idega.slide.business.IWSlideSession;
import com.idega.slide.util.WebdavRootResource;


/**
 * <p>Utility functions for category. 
 * functions for getting and setting all the available categories</p>
 *  Last modified: $Date: 2005/03/18 16:10:57 $ by $Author: joakim $
 * 
 * @author <a href="mailto:Joakim@idega.com">Joakim</a>
 * @version $Revision: 1.2 $
 */
public class CategoryUtil {
	private static final String CATEORY_FILE_PATH = "categories.properties";
	
	/**
	 * <p> Get a collection of categories </p>
	 * @return collection of strings
	 */
	public static Collection getCategories() {
		Collection ret = new ArrayList();
		
		String categories = getCategoriesAsString();
		StringTokenizer st = new StringTokenizer(categories,",");
		while(st.hasMoreTokens()) {
			ret.add(st.nextToken().trim());
		}
		
		return ret;
	}
	
	/**
	 * <p>Getts all the categories as a string</p>
	 * @return categories
	 */
	public static String getCategoriesAsString() {
		IWUserContext iwuc = IWContext.getInstance();
		String categories = null;
		try {
			IWSlideSession session = (IWSlideSession)IBOLookup.getSessionInstance(iwuc,IWSlideSession.class);
			WebdavRootResource rootResource = session.getWebdavRootResource();
//			String filePath = service.getURI(CATEORY_FILE_PATH);
//			System.out.println("Loading categories for "+filePath);
			//TODO have to fix the path
			categories = rootResource.getMethodDataAsString(CATEORY_FILE_PATH);
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
	public static void storeCategories(Collection categories) {
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
	public static void storeCategories(String categories) {
		IWContext iwc = IWContext.getInstance();
		
		try {
			IWSlideSession session = (IWSlideSession)IBOLookup.getSessionInstance(iwc,IWSlideSession.class);

			WebdavRootResource rootResource = session.getWebdavRootResource();
			rootResource.putMethod(CATEORY_FILE_PATH, categories);
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
	public static void addCategory(String category) {
		if(getCategories().contains(category)) {
			return;
		}
		StringBuffer sb = new StringBuffer(getCategoriesAsString());
		if(sb.length()>0) {
			sb.append(",");
		}
		sb.append(category);
		storeCategories(sb.toString());
	}

}
