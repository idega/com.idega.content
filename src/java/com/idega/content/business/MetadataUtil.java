/*
 * $Id: MetadataUtil.java,v 1.1 2005/01/24 17:15:58 joakim Exp $
 *
 * Copyright (C) 2004 Idega. All Rights Reserved.
 *
 * This software is the proprietary information of Idega.
 * Use is subject to license terms.
 *
 */
package com.idega.content.business;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Properties;
import java.util.StringTokenizer;


/**
 * 
 * Last modified: $Date: 2005/01/24 17:15:58 $ by $Author: joakim $
 * 
 * Utility class for Metadata
 *
 * @author Joakim Johnson
 * @version $Revision: 1.1 $
 */
public class MetadataUtil {
//	public static final String[] metadataType = new String[] {"categories","keywords","publisher"};
	private static ArrayList metadataType = new ArrayList();
	private static final String TYPE_KEY = "type";
	
	private static final String METADATA_PATH = "/properties/MetadataType.properties";
	
	/**
	 * Fetches metadata types from properties file
	 * @return ArrayList of metadata types as strings
	 */
	public static ArrayList getMetadataTypes() {
		//Only read file first time
		if(metadataType.size() == 0) {
			Properties prop = new Properties();
			String path = new ContentUtil().getBundle().getBundleBaseRealPath()+METADATA_PATH;
			try {
				prop.load(new java.io.FileInputStream(path));
				String type = prop.getProperty(TYPE_KEY);
				StringTokenizer st = new StringTokenizer(type,",");
				while(st.hasMoreTokens()) {
					metadataType.add(st.nextToken().trim());
				}
			}
			catch (FileNotFoundException e) {
				e.printStackTrace();
			}
			catch (IOException e) {
				e.printStackTrace();
			}
		}
		return metadataType;
	}
}
