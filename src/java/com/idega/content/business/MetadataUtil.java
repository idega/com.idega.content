/*
 * $Id: MetadataUtil.java,v 1.3 2006/02/20 11:03:21 laddi Exp $
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
import java.util.List;
import java.util.Properties;
import java.util.StringTokenizer;

/**
 *
 * Last modified: $Date: 2006/02/20 11:03:21 $ by $Author: laddi $
 *
 * Utility class for Metadata
 *
 * @author Joakim Johnson
 * @version $Revision: 1.3 $
 */
public class MetadataUtil {

	private static List<String> metadataType = new ArrayList<String>();
	private static final String TYPE_KEY = "type";
	private static final String METADATA_PATH = "/properties/MetadataType.properties";

	/**
	 * Fetches metadata types from properties file
	 * @return ArrayList of metadata types as strings
	 */
	public static List<String> getMetadataTypes() {
		//Only read file first time
		if(metadataType.size() == 0) {
			Properties prop = new Properties();
			//Path to properties file
			String path = ContentUtil.getBundle().getBundleBaseRealPath()+METADATA_PATH;
			try {
				//read property and store comma separated values into arraylist
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