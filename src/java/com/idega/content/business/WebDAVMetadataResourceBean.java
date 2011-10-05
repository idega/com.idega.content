/*
 * $Id: WebDAVMetadataResourceBean.java,v 1.16 2009/05/15 07:23:54 valdas Exp $
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
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jcr.Node;
import javax.jcr.Property;
import javax.jcr.RepositoryException;
import javax.jcr.Value;

import com.idega.builder.bean.AdvancedProperty;
import com.idega.business.IBOSessionBean;
import com.idega.content.business.categories.CategoryBean;
import com.idega.content.data.MetadataValueBean;
import com.idega.util.ArrayUtil;
import com.idega.util.CoreConstants;
import com.idega.util.ListUtil;
import com.idega.util.StringUtil;


/**
 * A resource bean that holds metadata info for the selected resouce
 *
 * Last modified: $Date: 2009/05/15 07:23:54 $ by $Author: valdas $
 *
 * @author Joakim Johnson
 * @version $Revision: 1.16 $
 */
public class WebDAVMetadataResourceBean extends IBOSessionBean implements WebDAVMetadataResource {

	private static final long serialVersionUID = -4731482043715283036L;

	private Collection<MetadataValueBean> metadataBeans = null;	//Holding MetadataValueBean
	private Collection<String> selectedCategories = null;

	private String currentPath = null;

	public WebDAVMetadataResourceBean() {
		super();
	}

	/**
	 * Clears the metadata beans (cache)
	 */
	@Override
	public void clear() {
		this.metadataBeans = null;
		this.selectedCategories=null;
		this.currentPath=null;
	}

	private void setMetadataBeans(String resourcePath, Collection<MetadataValueBean> meta) {
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
	@Override
	public Collection<MetadataValueBean> getMetadataBeans(String resourcePath) throws RemoteException, IOException {
		if(this.metadataBeans == null || !checkPath(resourcePath)) {
			setMetadataBeans(resourcePath, getMetadataFromRepository(resourcePath));
		}
		return this.metadataBeans;
	}


	/**
	 * returns categories selected for the article specified by the given resourcePath
	 * @return a collection of Strings
	 */
	@Override
	public Collection<String> getCategories(String resourcePath) throws RemoteException, IOException {
		if(selectedCategories == null || !checkPath(resourcePath)) {
			setSelectedCategories(resourcePath, getCategoriesFromRepository(resourcePath));
		}
		return this.selectedCategories;
	}

	@Override
	public MetadataValueBean[] getMetadata(String resourcePath) throws RemoteException, IOException {
		return getMetadataBeans(resourcePath).toArray(new MetadataValueBean[this.metadataBeans.size()]);
	}

	/**
	 * <p> returns a collection of metadata for the given resource</p>
	 * @param resourcePath
	 * @return collection of MetadataValueBean
	 * @throws RemoteException
	 * @throws IOException
	 */
	protected Collection<MetadataValueBean> getMetadataFromRepository(String resourcePath) throws RemoteException, IOException {
		metadataBeans = new ArrayList<MetadataValueBean>();

		Map<String, List<String>> metaDataValues = getMetaDataValues(MetadataUtil.getMetadataTypes(), resourcePath);
		if (metaDataValues == null || metaDataValues.isEmpty())
			return metadataBeans;

		for (String type: metaDataValues.keySet()) {
			List<String> typeValues = metaDataValues.get(type);
			if (ListUtil.isEmpty(typeValues))
				continue;

			StringBuffer values = new StringBuffer();
			for (Iterator<String> typeValuesIter = typeValues.iterator(); typeValuesIter.hasNext();) {
				values.append(typeValuesIter.next());
				if (typeValuesIter.hasNext())
					values.append(CoreConstants.COMMA);
			}
			metadataBeans.add(new MetadataValueBean(type, values.toString()));
		}

		return metadataBeans;
	}

	private Map<String, List<String>> getMetaDataValues(List<String> metaData, String resourcePath) {
		String filePath = resourcePath;
		String serverURI = getRepositoryService().getWebdavServerURL();
		if(!resourcePath.startsWith(serverURI)) {
			filePath = getRepositoryService().getURI(resourcePath);
		}

		Map<String, List<String>> metaDataValues = new HashMap<String, List<String>>();
		try {
			Node item = getRepositoryService().getNodeAsRootUser(filePath);
			if (item == null)
				return Collections.emptyMap();

			for (String type: metaData) {
				String propName = "DAV:".concat(type);
				if (!item.hasProperty(propName)) {
					log(Level.WARNING, "Item " + item + " does not have property " + propName);
					continue;
				}

				Property prop = item.getProperty(propName);
				Value[] values = prop.getValues();
				if (ArrayUtil.isEmpty(values)) {
					logWarning("Item " + item + " has property " + propName + " but there are no values assigned!");
					continue;
				}

				List<String> metaValues = metaDataValues.get(propName);
				if (metaValues == null) {
					metaValues = new ArrayList<String>();
					metaDataValues.put(propName, metaValues);
				}
				for (int i = 0; i < values.length; i++) {
					String value = values[i].getString();
					if (StringUtil.isEmpty(value))
						continue;

					metaValues.add(value);
				}
			}
		} catch (Exception e) {
			log(Level.WARNING, "Could not load metadata types " + metaData + " for " + filePath, e);
		}

		return metaDataValues;
	}

	/**
	 * <p> Get all the selected categories for the selected resource.
	 * This functionality might be moved to a separate class</p>
	 * @param resourcePath
	 * @return Collections of strings
	 * @throws RemoteException
	 * @throws IOException
	 */
	protected Collection<String> getCategoriesFromRepository(String resourcePath) throws RemoteException, IOException {
		Map<String, List<String>> metaDataValues = getMetaDataValues(Arrays.asList("categories"), resourcePath);
		if (metaDataValues == null || metaDataValues.isEmpty())
			return Collections.emptyList();

		StringBuffer values = new StringBuffer();
		try {
			for (String type: metaDataValues.keySet()) {
				List<String> typeValues = metaDataValues.get(type);
				if (ListUtil.isEmpty(typeValues))
					continue;

				for (Iterator<String> typeValuesIter = typeValues.iterator(); typeValuesIter.hasNext();) {
					values.append(typeValuesIter.next());
					if (typeValuesIter.hasNext())
						values.append(CoreConstants.COMMA);
				}
			}

			this.selectedCategories=CategoryBean.getCategoriesFromString(values.toString());

		} catch (Exception e) {
			Logger.getLogger(WebDAVMetadataResourceBean.class.getName()).log(Level.SEVERE, "Warning could not load categories for "+ resourcePath, e);
		}

		selectedCategories = CategoryBean.getCategoriesFromString(values.toString());
		return selectedCategories;
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
		return true;
	}

	@Override
	public void setCategories(String resourcePath, String categories, boolean setOnParent) throws IOException {
		String filePath = resourcePath;
		String serverURI = getRepositoryService().getWebdavServerURL();
		if(!resourcePath.startsWith(serverURI)) {
			filePath = getRepositoryService().getURI(resourcePath);
		}

		if (StringUtil.isEmpty(categories))
			return;

		setProperties(filePath, new AdvancedProperty("DAV:categories", categories));
		if (setOnParent)
			setProperties(getParentResource(filePath), new AdvancedProperty("DAV:categories", categories));

		// clear cached values so that they are reloaded
		clear();
	}

	private boolean setProperties(String path, AdvancedProperty... properties) {
		try {
			return getRepositoryService().setProperties(getRepositoryService().getNodeAsRootUser(path), properties);
		} catch (RepositoryException e) {
			e.printStackTrace();
		}
		return Boolean.FALSE;
	}

	@Override
	public void setMetadata(String resourcePath, String type, String val) throws IOException {
		if (StringUtil.isEmpty(type))
			return;

		String filePath = resourcePath;
		String serverURI = getRepositoryService().getWebdavServerURL();
		if (!resourcePath.startsWith(serverURI))
			filePath = getRepositoryService().getURI(resourcePath);

		//Store new settings
		setProperties(filePath, new AdvancedProperty("DAV:".concat(type), val));
		//Also set the metadata on the parent folder
		setProperties(getParentResource(filePath), new AdvancedProperty("DAV:".concat(type), val));

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
		int end = Math.max(resourceUri.lastIndexOf(CoreConstants.SLASH),resourceUri.lastIndexOf("\\"));
		resourceUri = resourceUri.substring(begin,end);
		return resourceUri;
	}

}