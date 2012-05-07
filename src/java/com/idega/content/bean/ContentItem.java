/*
 * $Id: ContentItem.java,v 1.7 2009/01/06 15:17:24 tryggvil Exp $
 * Created on 28.1.2005
 *
 * Copyright (C) 2005 Idega Software hf. All Rights Reserved.
 *
 * This software is the proprietary information of Idega hf.
 * Use is subject to license terms.
 */
package com.idega.content.bean;

import java.sql.Timestamp;
import java.util.Locale;

import com.idega.presentation.IWContext;


/**
 *  <p>
 *  Class which is a general interface for working with 'Content Items'.<br/>
 *  Content Items are pieces of content stored as files or resources in the Content (WebDav/JCR) repository.
 *  </p>
 *  Last modified: $Date: 2009/01/06 15:17:24 $ by $Author: tryggvil $
 * 
 * @author <a href="mailto:gummi@idega.com">Gudmundur Agust Saemundsson</a>
 * @version $Revision: 1.7 $
 */
public interface ContentItem {
	public Object getValue(String fieldName);
	public void setValue(String fieldName, Object value);
	//public List getAttachments();
	
	/**
	 * <p>
	 * This method tells the ContentItemViewer which fields to show, that is it iterates through
	 * the returning array and gets the value of those fields and presents them.
	 * </p>
	 * @return
	 */
	public String[] getContentFieldNames();
	public String getContentItemPrefix();
	
	public Timestamp getCreationDate();
	public Timestamp getLastModifiedDate();
	public String getVersionName();
	/**
	 * <p>
	 * Path to to the resource for this contentItem in the WebDav/JCR respository.
	 * </p>
	 * @return
	 */
	public String getResourcePath();
	public Locale getLocale();
	
	public Boolean getRendered();
	
	public String[] getToolbarActions();
	
	public void load()throws Exception;
	public void store()throws Exception;
	public void delete()throws Exception;
	public boolean createItemFolder();
	public boolean createItemFolder(IWContext iwc);
	
	
}
