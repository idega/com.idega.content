/*
 * $Id: ContentItem.java,v 1.1 2005/02/07 10:59:41 gummi Exp $
 * Created on 28.1.2005
 *
 * Copyright (C) 2005 Idega Software hf. All Rights Reserved.
 *
 * This software is the proprietary information of Idega hf.
 * Use is subject to license terms.
 */
package com.idega.content.bean;

import java.util.List;


/**
 * 
 *  Last modified: $Date: 2005/02/07 10:59:41 $ by $Author: gummi $
 * 
 * @author <a href="mailto:gummi@idega.com">Gudmundur Agust Saemundsson</a>
 * @version $Revision: 1.1 $
 */
public interface ContentItem {
	public Object getValue(String fieldName);
	public void setValue(String fieldName, Object value);
	public List getAttachments();
	public String[] getContentFieldNames();
	public String getContentItemPrefix();
}
