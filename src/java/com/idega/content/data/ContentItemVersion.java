/*
 * Created on 9.8.2004
 *
 * Copyright (C) 2004 Idega Software hf. All Rights Reserved.
 *
 * This software is the proprietary information of Idega hf.
 * Use is subject to license terms.
 */
package com.idega.content.data;


/**
 * 
 * @author <a href="mailto:tryggvil@idega.com">tryggvil</a>
 * @version 1.0
 */
public interface ContentItemVersion {
	
	public ContentItem getContentItem();
	public String getVersionId();
	
}
