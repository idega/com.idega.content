/*
 * Created on 9.8.2004
 *
 * Copyright (C) 2004 Idega Software hf. All Rights Reserved.
 *
 * This software is the proprietary information of Idega hf.
 * Use is subject to license terms.
 */
package com.idega.content.business;

import com.idega.content.data.ContentCategory;
import com.idega.content.data.ContentItem;
import com.idega.content.data.ContentItemVersion;
import com.idega.user.data.User;


/**
 * Main Service interface for the content repository
 * @author <a href="mailto:tryggvil@idega.com">tryggvil</a>
 * @version 1.0
 */
public interface ContentService {
	
	
	//Draft methods:
	
	public ContentItem createNewContentItem(ContentCategory category,User user);
	public ContentItem createNewContentItem(ContentCategory category,User user,String contentValue);
	
	public ContentItemVersion updateContentItem(ContentItem item,User user,String newContentValue);
	public boolean deleteContentItem(ContentItem item,User user);
	
}
