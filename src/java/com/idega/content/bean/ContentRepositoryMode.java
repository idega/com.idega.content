/**
 * $Id: ContentRepositoryMode.java,v 1.1 2009/01/06 15:17:24 tryggvil Exp $
 * Created in 2009 by tryggvil
 *
 * Copyright (C) 2000-2009 Idega Software hf. All Rights Reserved.
 *
 * This software is the proprietary information of Idega hf.
 * Use is subject to license terms.
 */
package com.idega.content.bean;

import org.springframework.context.annotation.Scope;
import org.springframework.stereotype.Service;

import com.idega.idegaweb.IWMainApplication;

/**
 * <p>
 * Class to control central settings of the Content subsystem
 * </p>
 *  Last modified: $Date: 2009/01/06 15:17:24 $ by $Author: tryggvil $
 * 
 * @author <a href="mailto:tryggvil@idega.com">tryggvil</a>
 * @version $Revision: 1.1 $
 */
@Scope("singleton")
@Service(ContentRepositoryMode.SPRING_BEAN_IDENTIFIER)
public class ContentRepositoryMode {

	public static final String SPRING_BEAN_IDENTIFIER="contentRepositoryMode";
	
	private String persistenceMode;
	
	public final static String CONTENT_PERSISTENCE="content.persistence";
	public final static String CONTENT_PERSISTENCE_WEBDAV="webdav";
	public final static String CONTENT_PERSISTENCE_JCR="jcr";
	
	public final static String DEFAULT_CONTENT_PERSISTENCE=CONTENT_PERSISTENCE_WEBDAV;

	/*public void setPersistToWebDav(boolean persistToWebDav) {
	this.persistToWebDav = persistToWebDav;
	}*/

	public String getPersistenceMode(){
		if(this.persistenceMode==null){
			//Try detection from Application settings:
			String prop = IWMainApplication.getDefaultIWMainApplication().getSettings().getProperty(CONTENT_PERSISTENCE);
			if(prop==null){
				return DEFAULT_CONTENT_PERSISTENCE;
			}
			else{
				return prop;
			}
		}
		else{
			return this.persistenceMode;
		}
	}
	
	public void setPersistenceMode(String mode){
		this.persistenceMode=mode;
	}
	
	public boolean isPersistToWebDav() {
		return(getPersistenceMode().equals(CONTENT_PERSISTENCE_WEBDAV));
	}

	public boolean isPersistToJCR() {
		return(getPersistenceMode().equals(CONTENT_PERSISTENCE_JCR));
	}
	
	
}
