/*
 * $Id: ContentSearch.java,v 1.2 2005/01/18 10:43:38 gimmi Exp $
 * Created on Jan 17, 2005
 *
 * Copyright (C) 2005 Idega Software hf. All Rights Reserved.
 *
 * This software is the proprietary information of Idega hf.
 * Use is subject to license terms.
 */
package com.idega.content.business;

import java.io.IOException;
import java.net.URLDecoder;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Enumeration;
import java.util.List;
import java.util.Map;
import org.apache.commons.httpclient.Credentials;
import org.apache.commons.httpclient.Header;
import org.apache.commons.httpclient.HostConfiguration;
import org.apache.commons.httpclient.HttpClient;
import org.apache.commons.httpclient.HttpException;
import org.apache.commons.httpclient.HttpState;
import org.apache.commons.httpclient.HttpURL;
import org.apache.webdav.lib.PropertyName;
import org.apache.webdav.lib.WebdavState;
import org.apache.webdav.lib.methods.SearchMethod;
import org.apache.webdav.lib.search.SearchException;
import org.apache.webdav.lib.search.SearchExpression;
import org.apache.webdav.lib.search.SearchRequest;
import org.apache.webdav.lib.search.SearchScope;
import com.idega.business.IBOLookup;
import com.idega.business.IBOLookupException;
import com.idega.core.search.business.Searchable;
import com.idega.idegaweb.IWBundle;
import com.idega.presentation.IWContext;
import com.idega.presentation.PresentationObject;
import com.idega.presentation.text.Link;
import com.idega.slide.business.IWSlideSession;


/**
 * 
 *  Last modified: $Date: 2005/01/18 10:43:38 $ by $Author: gimmi $
 * This class implements the Searchable interface and can therefor be used in a Search block for searching contents of the files in webdav.
 * To use it simply register this class as a iw.searchable component in a bundle.
 * @author <a href="mailto:eiki@idega.com">Eirikur S. Hrafnsson</a>
 * @version $Revision: 1.2 $
 */
public class ContentSearch implements Searchable {

	public static final String SEARCH_NAME_LOCALIZABLE_KEY = "content_search.name";
	public static final String DEFAULT_LINK_STYLE_CLASS = "content_search_link";
	
	private String linkStyleClass = DEFAULT_LINK_STYLE_CLASS;
	
	static final PropertyName DISPLAYNAME = new PropertyName("DAV:", "displayname");

	static final PropertyName LASTMODIFIED = new PropertyName("DAV:", "getlastmodified");
	
	
	/**
	 * 
	 */
	public ContentSearch() {
		super();
	}

	/* (non-Javadoc)
	 * @see com.idega.core.search.business.Searchable#initialize(com.idega.presentation.IWContext)
	 */
	public boolean initialize(IWContext iwc) {
		// TODO Auto-generated method stub
		return true;
	}

	/* (non-Javadoc)
	 * @see com.idega.core.search.business.Searchable#destroy(com.idega.presentation.IWContext)
	 */
	public void destroy(IWContext iwc) {
		// TODO Auto-generated method stub
		
	}

	/* (non-Javadoc)
	 * @see com.idega.core.search.business.Searchable#getSimpleSearchResults(java.lang.String, com.idega.presentation.IWContext)
	 */
	public Collection getSimpleSearchResults(String queryString, IWContext iwc) {
		List results = new ArrayList();
		try {
			IWSlideSession session = (IWSlideSession) IBOLookup.getSessionInstance(iwc, IWSlideSession.class);
			//IWSlideService service =
			// (IWSlideService)IBOLookup.getServiceInstance(iwuc,IWSlideService.class);
			SearchRequest s = new SearchRequest();
			s.addSelection(DISPLAYNAME);
			s.addSelection(LASTMODIFIED);
			s.addScope(new SearchScope("files"));
			SearchExpression expression = s.contains(queryString);
			s.setWhereExpression(expression);
			String search = s.asString();
			System.out.println(search);
			SearchMethod method = new SearchMethod("/content", search);
			HttpURL httpURL = new HttpURL("localhost", 8090, "/content");
			HttpClient client = new HttpClient();
			client.setState(new WebdavState());
			HostConfiguration hostConfig = client.getHostConfiguration();
			hostConfig.setHost(httpURL);
			
	        Credentials hostCredentials = session.getUserCredentials();
	
	        if (hostCredentials != null) {
	            HttpState clientState = client.getState();
	            clientState.setCredentials(null, httpURL.getHost(),hostCredentials);
	            clientState.setAuthenticationPreemptive(true);
	        }
			        
			        
			int state = client.executeMethod(method);
			System.out.println("State: " + state);
			Header[] headers = method.getResponseHeaders();
			for (int i = 0; i < headers.length; i++) {
				System.out.println(headers[i].toString());
			}
			Enumeration enumer = method.getAllResponseURLs();
			while (enumer.hasMoreElements()) {
				String url = (String) enumer.nextElement();
				System.out.println(url);
				results.add(url);
			}
			return results;
		}
		catch (IBOLookupException e) {
			e.printStackTrace();
		}
		catch (SearchException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		catch (HttpException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		return new ArrayList();
	}

	/* (non-Javadoc)
	 * @see com.idega.core.search.business.Searchable#getAdvancedSearchResults(java.util.Map, com.idega.presentation.IWContext)
	 */
	public Collection getAdvancedSearchResults(Map queryMap, IWContext iwc) {
		return null;
	}

	/* (non-Javadoc)
	 * @see com.idega.core.search.business.Searchable#getAdvancedSearchSupportedParameters()
	 */
	public List getAdvancedSearchSupportedParameters() {
		return null;
	}

	/* (non-Javadoc)
	 * @see com.idega.core.search.business.Searchable#getSupportsSimpleSearch()
	 */
	public boolean getSupportsSimpleSearch() {
		return true;
	}

	/* (non-Javadoc)
	 * @see com.idega.core.search.business.Searchable#getSupportsAdvancedSearch()
	 */
	public boolean getSupportsAdvancedSearch() {
		return false;
	}

	/* (non-Javadoc)
	 * @see com.idega.core.search.business.Searchable#getSearchName(com.idega.presentation.IWContext)
	 */
	public String getSearchName(IWContext iwc) {
		IWBundle bundle = ContentUtil.getBundle();
		return bundle.getResourceBundle(iwc).getLocalizedString(SEARCH_NAME_LOCALIZABLE_KEY,"Documents");
	}

	/* (non-Javadoc)
	 * @see com.idega.core.search.business.Searchable#getResultPresentation(java.lang.Object, com.idega.presentation.IWContext)
	 */
	public PresentationObject getResultPresentation(Object resultObject, IWContext iwc) {
		String fileURI = (String)resultObject;
		Link link = new Link(URLDecoder.decode(fileURI.substring(fileURI.lastIndexOf("/")+1))+" ("+fileURI.substring(0,fileURI.lastIndexOf("/")+1)+")",fileURI);
		link.setStyleClass(getLinkStyleClass());
		return link;
	}
	
	/**
	 * @return Returns the linkStyleClass.
	 */
	public String getLinkStyleClass() {
		return linkStyleClass;
	}
	/**
	 * @param linkStyleClass The linkStyleClass to set.
	 */
	public void setLinkStyleClass(String linkStyleClass) {
		this.linkStyleClass = linkStyleClass;
	}
	
}
