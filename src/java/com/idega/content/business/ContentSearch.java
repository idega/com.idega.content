/*
 * $Id: ContentSearch.java,v 1.9 2005/01/31 10:19:28 eiki Exp $
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
import java.rmi.RemoteException;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.List;
import org.apache.commons.httpclient.Credentials;
import org.apache.commons.httpclient.HostConfiguration;
import org.apache.commons.httpclient.HttpClient;
import org.apache.commons.httpclient.HttpException;
import org.apache.commons.httpclient.HttpState;
import org.apache.commons.httpclient.HttpURL;
import org.apache.webdav.lib.PropertyName;
import org.apache.webdav.lib.WebdavState;
import org.apache.webdav.lib.methods.SearchMethod;
import org.apache.webdav.lib.search.CompareOperator;
import org.apache.webdav.lib.search.SearchException;
import org.apache.webdav.lib.search.SearchExpression;
import org.apache.webdav.lib.search.SearchRequest;
import org.apache.webdav.lib.search.SearchScope;
import com.idega.business.IBOLookup;
import com.idega.business.IBOLookupException;
import com.idega.core.search.business.Search;
import com.idega.core.search.business.SearchPlugin;
import com.idega.core.search.business.SearchQuery;
import com.idega.core.search.data.BasicSearch;
import com.idega.core.search.data.BasicSearchResult;
import com.idega.core.search.data.SimpleSearchQuery;
import com.idega.idegaweb.IWBundle;
import com.idega.idegaweb.IWMainApplication;
import com.idega.presentation.IWContext;
import com.idega.slide.business.IWSlideService;
import com.idega.slide.business.IWSlideSession;


/**
 * 
 *  Last modified: $Date: 2005/01/31 10:19:28 $ by $Author: eiki $
 * This class implements the Searchplugin interface and can therefore be used in a Search block (com.idega.core.search)<br>
 *  for searching contents and properties (metadata) of the files in the iwfile system.
 * To use it simply register this class as a iw.searchable component in a bundle.
 * @author <a href="mailto:eiki@idega.com">Eirikur S. Hrafnsson</a>
 * @version $Revision: 1.9 $
 */
public class ContentSearch implements SearchPlugin {

	public static final String SEARCH_NAME_LOCALIZABLE_KEY = "content_search.name";
	public static final String SEARCH_DESCRIPTION_LOCALIZABLE_KEY = "content_search.description";
	
	public static final String SEARCH_TYPE = "document";


	static final PropertyName DISPLAYNAME = new PropertyName("DAV:", "displayname");
	static final PropertyName LASTMODIFIED = new PropertyName("DAV:", "getlastmodified");
	static final PropertyName CREATOR_DISPLAY_NAME = new PropertyName("DAV:", "creator-displayname");
	static final PropertyName COMMENT = new PropertyName("DAV:", "comment");
	
	private IWMainApplication iwma = null;
	private HttpURL httpURL;

	
	public ContentSearch() {
		super();
	}

	/* (non-Javadoc)
	 * @see com.idega.core.search.business.SearchPlugin#getAdvancedSearchSupportedParameters()
	 */
	public List getAdvancedSearchSupportedParameters() {
		return null;
	}

	/* (non-Javadoc)
	 * @see com.idega.core.search.business.SearchPlugin#getSupportsSimpleSearch()
	 */
	public boolean getSupportsSimpleSearch() {
		return true;
	}

	/* (non-Javadoc)
	 * @see com.idega.core.search.business.SearchPlugin#getSupportsAdvancedSearch()
	 */
	public boolean getSupportsAdvancedSearch() {
		return false;
	}
	

	/* (non-Javadoc)
	 * @see com.idega.core.search.business.SearchPlugin#initialize(com.idega.idegaweb.IWMainApplication)
	 */
	public boolean initialize(IWMainApplication iwma) {
		this.iwma = iwma;
		try {
			IWSlideService service = (IWSlideService) IBOLookup.getServiceInstance(iwma.getIWApplicationContext(), IWSlideService.class);
			httpURL = service.getWebdavServerURL();
		}
		catch (IBOLookupException e) {
			e.printStackTrace();
			return false;
		}
		catch (RemoteException e) {
			e.printStackTrace();
			return false;
		}

		return true;
	}

	/* (non-Javadoc)
	 * @see com.idega.core.search.business.SearchPlugin#destroy(com.idega.idegaweb.IWMainApplication)
	 */
	public void destroy(IWMainApplication iwma) {
	}

	/* (non-Javadoc)
	 * @see com.idega.core.search.business.SearchPlugin#createSearch(com.idega.core.search.business.SearchQuery)
	 */
	public Search createSearch(SearchQuery searchQuery) {
		List results = new ArrayList();
		BasicSearch searcher = new BasicSearch();
		searcher.setSearchName(getSearchName());
		searcher.setSearchType(SEARCH_TYPE);
		searcher.setSearchQuery(searchQuery);
		
		try {
			IWSlideSession session = (IWSlideSession) IBOLookup.getSessionInstance(IWContext.getInstance(), IWSlideSession.class);
			String servletMapping = session.getWebdavServerURI();
			String queryString = ((SimpleSearchQuery)searchQuery).getSimpleSearchQuery();
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
	        
	        
			String content = getContentSearch(queryString);
			SearchMethod contentSearch = new SearchMethod(servletMapping, content);

			String property = getPropertySearch(queryString);
			SearchMethod propertySearch = new SearchMethod(servletMapping, property);
			
			        
			        
			executeSearch(results, servletMapping, contentSearch, client);
			executeSearch(results, servletMapping, propertySearch, client);
			
			searcher.setSearchResults(results);
		}
		catch (IBOLookupException e) {
			e.printStackTrace();
		}
		catch (SearchException e) {
			e.printStackTrace();
		}
		catch (HttpException e) {
			e.printStackTrace();
		}
		catch (IOException e) {
			e.printStackTrace();
		}
		
		return searcher;
	}

	protected String getContentSearch(String queryString) throws SearchException {
		SearchRequest s = new SearchRequest();
		//s.addSelection(DISPLAYNAME);
		s.addSelection(LASTMODIFIED);
		s.addScope(new SearchScope("files"));
		
		SearchExpression expression = s.contains(queryString);
		s.setWhereExpression(expression);
		String search = s.asString();
		//System.out.println(search);
		return search;
	}
	
	protected String getPropertySearch(String queryString) throws SearchException {
		SearchRequest s = new SearchRequest();
		//s.addSelection(DISPLAYNAME);
		s.addSelection(LASTMODIFIED);
		s.addScope(new SearchScope("files"));
		SearchExpression expression = s.or( s.compare(CompareOperator.LIKE, DISPLAYNAME,"%"+queryString+"%"),
				s.or(s.compare(CompareOperator.LIKE, CREATOR_DISPLAY_NAME,queryString),s.compare(CompareOperator.LIKE, COMMENT,"%"+queryString+"%")) );
		//add other properties
		s.setWhereExpression(expression);
		String search = s.asString();
		//System.out.println(search);
		return search;
	}

	protected void executeSearch(List results, String servletMapping, SearchMethod method, HttpClient client) throws IOException, HttpException {
		int state = client.executeMethod(method);
		//todo remove
		System.out.println("DASL Search result state: " + state);
//		Header[] headers = method.getResponseHeaders();
//		for (int i = 0; i < headers.length; i++) {
//			System.out.println(headers[i].toString());
//		}
		Enumeration enum = method.getAllResponseURLs();
		
		while (enum.hasMoreElements()) {
			String fileURI = (String) enum.nextElement();
			if(!fileURI.equalsIgnoreCase(servletMapping)){
				BasicSearchResult result = new BasicSearchResult();
				
				result.setSearchResultType(SEARCH_TYPE);
				result.setSearchResultURI(fileURI);
				result.setSearchResultName(URLDecoder.decode(fileURI.substring(fileURI.lastIndexOf("/")+1)));
				result.setSearchResultExtraInformation(URLDecoder.decode(fileURI.substring(0,fileURI.lastIndexOf("/")+1)));
				
				results.add(result);
			}
		}
	}

	/* (non-Javadoc)
	 * @see com.idega.core.search.business.SearchPlugin#getSearchName()
	 */
	public String getSearchName() {
		IWBundle bundle = ContentUtil.getBundle();
		return bundle.getResourceBundle(IWContext.getInstance()).getLocalizedString(SEARCH_NAME_LOCALIZABLE_KEY,"Documents");
	}

	/* (non-Javadoc)
	 * @see com.idega.core.search.business.SearchPlugin#getSearchDescription()
	 */
	public String getSearchDescription() {
		IWBundle bundle = ContentUtil.getBundle();
		return bundle.getResourceBundle(IWContext.getInstance()).getLocalizedString(SEARCH_DESCRIPTION_LOCALIZABLE_KEY,"Searches the contents of documents in an IdegaWeb file system.");
	}
	
}
