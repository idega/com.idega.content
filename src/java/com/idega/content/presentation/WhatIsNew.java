/*
 * $Id: WhatIsNew.java,v 1.1.2.1 2006/06/30 15:14:17 eiki Exp $
 * Created on Jun 21, 2006
 *
 * Copyright (C) 2006 Idega Software hf. All Rights Reserved.
 *
 * This software is the proprietary information of Idega hf.
 * Use is subject to license terms.
 */
package com.idega.content.presentation;

import com.idega.content.business.ContentSearch;
import com.idega.core.search.business.SearchPlugin;
import com.idega.core.search.presentation.SearchResults;
import com.idega.presentation.IWContext;

/**
 * A block that displays the latest or all entries in the file repository ordered by creation date<br>
 * It extends SearchResults block and forces it to only use a DASL search (ContentSearch) with specific settings<br>
 * and the query is by default set to "*" and the path to "files" but that can be changed.
 * 
 *  Last modified: $Date: 2006/06/30 15:14:17 $ by $Author: eiki $
 * 
 * @author <a href="mailto:eiki@idega.com">eiki</a>
 * @version $Revision: 1.1.2.1 $
 */
public class WhatIsNew extends SearchResults {
	
	protected String startingPointURI = "files";
	protected String orderByProperty = "creationdate";
	protected boolean useDescendingOrder = true;
	protected int numberOfResultItemsToDisplay = -1;
	protected boolean ignoreFolders = true;
	private boolean useRootAccessForSearch = false;
	
	

	/* (non-Javadoc)
	 * @see com.idega.core.search.presentation.SearchResults#configureSearchPlugin(com.idega.core.search.business.SearchPlugin)
	 */
	protected SearchPlugin configureSearchPlugin(SearchPlugin searchPlugin) {
		
		if(searchPlugin instanceof ContentSearch){
			//Get a copy of the plugin
			ContentSearch contentSearch = (ContentSearch) ((ContentSearch)searchPlugin).clone();
			
			contentSearch.setScopeURI(getStartingPointURI());
			contentSearch.setPropertyToOrderBy(getOrderByProperty());
			contentSearch.setToUseDescendingOrder(isUsingDescendingOrder());
			contentSearch.setNumberOfResultItemsToReturn(getNumberOfResultItemsToDisplay());
			contentSearch.setToIgnoreFolders(isIgnoreFolders());
			contentSearch.setToUseRootAccessForSearch(isUsingRootAccessForSearch());
			return contentSearch;
		}
		else {
			return super.configureSearchPlugin(searchPlugin);
		}
	}

	/* (non-Javadoc)
	 * @see com.idega.core.search.presentation.SearchResults#isAdvancedSearch(com.idega.presentation.IWContext)
	 */
	protected boolean isAdvancedSearch(IWContext iwc) {
		return false;
	}

	/* (non-Javadoc)
	 * @see com.idega.core.search.presentation.SearchResults#getQueryString(com.idega.presentation.IWContext)
	 */
	protected String getSearchQueryString(IWContext iwc) {
		String query = "*";
		if(super.searchQueryString==null){
			return query;
		}
		else{
			return searchQueryString;
		}
	}

	/* (non-Javadoc)
	 * @see com.idega.core.search.presentation.SearchResults#isSimpleSearch(com.idega.presentation.IWContext)
	 */
	protected boolean isSimpleSearch(IWContext iwc) {
		return true;
	}

	/* (non-Javadoc)
	 * @see com.idega.core.search.presentation.SearchResults#getSearchPluginsToUse()
	 */
	public String getSearchPluginsToUse() {
		return "ContentSearch";
	}

	
	/**
	 * @return the ignoreFolders
	 */
	public boolean isIgnoreFolders() {
		return ignoreFolders;
	}

	
	/**
	 * @param ignoreFolders the ignoreFolders to set
	 */
	public void setIgnoreFolders(boolean ignoreFolders) {
		this.ignoreFolders = ignoreFolders;
	}

	
	/**
	 * @return the numberOfResultItemsToDisplay
	 */
	public int getNumberOfResultItemsToDisplay() {
		return numberOfResultItemsToDisplay;
	}

	
	/**
	 * @param numberOfResultItemsToDisplay the numberOfResultItemsToDisplay to set
	 */
	public void setNumberOfResultItemsToDisplay(int numberOfResultItemsToDisplay) {
		this.numberOfResultItemsToDisplay = numberOfResultItemsToDisplay;
	}

	
	/**
	 * @return the orderByProperty
	 */
	public String getOrderByProperty() {
		return orderByProperty;
	}

	
	/**
	 * @param orderByProperty the orderByProperty to set
	 */
	public void setOrderByProperty(String orderByProperty) {
		this.orderByProperty = orderByProperty;
	}

	
	/**
	 * @return the startingPointURI
	 */
	public String getStartingPointURI() {
		if(startingPointURI!=null && startingPointURI.startsWith("/")){
			return startingPointURI.substring(1);
		}
		else return startingPointURI;
	}

	
	/**
	 * @param startingPointURI the startingPointURI to set
	 */
	public void setStartingPointURI(String startingPointURI) {
		this.startingPointURI = startingPointURI;
	}

	
	/**
	 * @return the useDescendingOrder
	 */
	public boolean isUsingDescendingOrder() {
		return useDescendingOrder;
	}

	
	/**
	 * @param useDescendingOrder the useDescendingOrder to set
	 */
	public void setToUseDescendingOrder(boolean useDescendingOrder) {
		this.useDescendingOrder = useDescendingOrder;
	}
	
	
	/**
	 * @return the useRootAccessForSearch
	 */
	public boolean isUsingRootAccessForSearch() {
		return useRootAccessForSearch ;
	}


	
	/**
	 * Set to true if the content search should use the ROOT access for searching.<br>
	 * Does not give the user rights to open files beyond his access though.
	 * @param useRootAccessForSearch
	 */
	public void setToUseRootAccessForSearch(boolean useRootAccessForSearch) {
		this.useRootAccessForSearch = useRootAccessForSearch;
	}
	
	
	
}
