/*
 * $Id: WhatIsNew.java,v 1.9 2008/12/11 08:03:58 laddi Exp $
 * Created on Jun 21, 2006
 *
 * Copyright (C) 2006 Idega Software hf. All Rights Reserved.
 *
 * This software is the proprietary information of Idega hf.
 * Use is subject to license terms.
 */
package com.idega.content.presentation;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.faces.component.UIComponent;

import com.idega.block.web2.business.Web2Business;
import com.idega.block.web2.presentation.Accordion;
import com.idega.content.business.ContentSearch;
import com.idega.content.business.IWCacheInvalidatorIWSlideListener;
import com.idega.core.builder.data.ICPage;
import com.idega.core.search.business.SearchPlugin;
import com.idega.core.search.presentation.SearchResults;
import com.idega.idegaweb.IWBundle;
import com.idega.presentation.IWContext;
import com.idega.presentation.Layer;
import com.idega.presentation.text.Text;
import com.idega.util.CoreConstants;
import com.idega.util.PresentationUtil;
import com.idega.util.expression.ELUtil;

/**
 * A block that displays the latest or all entries in the file repository ordered by modification date<br>
 * It extends SearchResults block and forces it to only use a DASL search (ContentSearch) with specific settings<br>
 * and the query is by default set to "*" and the path to "files" but that can be changed.
 *
 *  Last modified: $Date: 2008/12/11 08:03:58 $ by $Author: laddi $
 *
 * @author <a href="mailto:eiki@idega.com">eiki</a>
 * @version $Revision: 1.9 $
 */
public class WhatIsNew extends SearchResults {

	public static final String STYLE_CLASS_WHATISNEW = "whatisnew";
	public static final String WHAT_IS_NEW_CACHE_KEY = "iw_whatisnew";

	protected String startingPointURI = "files/public";
	protected String orderByProperty = "getlastmodified";
	protected String groupHeight = null;

	protected int numberOfResultItemsToDisplay = -1;

	protected boolean useDescendingOrder = true;
	protected boolean ignoreFolders = true;
	protected boolean useRootAccessForSearch = false;
	protected boolean hideParentFolderPath = false;
	protected boolean hideFileExtension = false;
	protected boolean useUserHomeFolder = false;
	protected boolean showDeleteLink = false;
	protected boolean groupByExtraInfo = false;
	protected boolean orderGroups = true;

	protected ICPage deletePage = null;

	public WhatIsNew(){
		super();
		this.setCacheable(WHAT_IS_NEW_CACHE_KEY, 0);
		this.setStyleClass(STYLE_CLASS_WHATISNEW);
		this.setContainerID(STYLE_CLASS_WHATISNEW);
		this.setOpenLinksInAnotherWindow(true);
	}

	/*
	 * (non-Javadoc)
	 *
	 * @see com.idega.presentation.PresentationObject#main(com.idega.presentation.IWContext)
	 */
	@Override
	public void main(IWContext iwc) throws Exception {
		//just listen for changes
		startCachingStrategy();

		boolean useGlobalSettings = iwc.getApplicationSettings().getBoolean("whatisnew.use.global.settings", false);
		if (useGlobalSettings) {
			boolean useDescending = iwc.getApplicationSettings().getBoolean("whatisnew.use.descending", true);
			setToUseDescendingOrder(useDescending);

			String orderByProperty = iwc.getApplicationSettings().getProperty("whatisnew.order.property", "getlastmodified");
			setOrderByProperty(orderByProperty);
		}


		super.main(iwc);
	}

	protected void startCachingStrategy() {
		getRepositoryService().addRepositoryChangeListeners(new IWCacheInvalidatorIWSlideListener(getStartingPointURI(), WHAT_IS_NEW_CACHE_KEY));
	}

	@Override
	protected String getCacheState(IWContext iwc, String cacheStatePrefix) {
		IWBundle iwb = IWContext.getInstance().getIWMainApplication().getCoreBundle();
		Web2Business business = ELUtil.getInstance().getBean(Web2Business.class);

		List<String> actions = new ArrayList<String>();
		actions.add(business.getActionToLinkLinksWithFiles(STYLE_CLASS_WHATISNEW, true, false));

		PresentationUtil.addJavaScriptSourceLineToHeader(iwc, business.getBundleUriToLinkLinksWithFilesScriptFile());
		PresentationUtil.addStyleSheetsToHeader(iwc, Arrays.asList(
				iwb.getVirtualPathWithFileNameString("style/search.css"),
				business.getBundleUriToLinkLinksWithFilesStyleFile()
		));
		PresentationUtil.addJavaScriptActionsToBody(iwc, actions);

		StringBuffer buffer = new StringBuffer(cacheStatePrefix);
		buffer.append(getSearchQueryString(iwc))
		.append(getStartingPointURI())
		.append(getNumberOfResultItemsToDisplay())
		.append(getOrderByProperty())
		.append(isSetToHideFileExtension())
		.append(isSetToHideParentFolderPath())
		.append(isSetToShowAllResultProperties())
		.append(isSetToShowDeleteLink())
		.append(isUsingDescendingOrder())
		.append(isUsingRootAccessForSearch())
		.append(getAbstractTextStyleClass())
		.append(getExtraAttributeTextEvenStyleClass())
		.append(getExtraAttributeTextOddStyleClass())
		.append(getExtraInformationTextStyleClass())
		.append(getSearchNameStyleClass())
		.append(getRowEvenStyleClass())
		.append(getRowOddStyleClass());

		//hope I got them all!

		return buffer.toString();
	}

	/* (non-Javadoc)
	 * @see com.idega.core.search.presentation.SearchResults#configureSearchPlugin(com.idega.core.search.business.SearchPlugin)
	 */
	@Override
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
			contentSearch.setToHideParentFolderPath(isSetToHideParentFolderPath());
			contentSearch.setToHideFileExtensions(isSetToHideFileExtension());
			contentSearch.setToShowDeleteLink(showDeleteLink);
			contentSearch.setDeletePage(deletePage);
			return contentSearch;
		}
		else {
			return super.configureSearchPlugin(searchPlugin);
		}
	}

	private Map<String, List<UIComponent>> groups = null;

	@Override
	protected void addResultRow(Layer container, Layer rowContainer, String rowKey) {
		if (groupByExtraInfo) {
			if (rowKey.endsWith(CoreConstants.SLASH)) {
				rowKey = rowKey.substring(0, rowKey.length()-1);
			}
			rowKey = rowKey.substring(rowKey.lastIndexOf(CoreConstants.SLASH)+1);
			if (!groups.containsKey(rowKey)) {
				groups.put(rowKey, new ArrayList<UIComponent>());
			}

			groups.get(rowKey).add(rowContainer);
		} else {
			super.addResultRow(container, rowContainer, rowKey);
		}
	}

	@Override
	protected void beforeAddingResultRows(Layer container) {
		// CREATE THE MAP FOR THE CONTAINER
		if (groupByExtraInfo) {
			groups = new HashMap<String, List<UIComponent>>();
		}

	}
	@Override
	protected void afterAddingResultRows(Layer container) {
		if (groupByExtraInfo) {
			Set<String> keySet = groups.keySet();
			Accordion acc = new Accordion(getId()+"_acc");
			if (groupHeight != null) {
				acc.setHeight(groupHeight);
			}
			container.add(acc);

			List<String> keyList = new ArrayList<String>(keySet);
			if (orderGroups) {
				// sorting the keys.
				Collections.sort(keyList);
			}

			for (String key: keyList) {
				List<UIComponent> v = groups.get(key);
				if (v != null) {
					Layer la = new Layer();

					for (UIComponent component: v) {
						la.add(component);
					}
					acc.addPanel(new Text(key), la);
				}
			}
		}
	}

	/* (non-Javadoc)
	 * @see com.idega.core.search.presentation.SearchResults#isAdvancedSearch(com.idega.presentation.IWContext)
	 */
	@Override
	protected boolean isAdvancedSearch(IWContext iwc) {
		return false;
	}

	/* (non-Javadoc)
	 * @see com.idega.core.search.presentation.SearchResults#getQueryString(com.idega.presentation.IWContext)
	 */
	@Override
	protected String getSearchQueryString(IWContext iwc) {
		String query = isIgnoreFolders() ? "*.*" : "*";
		if(super.searchQueryString==null){
			return query;
		}
		else{
			return this.searchQueryString;
		}
	}

	/* (non-Javadoc)
	 * @see com.idega.core.search.presentation.SearchResults#isSimpleSearch(com.idega.presentation.IWContext)
	 */
	@Override
	protected boolean isSimpleSearch(IWContext iwc) {
		return true;
	}

	/* (non-Javadoc)
	 * @see com.idega.core.search.presentation.SearchResults#getSearchPluginsToUse()
	 */
	@Override
	public String getSearchPluginsToUse() {
		return "ContentSearch";
	}


	/**
	 * @return the ignoreFolders
	 */
	public boolean isIgnoreFolders() {
		return this.ignoreFolders;
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
		return this.numberOfResultItemsToDisplay;
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
		return this.orderByProperty;
	}


	/**
	 * @param orderByProperty the orderByProperty to set
	 */
	public void setOrderByProperty(String orderByProperty) {
		this.orderByProperty = orderByProperty;
	}


	/**
	 * Returns the StartingPointURI, if block is set to use User home folder, that is returned instead
	 * @return the startingPointURI
	 */
	public String getStartingPointURI() {
		if (useUserHomeFolder) {
			try {
				String homeFolder = getRepositorySession().getUserHomeFolder();
				if (homeFolder != null) {
					if (homeFolder.indexOf("/") == 0) {
						homeFolder = homeFolder.substring(1);
					}
					return homeFolder;
				} else {
					useUserHomeFolder = false;
					return getStartingPointURI();
				}
			} catch (Exception e) {
				useUserHomeFolder = false;
				return getStartingPointURI();
			}
		} else {
			if (this.startingPointURI!=null && this.startingPointURI.startsWith("/")) {
				return this.startingPointURI.substring(1);
			}
			else return this.startingPointURI;
		}
	}


	/**
	 * @param startingPointURI the startingPointURI to set
	 */
	public void setStartingPointURI(String startingPointURI) {
		this.startingPointURI = startingPointURI;
	}

	/**
	 * Sets the viewer to view the current users home folder content. Overrides
	 * the setStartingPointURI method
	 * @param useUserHomeFolder
	 */
	public void setUseUserHomeFolder(boolean useUserHomeFolder) {
		this.useUserHomeFolder = useUserHomeFolder;
	}
	/**
	 * @return the useDescendingOrder
	 */
	public boolean isUsingDescendingOrder() {
		return this.useDescendingOrder;
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
		return this.useRootAccessForSearch ;
	}



	/**
	 * Set to true if the content search should use the ROOT access for searching.<br>
	 * Does not give the user rights to open files beyond his access though.
	 * @param useRootAccessForSearch
	 */
	public void setToUseRootAccessForSearch(boolean useRootAccessForSearch) {
		this.useRootAccessForSearch = useRootAccessForSearch;
	}


	/**
	 * @return the hideFolderPath
	 */
	public boolean isSetToHideParentFolderPath() {
		return this.hideParentFolderPath;
	}


	/**
	 * If set to true the result will only state the parent folder of the result itm and not the full path
	 * @param hideParentFolderPath
	 */
	public void setToHideParentFolderPath(boolean hideParentFolderPath) {
		this.hideParentFolderPath = hideParentFolderPath;
	}

	public boolean isSetToHideFileExtension() {
		return this.hideFileExtension;
	}

	public void setToHideFileExtension(boolean hideFileExtension) {
		this.hideFileExtension = hideFileExtension;
	}

	public void setToShowDeleteLink(boolean show) {
		this.showDeleteLink = show;
	}

	public boolean isSetToShowDeleteLink() {
		return showDeleteLink;
	}

	public ICPage getDeletePage() {
		return deletePage;
	}

	public void setDeletePage(ICPage deletePage) {
		this.deletePage = deletePage;
	}

	public boolean isGroupByExtraInfo() {
		return groupByExtraInfo;
	}

	public void setGroupByExtraInfo(boolean groupByExtraInfo) {
		this.groupByExtraInfo = groupByExtraInfo;
	}

	public void setGroupHeight(String height) {
		this.groupHeight = height;
	}

	public void setOrderGroups(boolean orderGroups) {
		this.orderGroups = orderGroups;
	}

}