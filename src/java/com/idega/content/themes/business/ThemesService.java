package com.idega.content.themes.business;


import java.rmi.RemoteException;

import javax.jcr.observation.Event;

import com.idega.business.IBOService;
import com.idega.content.themes.helpers.bean.Theme;
import com.idega.core.builder.business.BuilderService;
import com.idega.core.builder.data.ICDomain;
import com.idega.core.builder.data.ICPage;
import com.idega.core.builder.data.ICPageHome;
import com.idega.repository.event.RepositoryEventListener;

public interface ThemesService extends IBOService, RepositoryEventListener {
	/**
	 * @see com.idega.content.themes.business.ThemesServiceBean#onSlideChange
	 */
	public void onSlideChange(Event idegaWebContentEvent);

	/**
	 * @see com.idega.content.themes.business.ThemesServiceBean#deleteIBPage
	 */
	public boolean deleteIBPage(String pageID, boolean deleteChildren, boolean markPagesForDeletingArticles) throws RemoteException;

	/**
	 * @see com.idega.content.themes.business.ThemesServiceBean#createIBPage
	 */
	public boolean createBuilderTemplate(Theme theme) throws RemoteException;

	/**
	 * @see com.idega.content.themes.business.ThemesServiceBean#updatePageWebDav
	 */
	public boolean updatePageWebDav(int id, String uri);

	/**
	 * @see ThemesServiceBean#updatePageWebDav(int, String, boolean)
	 */
	public boolean updatePageWebDav(int id, String uri, boolean clearCache);

	/**
	 * @see com.idega.content.themes.business.ThemesServiceBean#createIBPage
	 */
	public int createIBPage(String parentId, String name, String type, String templateId, String pageUri, String subType, int domainId, String format,
			String sourceMarkup) throws RemoteException;

	/**
	 * @see com.idega.content.themes.business.ThemesServiceBean#createIBPage
	 */
	public int createIBPage(String parentId, String name, String type, String templateId, String pageUri, String subType, int domainId, String format,
			String sourceMarkup, String treeOrder) throws RemoteException;

	/**
	 * @see com.idega.content.themes.business.ThemesServiceBean#getICPageHome
	 */
	public ICPageHome getICPageHome() throws RemoteException, RemoteException;

	/**
	 * @see com.idega.content.themes.business.ThemesServiceBean#getBuilderService
	 */
	public BuilderService getBuilderService();

	/**
	 * @see com.idega.content.themes.business.ThemesServiceBean#getICPage
	 */
	public ICPage getICPage(int id);

	/**
	 * @see com.idega.content.themes.business.ThemesServiceBean#getDomain
	 */
	public ICDomain getDomain();

	/**
	 * @see com.idega.content.themes.business.ThemesServiceBean#getICPage
	 */
	public ICPage getICPage(String pageKey);

	public String createChildTemplateForThisTemplate(String parentTemplateKey);
}