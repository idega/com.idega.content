package com.idega.content.themes.business;


import com.idega.core.builder.data.ICDomain;
import com.idega.core.builder.business.BuilderService;
import com.idega.core.builder.data.ICPage;
import com.idega.business.IBOService;
import com.idega.content.themes.helpers.Theme;
import com.idega.slide.business.IWContentEvent;
import com.idega.slide.business.IWSlideChangeListener;
import com.idega.core.builder.data.ICPageHome;
import java.rmi.RemoteException;

public interface ThemesService extends IBOService, IWSlideChangeListener {
	/**
	 * @see com.idega.content.themes.business.ThemesServiceBean#onSlideChange
	 */
	public void onSlideChange(IWContentEvent idegaWebContentEvent);

	/**
	 * @see com.idega.content.themes.business.ThemesServiceBean#deleteIBPage
	 */
	public boolean deleteIBPage(String pageID, boolean deleteChildren) throws RemoteException;

	/**
	 * @see com.idega.content.themes.business.ThemesServiceBean#createIBPage
	 */
	public boolean createIBPage(Theme theme) throws RemoteException;

	/**
	 * @see com.idega.content.themes.business.ThemesServiceBean#updatePageWebDav
	 */
	public boolean updatePageWebDav(int id, String uri);

	/**
	 * @see com.idega.content.themes.business.ThemesServiceBean#createIBPage
	 */
	public int createIBPage(String parentId, String name, String type, String templateId, String pageUri, String subType, int domainId, String format, String sourceMarkup) throws RemoteException;

	/**
	 * @see com.idega.content.themes.business.ThemesServiceBean#createIBPage
	 */
	public int createIBPage(String parentId, String name, String type, String templateId, String pageUri, String subType, int domainId, String format, String sourceMarkup, String treeOrder) throws RemoteException;

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
}