package com.idega.content.themes.business;


import com.idega.core.builder.business.BuilderService;
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
	public boolean deleteIBPage(Theme theme) throws RemoteException;

	/**
	 * @see com.idega.content.themes.business.ThemesServiceBean#createIBPage
	 */
	public boolean createIBPage(Theme theme) throws RemoteException;

	/**
	 * @see com.idega.content.themes.business.ThemesServiceBean#getICPageHome
	 */
	public ICPageHome getICPageHome() throws RemoteException, RemoteException;

	/**
	 * @see com.idega.content.themes.business.ThemesServiceBean#getBuilderService
	 */
	public BuilderService getBuilderService() throws RemoteException;
}