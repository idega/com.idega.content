package com.idega.content.themes.business;


import com.idega.core.builder.business.BuilderService;
import com.idega.core.file.data.ICFileHome;
import com.idega.business.IBOService;
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
	 * @see com.idega.content.themes.business.ThemesServiceBean#getICPageHome
	 */
	public ICPageHome getICPageHome() throws RemoteException, RemoteException;

	/**
	 * @see com.idega.content.themes.business.ThemesServiceBean#getICFileHome
	 */
	public ICFileHome getICFileHome() throws RemoteException, RemoteException;

	/**
	 * @see com.idega.content.themes.business.ThemesServiceBean#getBuilderService
	 */
	public BuilderService getBuilderService() throws RemoteException;
}