package com.idega.content.themes.business;


import javax.ejb.CreateException;
import com.idega.business.IBOHome;
import java.rmi.RemoteException;

public interface ThemesPreviewsProviderHome extends IBOHome {
	public ThemesPreviewsProvider create() throws CreateException, RemoteException;
}