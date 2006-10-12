package com.idega.content.themes.business;


import javax.ejb.CreateException;
import com.idega.business.IBOHome;
import java.rmi.RemoteException;

public interface ThemesServiceHome extends IBOHome {
	public ThemesService create() throws CreateException, RemoteException;
}