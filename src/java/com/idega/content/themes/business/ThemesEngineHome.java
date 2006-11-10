package com.idega.content.themes.business;


import javax.ejb.CreateException;
import com.idega.business.IBOHome;
import java.rmi.RemoteException;

public interface ThemesEngineHome extends IBOHome {
	public ThemesEngine create() throws CreateException, RemoteException;
}