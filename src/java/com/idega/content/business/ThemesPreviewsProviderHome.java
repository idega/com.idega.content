package com.idega.content.business;

import com.idega.business.IBOHome;

public interface ThemesPreviewsProviderHome extends IBOHome {
	
	public ThemesPreviewsProvider create() throws javax.ejb.CreateException, java.rmi.RemoteException;

}
