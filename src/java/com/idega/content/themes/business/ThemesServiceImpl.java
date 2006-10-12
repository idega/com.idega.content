package com.idega.content.themes.business;

import java.rmi.RemoteException;

import javax.ejb.CreateException;

import com.idega.business.IBOHomeImpl;

public class ThemesServiceImpl extends IBOHomeImpl implements ThemesServiceHome {

	public Class getBeanInterfaceClass() {
		return ThemesService.class;
	}

	public ThemesService create() throws CreateException, RemoteException {
		return (ThemesService) super.createIBO();
	}

}
