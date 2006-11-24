package com.idega.content.themes.business;


import javax.ejb.CreateException;
import com.idega.business.IBOHomeImpl;

public class ThemesServiceHomeImpl extends IBOHomeImpl implements ThemesServiceHome {
	public Class getBeanInterfaceClass() {
		return ThemesService.class;
	}

	public ThemesService create() throws CreateException {
		return (ThemesService) super.createIBO();
	}
}