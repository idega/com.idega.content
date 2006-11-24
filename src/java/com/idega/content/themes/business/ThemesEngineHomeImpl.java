package com.idega.content.themes.business;


import javax.ejb.CreateException;
import com.idega.business.IBOHomeImpl;

public class ThemesEngineHomeImpl extends IBOHomeImpl implements ThemesEngineHome {
	public Class getBeanInterfaceClass() {
		return ThemesEngine.class;
	}

	public ThemesEngine create() throws CreateException {
		return (ThemesEngine) super.createIBO();
	}
}