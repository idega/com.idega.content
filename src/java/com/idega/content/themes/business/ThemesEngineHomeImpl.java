package com.idega.content.themes.business;


import javax.ejb.CreateException;
import com.idega.business.IBOHomeImpl;

public class ThemesEngineHomeImpl extends IBOHomeImpl implements ThemesEngineHome {

	private static final long serialVersionUID = 7141978995132493723L;

	public Class<ThemesEngine> getBeanInterfaceClass() {
		return ThemesEngine.class;
	}

	public ThemesEngine create() throws CreateException {
		return (ThemesEngine) super.createIBO();
	}
}