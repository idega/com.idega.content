package com.idega.content.themes.business;


import javax.ejb.CreateException;
import com.idega.business.IBOHomeImpl;

public class ThemesPreviewsProviderHomeImpl extends IBOHomeImpl implements ThemesPreviewsProviderHome {

	private static final long serialVersionUID = -3687439882380414715L;

	public Class getBeanInterfaceClass() {
		return ThemesPreviewsProvider.class;
	}

	public ThemesPreviewsProvider create() throws CreateException {
		return (ThemesPreviewsProvider) super.createIBO();
	}
}