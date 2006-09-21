package com.idega.content.business;

import com.idega.business.IBOHomeImpl;

public class ThemesPreviewsProviderHomeImpl extends IBOHomeImpl {
	
	protected Class getBeanInterfaceClass() {
		return ThemesPreviewsProvider.class;
	}

	public ThemesPreviewsProvider create() throws javax.ejb.CreateException {
		return (ThemesPreviewsProvider) super.createIBO();
	}

}
