package com.idega.content.tree;

import com.idega.business.IBOHomeImpl;

public class PagePreviewHomeImpl extends IBOHomeImpl {
	
	protected Class getBeanInterfaceClass() {
		return PagePreview.class;
	}

	public PagePreview create() throws javax.ejb.CreateException {
		return (PagePreview) super.createIBO();
	}

}
