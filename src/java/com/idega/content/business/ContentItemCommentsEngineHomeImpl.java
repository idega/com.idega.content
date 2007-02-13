package com.idega.content.business;


import javax.ejb.CreateException;
import com.idega.business.IBOHomeImpl;

public class ContentItemCommentsEngineHomeImpl extends IBOHomeImpl implements ContentItemCommentsEngineHome {
	public Class getBeanInterfaceClass() {
		return ContentItemCommentsEngine.class;
	}

	public ContentItemCommentsEngine create() throws CreateException {
		return (ContentItemCommentsEngine) super.createIBO();
	}
}