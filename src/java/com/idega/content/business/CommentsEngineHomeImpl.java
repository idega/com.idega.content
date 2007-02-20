package com.idega.content.business;


import javax.ejb.CreateException;
import com.idega.business.IBOHomeImpl;

public class CommentsEngineHomeImpl extends IBOHomeImpl implements CommentsEngineHome {
	
	private static final long serialVersionUID = -6942588896879841078L;

	public Class getBeanInterfaceClass() {
		return CommentsEngine.class;
	}

	public CommentsEngine create() throws CreateException {
		return (CommentsEngine) super.createIBO();
	}
}