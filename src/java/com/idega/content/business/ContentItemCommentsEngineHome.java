package com.idega.content.business;


import javax.ejb.CreateException;
import com.idega.business.IBOHome;
import java.rmi.RemoteException;

public interface ContentItemCommentsEngineHome extends IBOHome {
	public ContentItemCommentsEngine create() throws CreateException, RemoteException;
}