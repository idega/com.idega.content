package com.idega.content.tree;

import com.idega.business.IBOHome;

public interface NodeIDHome extends IBOHome {
	
	public NodeIDHome create() throws javax.ejb.CreateException, java.rmi.RemoteException;

}
