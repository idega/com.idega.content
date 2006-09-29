package com.idega.content.tree;

import com.idega.business.IBOHomeImpl;

public class NodeIDHomeImpl extends IBOHomeImpl {
	
	protected Class getBeanInterfaceClass() {
		return NodeID.class;
	}

	public NodeID create() throws javax.ejb.CreateException {
		return (NodeID) super.createIBO();
	}

}
