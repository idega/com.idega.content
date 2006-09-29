package com.idega.content.tree;

public class NodeIDPair {
	private String IDinDB;
	private String IDinTree;
	
	NodeIDPair(String IDinDB, String IDinTree){
		this.IDinDB = IDinDB;
		this.IDinTree = IDinTree;
	}

	public void setIDinDB(String value){		
		IDinDB = value;
	}

	public String getIDinDB(){		
		return IDinDB;
	}

	public void setIDinTree(String value){		
		IDinTree = value;
	}

	public String getIDinTree(){		
		return IDinTree;
	}
}