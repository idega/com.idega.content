package com.idega.content.tree;

import java.util.Vector;

import com.idega.business.IBOService;

public interface NodeID extends IBOService{
	
	public String getIDinDB(String IDinTree);

	public void setIDinDB(String IDinTree, String value);

	public String getIDinTree(String IDinDB);

	public void setIDinTree(String IDinDB, String value);

	public void addID(String IDinDB, String IDinTree);
	
	public int countPairs();
	
	public String getIDinTree(int index);

	public String getIDinDB(int index);
	
	public Vector OldIDsToJavaScript();

}
