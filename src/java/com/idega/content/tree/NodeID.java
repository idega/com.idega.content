package com.idega.content.tree;

import java.util.Vector;

public class NodeID {
	private Vector IDsArray = new Vector();
	
	public String getIDinDB(String IDinTree){
		NodeIDPair IDPair;
		String result = "";
		for(int i = 0; i < IDsArray.size(); i++){
			IDPair = (NodeIDPair)IDsArray.get(i);
			if (IDPair.getIDinTree().equals(IDinTree)){
				result = IDPair.getIDinDB();
				break;
			}				
		}		
		return result;		
	}

	public void setIDinDB(String IDinTree, String value){
		NodeIDPair IDPair;
		for(int i = 0; i < IDsArray.size(); i++){
			IDPair = (NodeIDPair)IDsArray.get(i);			
			if (IDPair.getIDinTree().equals(IDinTree)){
				IDPair.setIDinDB(value);
				IDsArray.setElementAt((Object)IDPair, i);
			}
				
		}
	}

	public String getIDinTree(String IDinDB){
		NodeIDPair IDPair;
		String result = "";
		for(int i = 0; i < IDsArray.size(); i++){
			IDPair = (NodeIDPair)IDsArray.get(i);
			if (IDPair.getIDinDB().equals(IDinDB)){
				result = IDPair.getIDinTree();
				break;
			}				
		}		
		return result;		
	}

	public void setIDinTree(String IDinDB, String value){
		NodeIDPair IDPair;
		for(int i = 0; i < IDsArray.size(); i++){
			IDPair = (NodeIDPair)IDsArray.get(i);			
			if (IDPair.getIDinDB().equals(IDinDB)){
				IDPair.setIDinTree(value);
				IDsArray.setElementAt((Object)IDPair, i);
			}				
		}
	}

	public void addID(String IDinDB, String IDinTree){
		NodeIDPair PairOfIDs = new NodeIDPair(IDinDB, IDinTree);
		IDsArray.add(PairOfIDs);
	}
	
	public int countPairs(){
		return IDsArray.size();
	}
	
	public String getIDinTree(int index){
		NodeIDPair pair = (NodeIDPair)IDsArray.get(index);
		return pair.getIDinTree(); 
	}

	public String getIDinDB(int index){
		NodeIDPair pair = (NodeIDPair)IDsArray.get(index);
		return pair.getIDinDB(); 
	}

}
class NodeIDPair {
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