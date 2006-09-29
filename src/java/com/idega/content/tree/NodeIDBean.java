package com.idega.content.tree;

import java.util.Vector;

import com.idega.business.IBOServiceBean;

public class NodeIDBean extends IBOServiceBean implements NodeID{
	
	private static final long serialVersionUID = 2466045828687095430L;
	private Vector <NodeIDPair> IDsArray = new Vector <NodeIDPair> ();
	
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
				IDsArray.setElementAt(IDPair, i);
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
				IDsArray.setElementAt(IDPair, i);
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
	
	public Vector OldIDsToJavaScript(){
//		System.out.println("==> oldIdsToJavaScript");
		return IDsArray;
	}

}