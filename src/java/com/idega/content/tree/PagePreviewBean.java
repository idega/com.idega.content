package com.idega.content.tree;

import java.rmi.RemoteException;

import com.idega.business.IBOServiceBean;
import com.idega.core.builder.business.BuilderService;
import com.idega.core.builder.business.BuilderServiceFactory;

public class PagePreviewBean extends IBOServiceBean implements PagePreview{
	
	public String getPreviewUrl(String ID){
//	public String getPreviewUrl(){
		System.out.println("getURL id = "+ ID);
		//return "http://localhost:8080/pages";
		String uri=null;
		BuilderService builderService;
		try {
			builderService = BuilderServiceFactory.getBuilderService(getIWApplicationContext());
			uri = builderService.getPageURI(ID);
		} catch (RemoteException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		System.out.println("URI = "+ uri);
		return uri;
	}
}