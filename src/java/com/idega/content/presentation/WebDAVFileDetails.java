/*
 * Created on 14.12.2004
 */
package com.idega.content.presentation;

import java.io.IOException;
import java.rmi.RemoteException;
import java.util.Iterator;

import javax.faces.component.UIComponent;
import javax.faces.component.html.HtmlOutputLink;
import javax.faces.component.html.HtmlPanelGrid;
import javax.faces.context.FacesContext;

import org.apache.commons.httpclient.HttpException;
import org.apache.webdav.lib.WebdavResource;

import com.idega.business.IBOLookup;
import com.idega.business.IBOLookupException;
import com.idega.idegaweb.UnavailableIWContext;
import com.idega.presentation.IWBaseComponent;
import com.idega.presentation.IWContext;
import com.idega.slide.business.IWSlideSession;
import com.idega.webface.WFUtil;

/**
 * @author gimmi
 */
public class WebDAVFileDetails extends IWBaseComponent {

	private HtmlPanelGrid grid = new HtmlPanelGrid();
	private boolean fileFound = false;
	WebdavResource resource = null;
	
	protected void initializeContent() {
		this.setId(this.getId());

		grid.setId(this.getId()+"_grid");
		grid.setBorder(2);
		grid.setColumns(4);
		
		if (fileFound) {
			grid.getChildren().add(WFUtil.getText(resource.getName()));
			
			HtmlOutputLink link = new HtmlOutputLink();
			link.setValue(resource.getHttpURL());
			link.getChildren().add(WFUtil.getText("Get file"));
			grid.getChildren().add(link);
			
			//boolean doesExist = getChildren().contains(grid);
			for (Iterator iter = getChildren().iterator(); iter.hasNext();) {
				UIComponent comp2 = (UIComponent) iter.next();
				System.out.println("Parent = "+comp2.toString());
			}
			this.add(grid);
			
			System.out.println("ADDING FILE !!!!!!!");
		}
//		boolean vbPath = false;
//		vbPath = (webDavPath.indexOf("#{") > -1);
//		
//		if (vbPath) {
//			HtmlOutputText text = WFUtil.getTextVB(webDavPath);
//			table.getChildren().add(text);
//		} else {

		//addColumn(text);
		//this.getChildren().add(table);
//		this.add(text);
	}
	
	public void encodeBegin(FacesContext context) throws IOException {
		String webDavPath = (String) this.getAttributes().get("path");
		String path = (String) WFUtil.invoke(webDavPath);
		try {
			IWSlideSession ss = (IWSlideSession) IBOLookup.getSessionInstance(IWContext.getInstance(), IWSlideSession.class);
			resource = ss.getWebdavResource(path);
			fileFound = !resource.isCollection();
			if (fileFound) {
				initializeContent();
			}
//			if (getParent().getChildren().contains(this)) {
//				getParent().getChildren().remove(this);
//			}
		} catch (IBOLookupException e) {
			e.printStackTrace();
		} catch (UnavailableIWContext e) {
			e.printStackTrace();
		} catch (HttpException e) {
			e.printStackTrace();
		} catch (RemoteException e) {
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		}
		super.encodeBegin(context);
	}
	
	public void encodeEnd(FacesContext context) throws IOException{
		getChildren().remove(grid);
	}
	
	public void encodeChildren(FacesContext context) throws IOException{
		super.encodeChildren(context);
		for (Iterator iter = getChildren().iterator(); iter.hasNext();) {
			UIComponent child = (UIComponent) iter.next();
			renderChild(context,child);
		}
	}
		
	public boolean getRendersChildren() {
		return true;
	}

}
