/*
 * Created on 14.12.2004
 */
package com.idega.content.presentation;

import java.io.IOException;
import java.rmi.RemoteException;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.Vector;

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

	WebdavResource resource = null;
	
	protected void initializeContent() {
		this.setId(this.getId());

		HtmlPanelGrid grid = new HtmlPanelGrid();
		grid.setId(this.getId()+"_grid");
		grid.setBorder(2);
		grid.setColumns(2);
		grid.getFacets().put("header", WFUtil.getText("Document Details"));

		if (resource != null) {
			grid.getChildren().add(WFUtil.getText(resource.getName()));
			HtmlOutputLink link = new HtmlOutputLink();
			link.setValue(resource.getPath());
			link.getChildren().add(WFUtil.getText("Download/View"));
			grid.getChildren().add(link);
			try {
				Enumeration enumer = resource.propfindMethod("version-history");
				while (enumer.hasMoreElements()) {
					Object el = enumer.nextElement();
					grid.getChildren().add(WFUtil.getText("version"));
//					WebdavResource r = new WebdavResource();
					grid.getChildren().add(WFUtil.getText(el.toString()));
					//System.out.println("Enumer = "+enumer.nextElement());
				}
 			} catch (HttpException e) {
				e.printStackTrace();
			} catch (IOException e) {
				e.printStackTrace();
			}
			this.getChildren().add(grid);
		} 
		
		
		
		
	}
	
	public void encodeBegin(FacesContext context) throws IOException {
		String webDavPath = (String) this.getAttributes().get("path");
		String path = (String) WFUtil.invoke(webDavPath);
		try {
			IWSlideSession ss = (IWSlideSession) IBOLookup.getSessionInstance(IWContext.getInstance(), IWSlideSession.class);
			WebdavResource oldRes = resource;
			WebdavResource newRes = ss.getWebdavResource(path);
			if (!newRes.isCollection() && (oldRes == null || oldRes.getName().equals(newRes.getName())) ) {
				resource = newRes;
				
				this.setInitialized(false);
				Vector gr = new Vector();
				for (Iterator iter = getChildren().iterator(); iter.hasNext();) {
					UIComponent element = (UIComponent) iter.next();
					if (element instanceof HtmlPanelGrid) {
						gr.add((HtmlPanelGrid) element);
					}
				}
				if (gr != null) {
					getChildren().removeAll(gr);
				}
			} 
			
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
