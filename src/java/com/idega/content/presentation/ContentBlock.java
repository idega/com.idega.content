package com.idega.content.presentation;

import java.io.IOException;
import java.rmi.RemoteException;
import java.util.Iterator;
import java.util.Vector;

import javax.faces.component.UIComponent;
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
public abstract class ContentBlock extends IWBaseComponent {

	private WebdavResource resource = null;
	
	protected abstract void initializeContent();
	
	
	protected WebdavResource getWebdavResource() {
		return resource;
	}
	
	public void encodeBegin(FacesContext context) throws IOException {
		String webDavPath = (String) this.getAttributes().get("path");
		String path = null;
		
		if (webDavPath == null) {
			path = (String) WFUtil.createMethodBinding("#{WebDAVListBean.getClickedFilePath}", null).invoke(context, null);
		} else {
			path = (String) WFUtil.invoke(webDavPath);
		}
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
					//if (element instanceof HtmlPanelGrid) {
						gr.add(element);
					//}
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
