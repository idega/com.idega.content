package com.idega.content.presentation;

import java.io.IOException;
import java.rmi.RemoteException;
import java.util.Iterator;

import javax.faces.component.UIComponent;
import javax.faces.component.html.HtmlOutputText;
import javax.faces.context.FacesContext;
import javax.faces.el.ValueBinding;

import org.apache.commons.httpclient.HttpException;

import com.idega.business.IBOLookup;
import com.idega.business.IBOLookupException;
import com.idega.content.business.ContentUtil;
import com.idega.idegaweb.IWBundle;
import com.idega.idegaweb.UnavailableIWContext;
import com.idega.presentation.IWBaseComponent;
import com.idega.presentation.IWContext;
import com.idega.slide.business.IWSlideSession;
import com.idega.slide.util.WebdavExtendedResource;
import com.idega.webface.WFUtil;

/**
 * @author gimmi
 */
public abstract class ContentBlock extends IWBaseComponent {

	private WebdavExtendedResource resource = null;

	private IWSlideSession slideSession;
	
	// this, parentContentViewer, should not be saved in save state
	private ContentViewer parentContentViewer = null;

	protected String currentResourcePath = null;

	@Override
	protected abstract void initializeComponent(FacesContext context);

	public static IWBundle getBundle() {
		return ContentUtil.getBundle();
	}

	protected WebdavExtendedResource getWebdavExtendedResource() {
		return this.resource;
	}
		
	protected boolean useFolders() {
		return false;
	}

	public void refreshList() {
		WFUtil.invoke(WebDAVList.WEB_DAV_LIST_BEAN_ID, "refresh", this, UIComponent.class);
	}

	public WebdavExtendedResource getWebdavExentededResource(String path) {
		try {
			IWSlideSession ss = (IWSlideSession) IBOLookup.getSessionInstance(IWContext.getInstance(),IWSlideSession.class);
			return ss.getWebdavResource(path.replaceFirst(ss.getWebdavServerURI(), ""));
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
		return null;
	}

	protected IWSlideSession getIWSlideSession() {
		if (this.slideSession == null) {
			try {
				this.slideSession = (IWSlideSession) IBOLookup.getSessionInstance(IWContext.getInstance(),IWSlideSession.class);
			}
			catch (IBOLookupException e) {
				e.printStackTrace();
			}
			catch (UnavailableIWContext e) {
				e.printStackTrace();
			}
		}
		return this.slideSession;
	}
	
	protected boolean removeClickedFile(WebdavExtendedResource resource) {
		try {
			String parentPath = null;
			if (resource != null) {
				parentPath = resource.getParentPath().replaceFirst(getIWSlideSession().getWebdavServerURI(), "");
			}
			WFUtil.invoke(WebDAVList.WEB_DAV_LIST_BEAN_ID, "setWebDAVPath", parentPath, String.class);
			WFUtil.invoke(WebDAVList.WEB_DAV_LIST_BEAN_ID, "setClickedFilePath", null, String.class);
			return true;
		} catch (Exception e) {
			e.printStackTrace();
			return false;
		}
	}
	
	/**
	 * @return
	 */
	public String getCurrentResourcePath() {
		
		if (this.currentResourcePath != null) {
			return this.currentResourcePath;
		}
		ValueBinding vb = getValueBinding("currentResourcePath");
		String returner = vb != null ? (String)vb.getValue(getFacesContext()) : null;
		if(returner != null) {
			return returner;
		}
		
		ContentViewer v = getContentViewer();
		if(v!=null){
			String tmp = v.getCurrentResourcePath(); 
			return tmp;
		}
		return null;
	}
	
	public ContentViewer getContentViewer(){
		if(this.parentContentViewer == null){
			UIComponent tmp = this;
			ContentViewer v = null;
			while ( tmp != null && v == null) {
				if (tmp instanceof ContentViewer) {
					v = (ContentViewer) tmp;
				}
				else {
					tmp = tmp.getParent();
				}
			}
			this.parentContentViewer = v;
		}
		return this.parentContentViewer;
	}
	
	@Override
	public void encodeBegin(FacesContext context) throws IOException {
		String webDavPath = (String) this.getAttributes().get("path");
		String path = null;
		if (webDavPath == null) {
//			path = (String) WFUtil.createMethodBinding("#{WebDAVListBean.getClickedFilePath}", null).invoke(context,null);
//			if (path == null) {
//				path = (String) WFUtil.createMethodBinding("#{WebDAVListBean.getWebDAVPath}", null).invoke(context,null);
//			}
			path = getCurrentResourcePath();
		}
		else {
			path = (String) WFUtil.invoke(webDavPath);
		}
		try {
			WebdavExtendedResource oldRes = this.resource;
			WebdavExtendedResource newRes = getIWSlideSession().getWebdavResource(path);
			if (oldRes == null || oldRes.getName().equals(newRes.getName())) {
				if ((!useFolders() && !newRes.isCollection() ) || (useFolders() && newRes.isCollection())) {
					this.resource = newRes;
					this.setInitialized(false);
					getChildren().clear();
				} else if ( !useFolders() && newRes.isCollection()) {
					this.resource = newRes;
					this.setInitialized(false);
					getChildren().clear();
				}
			}
		}
		catch (IBOLookupException e) {
			e.printStackTrace();
		}
		catch (UnavailableIWContext e) {
			e.printStackTrace();
		}
		catch (HttpException e) {
			e.printStackTrace();
		}
		catch (RemoteException e) {
			e.printStackTrace();
		}
		catch (IOException e) {
			e.printStackTrace();
		}
		super.encodeBegin(context);
	}

	@Override
	public void encodeChildren(FacesContext context) throws IOException {
		super.encodeChildren(context);
		for (Iterator iter = getChildren().iterator(); iter.hasNext();) {
			UIComponent child = (UIComponent) iter.next();
			renderChild(context, child);
		}
	}

	@Override
	public boolean getRendersChildren() {
		return true;
	}
	
	protected HtmlOutputText getText(String localizationKey, String className) {
		HtmlOutputText text = getBundle().getLocalizedText(localizationKey);
		text.setStyleClass(className);
		return text;
	}
	
	protected HtmlOutputText getText(String localizationKey) {
		return getText(localizationKey, "wf_smalltext");
	}
	
	
	/**
	 * @param currentResourcePath The currentResourcePath to set.
	 */
	public void setCurrentResourcePath(String currentResourcePath) {
		this.currentResourcePath = currentResourcePath;
	}
	
	@Override
	public Object saveState(FacesContext ctx) {
		Object values[] = new Object[2];
		values[0] = super.saveState(ctx);
		values[1] = this.currentResourcePath;
		return values;
	}

	@Override
	public void restoreState(FacesContext ctx, Object state) {
		Object values[] = (Object[]) state;
		super.restoreState(ctx, values[0]);
		this.currentResourcePath = (String) values[1];
	}
}