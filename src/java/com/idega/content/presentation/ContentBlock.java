package com.idega.content.presentation;

import java.io.IOException;
import java.rmi.RemoteException;
import java.util.Iterator;
import javax.faces.component.UIComponent;
import javax.faces.component.html.HtmlOutputText;
import javax.faces.context.FacesContext;
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

	private static IWBundle bundle = null;
	private IWSlideSession slideSession;
	
	// this, parentContentViewer, should not be saved in save state
	private ContentViewer parentContentViewer = null;

	protected abstract void initializeContent();

	public static IWBundle getBundle() {
		return ContentUtil.getBundle();
	}

	protected WebdavExtendedResource getWebdavExtendedResource() {
		return resource;
	}
		
	protected boolean useFolders() {
		return false;
	}

	public void refreshList() {
		WFUtil.invoke("WebDAVListBean","refresh", this, UIComponent.class);
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
		if (slideSession == null) {
			try {
				slideSession = (IWSlideSession) IBOLookup.getSessionInstance(IWContext.getInstance(),IWSlideSession.class);
			}
			catch (IBOLookupException e) {
				e.printStackTrace();
			}
			catch (UnavailableIWContext e) {
				e.printStackTrace();
			}
		}
		return slideSession;
	}
	
	protected boolean removeClickedFile(WebdavExtendedResource resource) {
		try {
			String parentPath = null;
			if (resource == null) {
				parentPath = resource.getParentPath().replaceFirst(getIWSlideSession().getWebdavServerURI(), "");
			}
			WFUtil.invoke("WebDAVListBean", "setWebDAVPath", parentPath, String.class);
			WFUtil.invoke("WebDAVListBean","setClickedFilePath", null, String.class);
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
		ContentViewer v = getContentViewer();
		if(v!=null){
			return v.getCurrentResourcePath();
		}
		return null;
	}
	
	public ContentViewer getContentViewer(){
		if(parentContentViewer == null){
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
			parentContentViewer = v;
		}
		return parentContentViewer;
	}
	
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
			WebdavExtendedResource oldRes = resource;
			WebdavExtendedResource newRes = getIWSlideSession().getWebdavResource(path);
			if (oldRes == null || oldRes.getName().equals(newRes.getName())) {
				if ((!useFolders() && !newRes.isCollection() ) || (useFolders() && newRes.isCollection())) {
					resource = newRes;
					this.setInitialized(false);
					getChildren().clear();
				} else if ( !useFolders() && newRes.isCollection()) {
					resource = newRes;
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

	public void encodeChildren(FacesContext context) throws IOException {
		super.encodeChildren(context);
		for (Iterator iter = getChildren().iterator(); iter.hasNext();) {
			UIComponent child = (UIComponent) iter.next();
			renderChild(context, child);
		}
	}

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
	
	
}