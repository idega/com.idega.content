package com.idega.content.presentation;

import java.io.IOException;
import java.rmi.RemoteException;
import java.util.Iterator;
import java.util.Vector;
import javax.faces.component.UIComponent;
import javax.faces.component.html.HtmlOutputText;
import javax.faces.context.FacesContext;
import org.apache.commons.httpclient.HttpException;
import com.idega.business.IBOLookup;
import com.idega.business.IBOLookupException;
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

	public static final String IW_BUNDLE_IDENTIFIER = "com.idega.content";

	private static IWBundle bundle = null;

	protected abstract void initializeContent();

	public static IWBundle getBundle() {
		if (bundle == null) {
			setupBundle();
		}
		return bundle;
	}

	private static void setupBundle() {
		FacesContext context = FacesContext.getCurrentInstance();
		IWContext iwContext = IWContext.getIWContext(context);
		bundle = iwContext.getIWMainApplication().getBundle(IW_BUNDLE_IDENTIFIER);
	}

	protected WebdavExtendedResource getWebdavExtendedResource() {
		return resource;
	}
		
	protected boolean useFolders() {
		return false;
	}

	public void refreshList() {
		WFUtil.invoke("WebDAVListBean","refresh");
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
	
	public void encodeBegin(FacesContext context) throws IOException {
		String webDavPath = (String) this.getAttributes().get("path");
		String path = null;
		if (webDavPath == null) {
			path = (String) WFUtil.createMethodBinding("#{WebDAVListBean.getClickedFilePath}", null).invoke(context,null);
			if (path == null) {
				path = (String) WFUtil.createMethodBinding("#{WebDAVListBean.getWebDAVPath}", null).invoke(context,null);
			}
		}
		else {
			path = (String) WFUtil.invoke(webDavPath);
		}
		try {
			IWSlideSession ss = (IWSlideSession) IBOLookup.getSessionInstance(IWContext.getInstance(),IWSlideSession.class);
			WebdavExtendedResource oldRes = resource;
			WebdavExtendedResource newRes = ss.getWebdavResource(path);
			if (oldRes == null || oldRes.getName().equals(newRes.getName())) {
				if ((!useFolders() && !newRes.isCollection() ) || (useFolders() && newRes.isCollection())) {
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