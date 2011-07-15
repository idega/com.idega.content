package com.idega.content.presentation;

import java.io.IOException;
import java.util.Iterator;

import javax.el.ValueExpression;
import javax.faces.component.UIComponent;
import javax.faces.component.html.HtmlOutputText;
import javax.faces.context.FacesContext;
import javax.jcr.RepositoryException;

import com.idega.content.business.ContentUtil;
import com.idega.idegaweb.IWBundle;
import com.idega.presentation.IWBaseComponent;
import com.idega.repository.bean.RepositoryItem;
import com.idega.repository.jcr.JCRItem;
import com.idega.util.CoreConstants;
import com.idega.webface.WFUtil;

/**
 * @author gimmi
 */
public abstract class ContentBlock extends IWBaseComponent {

	private JCRItem resource = null;

	// this, parentContentViewer, should not be saved in save state
	private ContentViewer parentContentViewer = null;

	protected String currentResourcePath = null;

	@Override
	protected abstract void initializeComponent(FacesContext context);

	public static IWBundle getBundle() {
		return ContentUtil.getBundle();
	}

	protected JCRItem getRepositoryItem() {
		return this.resource;
	}

	protected boolean useFolders() {
		return false;
	}

	public void refreshList() {
		WFUtil.invoke(WebDAVList.WEB_DAV_LIST_BEAN_ID, "refresh", this, UIComponent.class);
	}

	public <T extends RepositoryItem> T getWebdavExentededResource(String path) {
		try {
			return getRepositoryService().getRepositoryItem(path.replaceFirst(getRepositoryService().getWebdavServerURL(), CoreConstants.EMPTY));
		} catch (RepositoryException e) {
			e.printStackTrace();
		}
		return null;
	}

	protected boolean removeClickedFile(RepositoryItem resource) {
		try {
			String parentPath = null;
			if (resource != null) {
				parentPath = resource.getParentPath().replaceFirst(getRepositoryService().getWebdavServerURL(), CoreConstants.EMPTY);
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
		if (this.currentResourcePath != null)
			return this.currentResourcePath;

		ValueExpression ve = getValueExpression("currentResourcePath");
		String returner = ve == null ? null : (String) ve.getValue(getFacesContext().getELContext());
		if (returner instanceof String)
			return returner;

		ContentViewer v = getContentViewer();
		if (v == null)
			return null;

		String tmp = v.getCurrentResourcePath();
		return tmp;
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
			JCRItem oldRes = this.resource;
			JCRItem newRes = getRepositoryService().getRepositoryItem(path);
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
		catch (RepositoryException e) {
			e.printStackTrace();
		}
		super.encodeBegin(context);
	}

	@Override
	public void encodeChildren(FacesContext context) throws IOException {
		super.encodeChildren(context);
		for (Iterator<UIComponent> iter = getChildren().iterator(); iter.hasNext();) {
			UIComponent child = iter.next();
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