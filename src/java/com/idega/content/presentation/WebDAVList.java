package com.idega.content.presentation;

import java.io.IOException;
import java.util.Collection;
import java.util.Iterator;
import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import com.idega.presentation.IWBaseComponent;
import com.idega.webface.WFList;
import com.idega.webface.WFUtil;

/**
 * @author gimmi
 *
 * Component that lists up a content of a WebDAV folder
 */
public class WebDAVList extends IWBaseComponent {
	
	public final static String WEB_DAV_LIST_BEAN_ID = "WebDAVListBean";
	
	private String startFolder = null;
	private String rootFolder = null;
	private String iconTheme = null;
	private boolean showFolders = true;
	private Collection columnsToHide = null;
	
	public WebDAVList() {
	}
	
	protected void initializeContent() {
		
		if (startFolder != null) {
			WFUtil.invoke(WEB_DAV_LIST_BEAN_ID, "setStartFolder", startFolder);
		} else {
			WFUtil.invoke(WEB_DAV_LIST_BEAN_ID, "setStartFolder", "");
		}
		if (rootFolder != null) {
			WFUtil.invoke(WEB_DAV_LIST_BEAN_ID, "setRootFolder", rootFolder);
		} else {
			WFUtil.invoke(WEB_DAV_LIST_BEAN_ID, "setRootFolder", "");
		}
		if (iconTheme != null) {
			WFUtil.invoke(WEB_DAV_LIST_BEAN_ID, "setIconTheme", iconTheme);
		} else {
			WFUtil.invoke(WEB_DAV_LIST_BEAN_ID, "setIconTheme", "");
		}
		
		WFUtil.invoke(WEB_DAV_LIST_BEAN_ID, "setShowFolders", new Boolean(showFolders));
		
		if (columnsToHide != null) {
			WFUtil.invoke(WEB_DAV_LIST_BEAN_ID, "setColumnsToHide", columnsToHide, Collection.class);
		}
		
		
		this.setId(this.getId());
		WFList list = new WFList(WEB_DAV_LIST_BEAN_ID, 0, 0);
		list.setId(this.getId()+"_l");
		getChildren().add(list);

	}
	
	public void setStartFolder(String start) {
		this.startFolder = start;
	}
	
	public void setRootFolder(String root) {
		this.rootFolder = root;
	}
	
	public void setIconTheme(String theme) {
		this.iconTheme = theme;
	}
	
	public void setShowFolders(boolean showFolders) {
		this.showFolders = showFolders;
	}
	
	public void setColumnsToHide(Collection columns) {
		this.columnsToHide = columns;
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
