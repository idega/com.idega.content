package com.idega.content.presentation;

import java.io.IOException;
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
	
//	String startPath = null;
//	String rootPath = null;
	
	public WebDAVList() {
	}
	
	protected void initializeContent() {
		
//		startPath = (String) this.getAttributes().get("startFolder");
//		rootPath = (String) this.getAttributes().get("rootFolder");

		String startFolder = (String) this.getAttributes().get("startFolder");
		String rootFolder = (String) this.getAttributes().get("rootFolder");
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
		
		this.setId(this.getId());
		WFList list = new WFList(WEB_DAV_LIST_BEAN_ID, 0, 0);
		list.setId(this.getId()+"_l");
		getChildren().add(list);

	}
	
	public void setStartFolder(String start) {
		getAttributes().put("startFolder", start);
	}
	
	public void setRootFolder(String root) {
		getAttributes().put("rootFolder", root);
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
