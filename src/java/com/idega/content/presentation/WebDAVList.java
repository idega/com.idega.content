package com.idega.content.presentation;

import java.io.IOException;
import java.util.Iterator;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;

import com.idega.presentation.IWBaseComponent;
import com.idega.webface.WFList;

/**
 * @author gimmi
 *
 * Component that lists up a content of a WebDAV folder
 */
public class WebDAVList extends IWBaseComponent {

	public final static String WEB_DAV_LIST_BEAN_ID = "webDavList";

	public WebDAVList() {
	}
	
	protected void initializeContent() {
		this.setId(this.getId());
		WFList list = new WFList(WEB_DAV_LIST_BEAN_ID, 0, 0);
		list.setId(this.getId()+"_l");
		getChildren().add(list);
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
