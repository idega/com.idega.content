package com.idega.content.presentation;

import com.idega.presentation.IWBaseComponent;
import com.idega.webface.WFList;

/**
 * @author gimmi
 *
 * File that lists up a content of a WebDAV folder
 */
public class WebDAVList extends IWBaseComponent {

	private final static String WEB_DAV_LIST_BEAN_ID = "webDavList";
	
	public WebDAVList() {
	}
	
	protected void initializeContent() {
		WFList list = new WFList(WEB_DAV_LIST_BEAN_ID, 0, 2);
		list.setId("GM_TMP");
//		HtmlPanelGrid ap = WFPanelUtil.getApplicationPanel();
//		ap.getChildren().add(list);
//		add(ap);
		add(list);
	}

}
