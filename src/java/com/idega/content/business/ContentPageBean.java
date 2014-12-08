package com.idega.content.business;

import java.util.List;

import org.springframework.context.annotation.Scope;
import org.springframework.stereotype.Service;

import com.idega.content.data.ContentPage;
import com.idega.core.business.DefaultSpringBean;
import com.idega.presentation.IWContext;
import com.idega.util.CoreConstants;
import com.idega.util.CoreUtil;
import com.idega.util.StringUtil;

@Scope("request")
@Service(ContentPageBean.BEAN_NAME)
public class ContentPageBean extends DefaultSpringBean {

	public static final String BEAN_NAME = "contentPageBean";

	private Boolean admin;

	private ContentPage page;
	private List<ContentPage> pages;

	private String currentPage, eventHandler, responseAction = CoreConstants.HASH, menuStyle;

	public ContentPage getPage() {
		return page;
	}

	public void setPage(ContentPage page) {
		this.page = page;
	}

	public List<ContentPage> getPages() {
		return pages;
	}

	public void setPages(List<ContentPage> pages) {
		this.pages = pages;
	}

	public String getCurrentPage() {
		return currentPage;
	}

	public void setCurrentPage(String currentPage) {
		this.currentPage = currentPage;
	}

	public boolean isAdmin() {
		if (admin == null) {
			IWContext iwc = CoreUtil.getIWContext();
			String role = getApplication().getSettings().getProperty("content_page_admin_role", "content_creator");
			admin = iwc.isLoggedOn() ? iwc.isSuperAdmin() || (StringUtil.isEmpty(role) ? Boolean.FALSE : iwc.hasRole(role)) : Boolean.FALSE;
		}

		return admin;
	}

	public String getEventHandler() {
		return eventHandler;
	}

	public void setEventHandler(String eventHandler) {
		this.eventHandler = eventHandler;
	}

	public String getResponseAction() {
		return responseAction;
	}

	public void setResponseAction(String responseAction) {
		this.responseAction = responseAction;
	}

	public String getMenuStyle() {
		return menuStyle;
	}

	public void setMenuStyle(String menuStyle) {
		this.menuStyle = menuStyle;
	}

}