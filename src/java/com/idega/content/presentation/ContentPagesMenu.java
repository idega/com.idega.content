package com.idega.content.presentation;

import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;

import javax.faces.context.FacesContext;

import org.springframework.beans.factory.annotation.Autowired;

import com.idega.block.web2.business.JQuery;
import com.idega.block.web2.business.Web2Business;
import com.idega.builder.bean.AdvancedProperty;
import com.idega.builder.business.BuilderLogicWrapper;
import com.idega.business.IBORuntimeException;
import com.idega.content.business.ContentConstants;
import com.idega.content.business.ContentPageBean;
import com.idega.content.data.ContentPage;
import com.idega.content.data.dao.ContentPageDAO;
import com.idega.event.IWPageEventListener;
import com.idega.facelets.ui.FaceletComponent;
import com.idega.idegaweb.IWBundle;
import com.idega.idegaweb.IWException;
import com.idega.idegaweb.IWMainApplication;
import com.idega.presentation.IWBaseComponent;
import com.idega.presentation.IWContext;
import com.idega.util.ListUtil;
import com.idega.util.PresentationUtil;
import com.idega.util.expression.ELUtil;

public class ContentPagesMenu extends IWBaseComponent implements IWPageEventListener {

	private static final String PARAMETER_ACTION = "prm_menu_action";

	protected static final String	PARAMETER_PAGE_ID = "prm_page_id",
									PARAMETER_CONTENT_PAGE_ID = "prm_content_page_id";
	private static final String PARAMETER_HEADLINE = "prm_headline";
	private static final String PARAMETER_TEXT = "prm_text";

	private static final int ACTION_VIEW = 1;
	private static final int ACTION_EDIT = 2;
	private static final int ACTION_DELETE = 3;

	@Autowired
	private ContentPageDAO dao;

	@Autowired
	private BuilderLogicWrapper builder;

	@Autowired
	private JQuery jQuery;

	@Autowired
	private Web2Business web2;

	private String menuStyle;

	@Override
	protected void initializeComponent(FacesContext context) {
		ELUtil.getInstance().autowire(this);

		IWContext iwc = IWContext.getIWContext(context);
		IWBundle bundle = iwc.getIWMainApplication().getBundle(ContentConstants.IW_BUNDLE_IDENTIFIER);

		try {
			int pageId = builder.getBuilderService(iwc).getCurrentPageId(iwc);

			ContentPageBean bean = getBeanInstance(ContentPageBean.BEAN_NAME);
			bean.setCurrentPage(builder.getBuilderService(iwc).getPageURI(pageId));
			List<ContentPage> pages = dao.getContentPagesInPage(pageId);
			bean.setPages(pages);
			bean.setMenuStyle(getMenuStyle());

			if (iwc.isParameterSet(PARAMETER_CONTENT_PAGE_ID)) {
				bean.setPage(dao.getContentPage(Long.valueOf(iwc.getParameter(PARAMETER_CONTENT_PAGE_ID))));
			} else if (!ListUtil.isEmpty(pages) && iwc.getIWMainApplication().getSettings().getBoolean("content_page_show_first_if_empty", Boolean.TRUE)) {
				bean.setPage(pages.get(0));
			}

			if (bean.isAdmin()) {
				bean.setEventHandler(IWMainApplication.getEncryptedClassName(ContentPagesMenu.class));

				PresentationUtil.addJavaScriptSourceLineToHeader(iwc, jQuery.getBundleURIToJQueryLib());
				PresentationUtil.addJavaScriptSourcesLinesToHeader(iwc, web2.getScriptsForTinyMCE());
				PresentationUtil.addJavaScriptSourcesLinesToHeader(iwc, web2.getBundleURIsToFancyBoxScriptFiles());
				PresentationUtil.addJavaScriptSourceLineToHeader(iwc, bundle.getVirtualPathWithFileNameString("javascript/content-page-menu.js"));

				PresentationUtil.addStyleSheetToHeader(iwc, web2.getBundleURIToFancyBoxStyleFile());
			}
			PresentationUtil.addStyleSheetToHeader(iwc, bundle.getVirtualPathWithFileNameString("style/content-page-style.css"));

			String responseAction = builder.getBuilderService(iwc).getUriToObject(ContentPagesMenu.class, new ArrayList<AdvancedProperty>());
			bean.setResponseAction(responseAction);

			FaceletComponent facelet = (FaceletComponent)iwc.getApplication().createComponent(FaceletComponent.COMPONENT_TYPE);
			switch (parseAction(iwc)) {
				case ACTION_VIEW:
					facelet.setFaceletURI(bundle.getFaceletURI("content-menu.xhtml"));
					break;

				case ACTION_EDIT:
					if (iwc.isParameterSet("prm_new_page") && Boolean.valueOf(iwc.getParameter("prm_new_page"))) {
						bean.setPage(null);
					}
					facelet.setFaceletURI(bundle.getFaceletURI("content-page-edit.xhtml"));
					break;

				case ACTION_DELETE:
					bean.setPage(null);
					facelet.setFaceletURI(bundle.getFaceletURI("content-menu.xhtml"));
					break;
			}
			add(facelet);
		} catch (Exception e) {
			getLogger().log(Level.WARNING, "Error rendering content pages menu", e);
		}
	}

	private int parseAction(IWContext iwc) {
		int action = iwc.isParameterSet(PARAMETER_ACTION) ? Integer.parseInt(iwc.getParameter(PARAMETER_ACTION)) : ACTION_VIEW;
		return action;
	}

	@Override
	public boolean actionPerformed(IWContext iwc) throws IWException {
		if (dao == null) {
			ELUtil.getInstance().autowire(this);
		}

		try {
			switch (parseAction(iwc)) {
				case ACTION_VIEW:
					int pageId = builder.getBuilderService(iwc).getCurrentPageId(iwc);
					ContentPage page = dao.storeContentPage(
						pageId,
						iwc.isParameterSet(PARAMETER_CONTENT_PAGE_ID) ? Long.valueOf(iwc.getParameter(PARAMETER_CONTENT_PAGE_ID)) : null,
						iwc.getParameter(PARAMETER_HEADLINE),
						iwc.getParameter(PARAMETER_TEXT)
					);
					if (page == null) {
						getLogger().warning("Unable to store content for page " + pageId);
					}
					break;

				case ACTION_DELETE:
					dao.removeContentPage(Long.valueOf(iwc.getParameter(PARAMETER_CONTENT_PAGE_ID)));
					break;
			}
		} catch (Exception e) {
			throw new IBORuntimeException(e);
		}

		return true;
	}

	public String getMenuStyle() {
		return menuStyle;
	}

	public void setMenuStyle(String menuStyle) {
		this.menuStyle = menuStyle;
	}

}