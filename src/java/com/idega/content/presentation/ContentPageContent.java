package com.idega.content.presentation;

import javax.faces.context.FacesContext;

import org.springframework.beans.factory.annotation.Autowired;

import com.idega.business.IBORuntimeException;
import com.idega.content.business.ContentConstants;
import com.idega.content.business.ContentPageBean;
import com.idega.content.data.dao.ContentPageDAO;
import com.idega.facelets.ui.FaceletComponent;
import com.idega.idegaweb.IWBundle;
import com.idega.presentation.IWBaseComponent;
import com.idega.presentation.IWContext;
import com.idega.util.expression.ELUtil;

public class ContentPageContent extends IWBaseComponent {

	@Autowired
	private ContentPageDAO dao;

	@Override
	protected void initializeComponent(FacesContext context) {
		IWContext iwc = IWContext.getIWContext(context);
		IWBundle bundle = iwc.getIWMainApplication().getBundle(ContentConstants.IW_BUNDLE_IDENTIFIER);

		try {
			ContentPageBean bean = getBeanInstance(ContentPageBean.BEAN_NAME);

			if (iwc.isParameterSet(ContentPagesMenu.PARAMETER_CONTENT_PAGE_ID)) {
				bean.setPage(getDao().getContentPage(Long.valueOf(iwc.getParameter(ContentPagesMenu.PARAMETER_CONTENT_PAGE_ID))));
			}

			FaceletComponent facelet = (FaceletComponent)iwc.getApplication().createComponent(FaceletComponent.COMPONENT_TYPE);
			facelet.setFaceletURI(bundle.getFaceletURI("content-page-content.xhtml"));
			add(facelet);
		} catch (Exception re) {
			throw new IBORuntimeException(re);
		}
	}

	private ContentPageDAO getDao() {
		if (dao == null) {
			ELUtil.getInstance().autowire(this);
		}

		return dao;
	}

}