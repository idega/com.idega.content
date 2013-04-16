/*
 * $Id: WebDAVDocumentDeleter.java,v 1.12 2009/01/12 14:44:57 valdas Exp $
 * Created on 30.12.2004
 *
 * Copyright (C) 2004 Idega Software hf. All Rights Reserved.
 *
 * This software is the proprietary information of Idega hf.
 * Use is subject to license terms.
 */
package com.idega.content.presentation;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import javax.faces.component.UICommand;
import javax.faces.component.UIComponent;
import javax.faces.component.html.HtmlForm;
import javax.faces.context.FacesContext;
import javax.faces.event.AbortProcessingException;
import javax.faces.event.ActionEvent;
import javax.faces.event.ActionListener;

import org.springframework.beans.factory.annotation.Autowired;

import com.idega.block.web2.business.JQuery;
import com.idega.block.web2.business.Web2Business;
import com.idega.content.bean.ContentPathBean;
import com.idega.content.business.DocumentsService;
import com.idega.idegaweb.IWBundle;
import com.idega.idegaweb.IWResourceBundle;
import com.idega.presentation.IWContext;
import com.idega.presentation.ui.GenericButton;
import com.idega.repository.bean.RepositoryItem;
import com.idega.util.CoreConstants;
import com.idega.util.CoreUtil;
import com.idega.util.PresentationUtil;
import com.idega.util.StringUtil;
import com.idega.util.expression.ELUtil;
import com.idega.webface.WFContainer;
import com.idega.webface.WFUtil;

/**
 *
 *  Last modified: $Date: 2009/01/12 14:44:57 $ by $Author: valdas $
 *
 * @author <a href="mailto:gimmi@idega.com">gimmi</a>
 * @version $Revision: 1.12 $
 */
public class WebDAVDocumentDeleter extends ContentBlock implements ActionListener {

	private static final String ACTION = "dd_a";
	private static final String ACTION_YES = "dd_ay";
	public static final String PARAMETER_PATH = "dd_pp";

	private boolean embedInForm = false;
	private String redirectOnSuccessURI = null;
	private boolean useLinkAsSubmit = false;
	private List<UIComponent> WFContainerLines = null;
	private HtmlForm form = null;

	@Autowired
	private JQuery jQuery;

	@Autowired
	private Web2Business web2;

	@Override
	protected void initializeComponent(FacesContext context) {
		ELUtil.getInstance().autowire(this);

		IWContext iwc = IWContext.getIWContext(context);
		String pathToUse = iwc.getParameter(PARAMETER_PATH);
		Boolean deleted = (Boolean) WFUtil.invoke("webdavdocumentdeleterbean", "getDeleted");
		WFContainerLines = new ArrayList<UIComponent>();
		if (deleted == null) {
			String clickedPath = null;
			if (pathToUse != null) {
				clickedPath = pathToUse;
			} else {
				clickedPath =(String) WFUtil.invoke(WebDAVList.WEB_DAV_LIST_BEAN_ID, "getClickedFilePath");
			}
			if (StringUtil.isEmpty(clickedPath)) {
				clickedPath = iwc.getParameter(ContentViewer.PATH_TO_DELETE);
			}

			RepositoryItem item = null;
			if (clickedPath != null) {
				try {
					item = getRepositoryService().getRepositoryItem(iwc.getLoggedInUser(), clickedPath);
				} catch (Exception e) {
					e.printStackTrace();
				}
			}
			if (item != null) {
				String path = item.getPath();
				path = path.replaceFirst(getRepositoryService().getWebdavServerURL(), CoreConstants.EMPTY);

				addLineToContainer(new UIComponent[]{WFUtil.getText(path)}, "wf_text", "resource_path");

				if (item.isCollection()) {
					addLineToContainer(new UIComponent[]{getText("are_you_sure_you_want_to_delete_folder")}, "verify", "verify_question");
				} else {
					addLineToContainer(new UIComponent[]{getText("are_you_sure_you_want_to_delete_file")}, "verify", "verify_question");
				}

				IWBundle bundle = getBundle();
				PresentationUtil.addJavaScriptSourcesLinesToHeader(iwc, Arrays.asList(
						jQuery.getBundleURIToJQueryLib(),
						web2.getBundleUriToHumanizedMessagesScript(),
						CoreConstants.DWR_ENGINE_SCRIPT,
						"/dwr/interface/" + DocumentsService.DWR_OBJECT + ".js",
						bundle.getVirtualPathWithFileNameString("javascript/DocumentsHelper.js")
				));
				PresentationUtil.addStyleSheetToHeader(iwc, web2.getBundleUriToHumanizedMessagesStyleSheet());
				IWResourceBundle iwrb = bundle.getResourceBundle(iwc);
				GenericButton button = new GenericButton(iwrb.getLocalizedString("yes", "Yes"));
				button.setOnClick("DocumentsHelper.doDeleteResource('" + iwrb.getLocalizedString("deleting", "Deleting...") + "', '" + path + "');");

				addLineToContainer(new UIComponent[]{button}, "submit", "submit");
			}
		} else {
			Boolean wasFolder = (Boolean) WFUtil.invoke("webdavdocumentdeleterbean", "getWasFolder");
			if (deleted.booleanValue()) {
				if (wasFolder.booleanValue()) {
					addLineToContainer(new UIComponent[]{getText("folder_deleted")}, "delete_result", "delete_result");
				} else {
					addLineToContainer(new UIComponent[]{getText("file_deleted")}, "delete_result", "delete_result");
				}
			} else {
				addLineToContainer(new UIComponent[]{getText("deletion_failed")}, "delete_result", "delete_result");
			}
			try {
				ContentViewer viewer = (ContentViewer) getParent().getParent();
				viewer.setRenderFlags(ContentViewer.ACTION_LIST);
			} catch (ClassCastException c) {
				System.out.println("[WebDAVDocumentDeleter] grandpa is not ContentViewer");
			}
		}

		if (embedInForm) {
			HtmlForm form = getForm();
			add(form);
		}

		addLines();
	}

	private HtmlForm getForm() {
		if (this.form == null) {
			form = new HtmlForm();
			form.setStyleClass("wf_webdav_deleter_form");
			form.setId("webdavdeleterform_"+getId());
			form.setEnctype("multipart/form-data");
		}
		return form;
	}

	private void addLineToContainer(UIComponent[] lineElements, String styleClass, String ID) {
		if (lineElements == null) {
			return;
		}
		WFContainer line = new WFContainer();
		line.setStyleClass(styleClass);
		line.setId(ID);
		for (int i = 0; i < lineElements.length; i++) {
			line.add(lineElements[i]);
		}
		WFContainerLines.add(line);
	}

	private void addLines() {
		if (WFContainerLines == null) {
			return;
		}

		if (embedInForm) {
			for (int i = 0; i < WFContainerLines.size(); i++) {
				getForm().getChildren().add(WFContainerLines.get(i));
			}
		} else {
			for (int i = 0; i < WFContainerLines.size(); i++) {
				add(WFContainerLines.get(i));
			}
		}
	}

	@Override
	public void processAction(ActionEvent arg0) throws AbortProcessingException {
		UICommand source = (UICommand) arg0.getSource();
		String path = (String) source.getAttributes().get(PARAMETER_PATH);
		String action = (String) source.getAttributes().get(ACTION);
		if (ACTION_YES.equals(action)) {
			Boolean deleted = Boolean.FALSE;
			Boolean wasFolder = Boolean.FALSE;
			try {
				RepositoryItem res = getRepositoryService().getRepositoryItem(path);
				String parentPath = res.getParentPath();
				wasFolder = res.isCollection();
				super.refreshList();
				if (parentPath != null) {
					String currentPath = parentPath.replaceFirst(getRepositoryService().getWebdavServerURL(), CoreConstants.EMPTY);
					WFUtil.invoke(WebDAVList.WEB_DAV_LIST_BEAN_ID, "setWebDAVPath", currentPath);
					WFUtil.invoke(ContentPathBean.BEAN_ID, "setPath", currentPath);
				}
				WFUtil.invoke(WebDAVList.WEB_DAV_LIST_BEAN_ID, "setClickedFilePath", null, String.class);
				deleted = res.delete();
			} catch (Exception e) {
				e.printStackTrace();
			}

			WFUtil.invoke("webdavdocumentdeleterbean", "setDeleted", deleted, Boolean.class);
			WFUtil.invoke("webdavdocumentdeleterbean", "setWasFolder", wasFolder, Boolean.class);
			if (deleted && redirectOnSuccessURI != null) {
				CoreUtil.getIWContext().sendRedirect(redirectOnSuccessURI);
			}
		}
	}

	public void setEmbeddedInForm(boolean embedInForm) {
		this.embedInForm = embedInForm;
	}

	public boolean getEmbeddedInForm() {
		return embedInForm;
	}

	public void setRedirectOnSuccessURI(String uri) {
		this.redirectOnSuccessURI = uri;
	}

	public String getRedirectOnSuccessURI() {
		return redirectOnSuccessURI;
	}

	public boolean getUseLinkAsSubmit() {
		return useLinkAsSubmit;
	}

	public void setUseLinkAsSubmit(boolean useLinkAsSubmit) {
		this.useLinkAsSubmit = useLinkAsSubmit;
	}

	@Override
	public Object saveState(FacesContext ctx) {
		Object values[] = new Object[4];
		values[0] = super.saveState(ctx);
		values[1] = new Boolean(embedInForm);
		values[2] = redirectOnSuccessURI;
		values[3] = new Boolean(useLinkAsSubmit);
		return values;
	}

	@Override
	public void restoreState(FacesContext ctx, Object state) {
		Object values[] = (Object[]) state;
		super.restoreState(ctx, values[0]);
		this.embedInForm = ((Boolean) values[1]).booleanValue();
		redirectOnSuccessURI = (String) values[2];
		this.useLinkAsSubmit = ((Boolean) values[3]).booleanValue();
	}

}
