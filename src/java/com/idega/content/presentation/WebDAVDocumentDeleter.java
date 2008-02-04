/*
 * $Id: WebDAVDocumentDeleter.java,v 1.10 2008/02/04 12:13:13 valdas Exp $
 * Created on 30.12.2004
 *
 * Copyright (C) 2004 Idega Software hf. All Rights Reserved.
 *
 * This software is the proprietary information of Idega hf.
 * Use is subject to license terms.
 */
package com.idega.content.presentation;

import java.io.IOException;
import java.rmi.RemoteException;
import java.util.ArrayList;
import java.util.List;

import javax.faces.component.UICommand;
import javax.faces.component.UIComponent;
import javax.faces.component.html.HtmlCommandButton;
import javax.faces.component.html.HtmlCommandLink;
import javax.faces.component.html.HtmlForm;
import javax.faces.component.html.HtmlOutputText;
import javax.faces.context.FacesContext;
import javax.faces.event.AbortProcessingException;
import javax.faces.event.ActionEvent;
import javax.faces.event.ActionListener;

import org.apache.commons.httpclient.HttpException;

import com.idega.content.bean.ContentPathBean;
import com.idega.presentation.IWContext;
import com.idega.slide.util.WebdavExtendedResource;
import com.idega.webface.WFContainer;
import com.idega.webface.WFUtil;


/**
 * 
 *  Last modified: $Date: 2008/02/04 12:13:13 $ by $Author: valdas $
 * 
 * @author <a href="mailto:gimmi@idega.com">gimmi</a>
 * @version $Revision: 1.10 $
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
	
	protected void initializeComponent(FacesContext context) {
		String pathToUse = IWContext.getIWContext(context).getParameter(PARAMETER_PATH);
		Boolean deleted = (Boolean) WFUtil.invoke("webdavdocumentdeleterbean", "getDeleted");
		WFContainerLines = new ArrayList<UIComponent>();
		if (deleted == null) {
			String clickedPath = null;
			if (pathToUse != null) {
				clickedPath = pathToUse;
			} else {
				clickedPath =(String) WFUtil.invoke("WebDAVListBean", "getClickedFilePath");
			}
			WebdavExtendedResource resource = null;
			try {
				resource = getIWSlideSession().getWebdavResource(clickedPath);
			}
			catch (HttpException e1) {
				e1.printStackTrace();
			}
			catch (RemoteException e1) {
				e1.printStackTrace();
			}
			catch (IOException e1) {
				e1.printStackTrace();
			}
			
//			WebdavExtendedResource resource = super.getWebdavExtendedResource();
			String path = resource.getPath();
			try {
				path = path.replaceFirst(getIWSlideSession().getWebdavServerURI(), "");
			}
			catch (RemoteException e) {
				e.printStackTrace();
			}
	
			addLineToContainer(new UIComponent[]{WFUtil.getText(path)}, "wf_text", "resource_path");
			
//			if (showResourceName) {
//				HtmlOutputText resName = new HtmlOutputText();
//				resName.setValue(resource.getName());
//				resName.setStyleClass("wf_header_text");
//				
//				table.add(resName, 1, row++);
//			}
			
			if (resource.isCollection()) {
				addLineToContainer(new UIComponent[]{getText("are_you_sure_you_want_to_delete_folder")}, "verify", "verify_question");
			} else {
				addLineToContainer(new UIComponent[]{getText("are_you_sure_you_want_to_delete_file")}, "verify", "verify_question");
			}
			
			UICommand button = null;
			if (useLinkAsSubmit) {
				button = new HtmlCommandLink();
				HtmlOutputText text = getBundle().getLocalizedText("yes");
				text.setStyleClass("forcespan");
				button.getChildren().add(text);
			} else {
				button = new HtmlCommandButton();
				getBundle().getLocalizedUIComponent("yes", button);
			}
			button.getAttributes().put(ACTION, ACTION_YES);
			button.getAttributes().put(PARAMETER_PATH, resource.getPath());
			button.setActionListener(WFUtil.createMethodBinding("#{contentviewerbean.processAction}", new Class[]{ActionEvent.class}));

			addLineToContainer(new UIComponent[]{button}, "submit", "submit");
			
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
	
	public void processAction(ActionEvent arg0) throws AbortProcessingException {
		UICommand source = (UICommand) arg0.getSource();
		String path = (String) source.getAttributes().get(PARAMETER_PATH);
		String action = (String) source.getAttributes().get(ACTION);
		
		if (ACTION_YES.equals(action)) {

			WebdavExtendedResource res = super.getWebdavExentededResource(path);
			String parentPath = res.getParentPath();
			Boolean wasFolder = new Boolean(res.isCollection());
			Boolean deleted = null;
			try {
				super.refreshList();
				if (parentPath != null) {
					String currentPath = parentPath.replaceFirst(getIWSlideSession().getWebdavServerURI(), "");
					WFUtil.invoke("WebDAVListBean", "setWebDAVPath", currentPath);
					WFUtil.invoke(ContentPathBean.BEAN_ID, "setPath", currentPath);
				}
				WFUtil.invoke("WebDAVListBean","setClickedFilePath", null, String.class);
				res.deleteMethod();
				deleted = new Boolean(true);
			}
			catch (HttpException e) {
				deleted = new Boolean(false);
				e.printStackTrace();
			}
			catch (IOException e) {
				deleted = new Boolean(false);
				e.printStackTrace();
			}
			catch (Exception e) {
				deleted = new Boolean(false);
				e.printStackTrace();
			}
			WFUtil.invoke("webdavdocumentdeleterbean", "setDeleted", deleted, Boolean.class);
			WFUtil.invoke("webdavdocumentdeleterbean", "setWasFolder", wasFolder, Boolean.class);
			if (deleted != null && deleted.booleanValue() && redirectOnSuccessURI != null) {
				IWContext.getInstance().sendRedirect(redirectOnSuccessURI);
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
	
	public Object saveState(FacesContext ctx) {
		Object values[] = new Object[4];
		values[0] = super.saveState(ctx);
		values[1] = new Boolean(embedInForm);
		values[2] = redirectOnSuccessURI;
		values[3] = new Boolean(useLinkAsSubmit);
		return values;
	}

	public void restoreState(FacesContext ctx, Object state) {
		Object values[] = (Object[]) state;
		super.restoreState(ctx, values[0]);
		this.embedInForm = ((Boolean) values[1]).booleanValue();
		redirectOnSuccessURI = (String) values[2];
		this.useLinkAsSubmit = ((Boolean) values[3]).booleanValue();
	}	

}
