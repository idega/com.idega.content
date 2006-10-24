/*
 * $Id: WebDAVDocumentDeleter.java,v 1.7 2006/10/24 14:47:34 gimmi Exp $
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

import javax.faces.component.html.HtmlCommandButton;
import javax.faces.component.html.HtmlForm;
import javax.faces.context.FacesContext;
import javax.faces.event.AbortProcessingException;
import javax.faces.event.ActionEvent;
import javax.faces.event.ActionListener;

import org.apache.commons.httpclient.HttpException;

import com.idega.presentation.IWContext;
import com.idega.presentation.Table;
import com.idega.slide.util.WebdavExtendedResource;
import com.idega.webface.WFUtil;


/**
 * 
 *  Last modified: $Date: 2006/10/24 14:47:34 $ by $Author: gimmi $
 * 
 * @author <a href="mailto:gimmi@idega.com">gimmi</a>
 * @version $Revision: 1.7 $
 */
public class WebDAVDocumentDeleter extends ContentBlock implements ActionListener {

	private static final String ACTION = "dd_a";
	private static final String ACTION_YES = "dd_ay";
	public static final String PARAMETER_PATH = "dd_pp";
	
	private boolean embedInForm = false;
	
	protected void initializeComponent(FacesContext context) {
		String pathToUse = IWContext.getIWContext(context).getParameter(PARAMETER_PATH);
		Boolean deleted = (Boolean) WFUtil.invoke("webdavdocumentdeleterbean", "getDeleted");
		Table table = null;
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
	
			table = new Table();
			int row = 1;
			table.add(WFUtil.getText(path, "wf_text"), 1, row++);
			
//			if (showResourceName) {
//				HtmlOutputText resName = new HtmlOutputText();
//				resName.setValue(resource.getName());
//				resName.setStyleClass("wf_header_text");
//				
//				table.add(resName, 1, row++);
//			}
			
			if (resource.isCollection()) {
				table.add(getText("are_you_sure_you_want_to_delete_folder"), 1, row++);
			} else {
				table.add(getText("are_you_sure_you_want_to_delete_file"), 1, row++);
			}
			
			HtmlCommandButton button = new HtmlCommandButton();
			button.getAttributes().put(ACTION, ACTION_YES);
			button.getAttributes().put(PARAMETER_PATH, resource.getPath());
			button.setActionListener(WFUtil.createMethodBinding("#{contentviewerbean.processAction}", new Class[]{ActionEvent.class}));
			getBundle().getLocalizedUIComponent("yes", button);
			table.setAlignment(1, row, Table.HORIZONTAL_ALIGN_RIGHT);
			table.add(button, 1, row);
			
		} else {
			table = new Table();
			Boolean wasFolder = (Boolean) WFUtil.invoke("webdavdocumentdeleterbean", "getWasFolder");
			if (deleted.booleanValue()) {
				if (wasFolder.booleanValue()) {
					table.add(getText("folder_deleted"));
				} else {
					table.add(getText("file_deleted"));
				}
			} else {
				table.add(getText("deletion_failed"));
			}
			try {
				ContentViewer viewer = (ContentViewer) getParent().getParent();
				viewer.setRenderFlags(ContentViewer.ACTION_LIST);
			} catch (ClassCastException c) {
				System.out.println("[WebDAVDocumentDeleter] grandpa is not ContentViewer");
			}
		}
		if (table != null) {
			if (embedInForm) {
				HtmlForm form = new HtmlForm();
				form.setId("webdavdeleterform_"+getId());
				form.setEnctype("multipart/form-data");
				form.getChildren().add(table);
				getChildren().add(form);
			} else {
				getChildren().add(table);
			}
		}
	}

	public void processAction(ActionEvent arg0) throws AbortProcessingException {
		HtmlCommandButton source = (HtmlCommandButton) arg0.getSource();
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
					WFUtil.invoke("WebDAVListBean", "setWebDAVPath", parentPath.replaceFirst(getIWSlideSession().getWebdavServerURI(), ""));
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
		}
	}

	public void setEmbeddedInForm(boolean embedInForm) {
		this.embedInForm = embedInForm;
	}
	
	public boolean getEmbeddedInForm() {
		return embedInForm;
	}
	

}
