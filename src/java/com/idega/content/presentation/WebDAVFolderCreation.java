package com.idega.content.presentation;

import java.io.IOException;

import javax.faces.component.UIComponent;
import javax.faces.component.html.HtmlCommandButton;
import javax.faces.component.html.HtmlInputText;
import javax.faces.component.html.HtmlOutputText;
import javax.faces.context.FacesContext;
import javax.faces.el.ValueBinding;
import javax.faces.event.AbortProcessingException;
import javax.faces.event.ActionEvent;
import javax.faces.event.ActionListener;

import org.apache.commons.httpclient.HttpException;

import com.idega.business.IBOLookup;
import com.idega.presentation.IWContext;
import com.idega.presentation.Table;
import com.idega.slide.business.IWSlideSession;
import com.idega.slide.util.WebdavExtendedResource;
import com.idega.webface.WFUtil;

/**
 * @author gimmi
 */
public class WebDAVFolderCreation extends ContentBlock implements ActionListener {

	private static final String PARAMETER_RESOURCE_PATH = "prp";
	
	private boolean folderCreated = false;
	private String errorMessage = null;
	
	protected void initializeContent() {
		WebdavExtendedResource res = getWebdavExtendedResource();
		
		Table table = new Table();
		int row = 1;
		
		HtmlOutputText text = getBundle().getLocalizedText("name");
		text.setId(getId()+"_txtN");
		text.setStyleClass("wf_listtext");
		HtmlInputText folderName = new HtmlInputText();
		folderName.setId(this.getId()+"_inpN");
		folderName.setValueBinding("value", WFUtil.createValueBinding("#{webdavfoldercreationbean.folderName}"));

		try {
			IWSlideSession ss = (IWSlideSession) IBOLookup.getSessionInstance(IWContext.getInstance(),IWSlideSession.class);
			HtmlOutputText currFol = getBundle().getLocalizedText("current_folder");
			currFol.setStyleClass("wf_listtext");
			
			table.add(currFol, 1, row);
			table.add(WFUtil.getText(" = "+res.getPath().replaceFirst(ss.getWebdavServerURI(), ""), "wf_listtext"), 1, row);
		} catch (Exception e) {
			HtmlOutputText currFol = getBundle().getLocalizedText("failed_getting_current_folder");
			currFol.setStyleClass("wf_listtext");
			table.add(currFol, 1, row);
		}
		table.mergeCells(1, row, 2, row);
		
		if (errorMessage != null) {
			++row;
			HtmlOutputText txt = getBundle().getLocalizedText("folder_creation_failed");
			txt.setStyleClass("wf_listtext");
			
			table.add(txt, 1, row);
			table.add(WFUtil.getText(" = "+errorMessage, "wf_listtext"), 1, row);
			table.mergeCells(1, row, 2, row);
		} else if (folderCreated) {
			++row;
			HtmlOutputText txt = getBundle().getLocalizedText("folder_created");
			txt.setStyleClass("wf_listtext");

			table.add(txt, 1, row);
			table.mergeCells(1, row, 2, row);
		}
	
		++row;
		table.add(text, 1, row);
		table.add(folderName, 2, row);
		
		++row;
		HtmlCommandButton save = new HtmlCommandButton();
		save.setId(getId()+"_btnS");
		save.setActionListener(WFUtil.createMethodBinding("#{contentviewerbean.processAction}", new Class[]{ActionEvent.class}));
		save.getAttributes().put(PARAMETER_RESOURCE_PATH, res.getPath());
		table.add(save, 2, row);
		table.setAlignment(2, row, Table.HORIZONTAL_ALIGN_RIGHT);
		getChildren().add(table);
	}
	
	private void createResource(String parentPath, String name) {
		if (name != null) {
			try {
				if (name.charAt(0) != '/') {
					name = "/"+name;
				}
				name = parentPath+name;
				WebdavExtendedResource parent = getWebdavExentededResource(parentPath);
				folderCreated = parent.mkcolMethod(name);
				if (!folderCreated) {
					errorMessage = parent.getStatusMessage();
				} else {
					super.refreshList();
					ValueBinding vb = WFUtil.createValueBinding("#{webdavfoldercreationbean.folderName}");
					vb.setValue(FacesContext.getCurrentInstance(), "");
				}
			} catch (HttpException e) {
				e.printStackTrace();
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
	}

	protected boolean useFolders() {
		return true;
	}

	public void processAction(ActionEvent event) throws AbortProcessingException {
		UIComponent source = (UIComponent) event.getSource();
		String parentPath = (String) source.getAttributes().get(PARAMETER_RESOURCE_PATH);
		
		ValueBinding vb = WFUtil.createValueBinding("#{webdavfoldercreationbean.folderName}");
		HtmlInputText folderName = new HtmlInputText();
		folderName.setId(this.getId()+"_inpN");
		folderName.setValueBinding("value", vb);
		String newFolderName = (String) folderName.getValue();
		createResource(parentPath, newFolderName);
	}
}
