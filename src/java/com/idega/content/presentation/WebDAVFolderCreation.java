package com.idega.content.presentation;

import java.io.IOException;

import javax.faces.component.html.HtmlCommandButton;
import javax.faces.component.html.HtmlInputText;
import javax.faces.component.html.HtmlOutputText;
import javax.faces.context.FacesContext;
import javax.faces.el.ValueBinding;
import javax.faces.event.AbortProcessingException;
import javax.faces.event.ActionEvent;
import javax.faces.event.ActionListener;

import org.apache.commons.httpclient.HttpException;

import com.idega.presentation.Table;
import com.idega.slide.util.WebdavExtendedResource;
import com.idega.webface.WFUtil;

/**
 * @author gimmi
 */
public class WebDAVFolderCreation extends ContentBlock implements ActionListener {

	private String newFolderName = null;
	private boolean folderCreated = false;
	private String errorMessage = null;
	
	protected void initializeContent() {
		WebdavExtendedResource res = getWebdavExtendedResource();
		createResource(res, newFolderName);
		
		Table table = new Table();
		int row = 1;
		
		HtmlOutputText text = getBundle().getLocalizedText("name");
		text.setId(getId()+"_txtN");
		text.setStyleClass("wf_listtext");
		HtmlInputText folderName = new HtmlInputText();
		folderName.setId(this.getId()+"_inpN");
		folderName.setValueBinding("value", WFUtil.createValueBinding("#{webdavfoldercreationbean.folderName}"));
		
		table.add(WFUtil.getText("Current folder = "+res.getDecodedPath(), "wf_listtext"), 1, row);
		table.mergeCells(1, row, 2, row);
		
		if (errorMessage != null) {
			++row;
			table.add(WFUtil.getText("Creation failed = "+errorMessage, "wf_listtext"), 1, row);
			table.mergeCells(1, row, 2, row);
		} else if (folderCreated) {
			++row;
			table.add(WFUtil.getText("FolderCreated", "wf_listtext"), 1, row);
			table.mergeCells(1, row, 2, row);
		}
	
		++row;
		table.add(text, 1, row);
		table.add(folderName, 2, row);
		
		++row;
		HtmlCommandButton save = new HtmlCommandButton();
		save.setId(getId()+"_btnS");
		save.setActionListener(WFUtil.createMethodBinding("#{contentviewerbean.processAction}", new Class[]{ActionEvent.class}));
		table.add(save, 2, row);
		table.setAlignment(2, row, Table.HORIZONTAL_ALIGN_RIGHT);
		getChildren().add(table);
	}
	
	private void createResource(WebdavExtendedResource parent, String name) {
		if (name != null) {
			try {
				if (name.charAt(0) != '/') {
					name = "/"+name;
				}
				name = parent.getPath()+name;
				folderCreated = parent.mkcolMethod(name);
				if (!folderCreated) {
					errorMessage = parent.getStatusMessage();
				} else {
					super.refreshList();
					
					// Clearing inputField
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
		ValueBinding vb = WFUtil.createValueBinding("#{webdavfoldercreationbean.folderName}");
		HtmlInputText folderName = new HtmlInputText();
		folderName.setId(this.getId()+"_inpN");
		folderName.setValueBinding("value", vb);
		newFolderName = (String) folderName.getValue();
	}
}
