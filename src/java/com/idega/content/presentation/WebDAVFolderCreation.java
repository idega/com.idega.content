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
import com.idega.presentation.Table;
import com.idega.slide.util.WebdavExtendedResource;
import com.idega.webface.WFUtil;

/**
 * @author gimmi
 */
public class WebDAVFolderCreation extends ContentBlock implements ActionListener {

	private static final String PARAMETER_RESOURCE_PATH = "prp";
		
	protected void initializeContent() {
		WebdavExtendedResource res = getWebdavExtendedResource();
		
		Table table = new Table();
		int row = 1;
		
		HtmlOutputText text = getBundle().getLocalizedText("name");
		text.setId(getId()+"_txtN");
		text.setStyleClass("wf_inputtext");
		HtmlInputText folderName = new HtmlInputText();
		folderName.setId(this.getId()+"_inpN");
		folderName.setValueBinding("value", WFUtil.createValueBinding("#{webdavfoldercreationbean.folderName}"));
		
		Boolean folderCreated = (Boolean) WFUtil.invoke("webdavfoldercreationbean", "getFolderCreated");
		String errorMessage = (String) WFUtil.invoke("webdavfoldercreationbean", "getErrorMessage");
		if (folderCreated == null) {
			folderCreated = new Boolean(false);
		}
		if (errorMessage != null) {
			HtmlOutputText txt = getBundle().getLocalizedText("folder_creation_failed");
			txt.setStyleClass("wf_listtext");
			
			table.add(txt, 1, row);
			table.add(WFUtil.getText(" = "+errorMessage, "wf_listtext"), 1, row);
			table.mergeCells(1, row, 2, row);
		} else if (folderCreated.booleanValue()) {
			ContentViewer viewer = (ContentViewer) getParent().getParent();
			viewer.setRenderFlags(ContentViewer.ACTION_LIST);
			folderName.setDisabled(true);

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
		save = (HtmlCommandButton) getBundle().getLocalizedUIComponent("create", save);
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
				boolean folderCreated = parent.mkcolMethod(name);
				String errorMessage = null;
				if (!folderCreated) {
					errorMessage = parent.getStatusMessage();
					WFUtil.invoke("webdavfoldercreationbean", "setErrorMessage", errorMessage, String.class);
				} else {
					super.refreshList();
					WFUtil.invoke("webdavfoldercreationbean", "setFolderCreated", new Boolean(folderCreated), Boolean.class);
//					ContentViewer viewer = (ContentViewer) getParent().getParent();
//					viewer.getFacet(ContentViewer.NEW_FOLDER).setRendered(false);
//					viewer.setRenderFlags(ContentViewer.LIST);
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
		
		String newFolderName = (String) WFUtil.invoke("webdavfoldercreationbean", "getFolderName");
		createResource(parentPath, newFolderName);
	}
}
