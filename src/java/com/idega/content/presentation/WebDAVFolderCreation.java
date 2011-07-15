package com.idega.content.presentation;

import javax.el.ValueExpression;
import javax.faces.component.UIComponent;
import javax.faces.component.html.HtmlCommandButton;
import javax.faces.component.html.HtmlInputText;
import javax.faces.component.html.HtmlOutputText;
import javax.faces.context.FacesContext;
import javax.faces.event.AbortProcessingException;
import javax.faces.event.ActionEvent;
import javax.faces.event.ActionListener;
import javax.jcr.RepositoryException;

import org.apache.myfaces.custom.htmlTag.HtmlTag;

import com.idega.content.business.ContentUtil;
import com.idega.idegaweb.IWResourceBundle;
import com.idega.presentation.IWContext;
import com.idega.repository.jcr.JCRItem;
import com.idega.util.CoreConstants;
import com.idega.webface.WFUtil;

/**
 * @author gimmi
 */
public class WebDAVFolderCreation extends ContentBlock implements ActionListener {

	protected static final String PARAMETER_RESOURCE_PATH = "prp";

	@Override
	protected void initializeComponent(FacesContext context) {
		JCRItem res = getRepositoryItem();

		//	Messages
		HtmlTag messagesContainer = (HtmlTag) context.getApplication().createComponent(HtmlTag.COMPONENT_TYPE);
		messagesContainer.setValue("div");

		Boolean folderCreated = (Boolean) WFUtil.invoke("webdavfoldercreationbean", "getFolderCreated");
		String errorMessage = (String) WFUtil.invoke("webdavfoldercreationbean", "getErrorMessage");
		if (folderCreated == null) {
			folderCreated = Boolean.FALSE;
		}
		if (errorMessage != null) {
			HtmlOutputText txt = getBundle().getLocalizedText("folder_creation_failed");
			txt.setStyleClass("wf_listtext");

			messagesContainer.getChildren().add(WFUtil.getText(" = "+errorMessage, "wf_listtext"));
		} else if (folderCreated.booleanValue()) {
			ContentViewer viewer = (ContentViewer) getParent().getParent();
			viewer.setRenderFlags(ContentViewer.ACTION_LIST);

			HtmlOutputText txt = getBundle().getLocalizedText("folder_created");
			txt.setStyleClass("wf_listtext");
			messagesContainer.getChildren().add(txt);
		}
		getChildren().add(messagesContainer);

		//	Folder creation
		HtmlTag creationContainer = (HtmlTag) context.getApplication().createComponent(HtmlTag.COMPONENT_TYPE);
		creationContainer.setValue("div");

		HtmlOutputText text = getBundle().getLocalizedText("name");
		text.setId(getId()+"_txtN");
		text.setStyleClass("wf_inputtext");
		HtmlInputText folderName = (HtmlInputText) context.getApplication().createComponent(HtmlInputText.COMPONENT_TYPE);
		folderName.setId(this.getId()+"_inpN");
		folderName.setValueExpression("value", WFUtil.createValueExpression(context.getELContext(), "#{webdavfoldercreationbean.folderName}", String.class));
		creationContainer.getChildren().add(text);
		creationContainer.getChildren().add(folderName);

		HtmlCommandButton save = (HtmlCommandButton) context.getApplication().createComponent(HtmlCommandButton.COMPONENT_TYPE);
		save.setId(getId()+"_btnS");
		save.getAttributes().put(PARAMETER_RESOURCE_PATH, res.getPath());
		save = (HtmlCommandButton) getBundle().getLocalizedUIComponent("create", save);
		save.addActionListener(WFUtil.getActionListener(context.getELContext(), "#{contentviewerbean.processAction}"));
		creationContainer.getChildren().add(save);

		getChildren().add(creationContainer);
	}

	private void createResource(String parentPath, String name) {
		if (name != null) {
			try {
				FacesContext fc = getFacesContext();
				IWResourceBundle iwrb = ContentUtil.getBundle().getResourceBundle(IWContext.getIWContext(fc));

				if (!name.startsWith(CoreConstants.SLASH)) {
					name = CoreConstants.SLASH.concat(name);
				}
				name = parentPath+name;
				boolean folderCreated = getRepositoryService().createFolderAsRoot(name);
				if (!folderCreated) {
					WFUtil.invoke("webdavfoldercreationbean", "setErrorMessage", iwrb.getLocalizedString("error_creating_folder", "Error creating folder") + ": " + name,
							String.class);
				} else {
					super.refreshList();
					WFUtil.invoke("webdavfoldercreationbean", "setFolderCreated", new Boolean(folderCreated), Boolean.class);
					ValueExpression ve = WFUtil.createValueExpression(fc.getELContext(), "#{webdavfoldercreationbean.folderName}", String.class);
					ve.setValue(fc.getELContext(), CoreConstants.EMPTY);
				}
			} catch (RepositoryException e) {
				e.printStackTrace();
			}
		}
	}

	@Override
	protected boolean useFolders() {
		return true;
	}

	@Override
	public void processAction(ActionEvent event) throws AbortProcessingException {
		UIComponent source = (UIComponent) event.getSource();

		ContentViewer.maintainPathParameter(source.getAttributes(), PARAMETER_RESOURCE_PATH);

		String newFolderName = (String) WFUtil.invoke("webdavfoldercreationbean", "getFolderName");
		createResource(source.getAttributes().get(PARAMETER_RESOURCE_PATH).toString(), newFolderName);
	}
}