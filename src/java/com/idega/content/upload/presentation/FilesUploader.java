package com.idega.content.upload.presentation;

import java.rmi.RemoteException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import com.idega.block.web2.business.Web2Business;
import com.idega.builder.bean.AdvancedProperty;
import com.idega.content.business.ContentConstants;
import com.idega.core.builder.business.BuilderService;
import com.idega.presentation.Block;
import com.idega.presentation.IWContext;
import com.idega.presentation.Layer;
import com.idega.presentation.Span;
import com.idega.presentation.text.Heading1;
import com.idega.presentation.text.Link;
import com.idega.presentation.text.Text;
import com.idega.util.PresentationUtil;
import com.idega.util.StringUtil;
import com.idega.util.expression.ELUtil;

public class FilesUploader extends Block {

	private String parentPath = null;
	private String componentToRerenderId = null;
	
	@Override
	public void main(IWContext iwc) {
		Layer container = new Layer();
		add(container);
		container.setStyleClass("filesUploaderContainerStyle");
		
		if (StringUtil.isEmpty(parentPath)) {
			container.add(new Heading1(getResourceBundle(iwc).getLocalizedString("unkown_parent_path", "Provide parent path!")));
			return;
		}
		
		Link uploadButton = new Link(new Span(new Text(getResourceBundle(iwc).getLocalizedString("upload", "Upload"))));
		container.add(uploadButton);
		uploadButton.setStyleClass("filesUploaderUploadButtonStyle");
		uploadButton.setURL("javascript:void(0);");
		uploadButton.setOnClick(new StringBuilder("MOOdalBox.open('").append(getUriToComponent(iwc)).append("', '")
						.append(getResourceBundle(iwc).getLocalizedString("files_uploader.upload_files_window", "Files uploader")).append("', '400 250');")
						.toString());
		
		addResources(iwc, true);
	}
	
	private String getUriToComponent(IWContext iwc) {
		BuilderService builderService = null;
		try {
			builderService = getBuilderService(iwc);
		} catch (RemoteException e) {
			e.printStackTrace();
		}
		if (builderService == null) {
			return null;
		}
		
		List<AdvancedProperty> parameters = new ArrayList<AdvancedProperty>();
		parameters.add(new AdvancedProperty(FilesUploaderForm.PARENT_PATH_FOLDER_CHOOSER_PARAMETER, parentPath));
		if (!StringUtil.isEmpty(componentToRerenderId)) {
			parameters.add(new AdvancedProperty(FilesUploaderForm.COMPONENT_TO_RERENDER_ID_PARAMETER, componentToRerenderId));
		}
		
		return builderService.getUriToObject(FilesUploaderForm.class, parameters);
	}
	
	private void addResources(IWContext iwc, boolean useCompressedScript) {
		Web2Business web2 = ELUtil.getInstance().getBean(Web2Business.SPRING_BEAN_IDENTIFIER);
		try {
			PresentationUtil.addJavaScriptSourcesLinesToHeader(iwc, Arrays.asList(web2.getBundleURIToMootoolsLib(!useCompressedScript),
																					web2.getMoodalboxScriptFilePath(!useCompressedScript)));
			PresentationUtil.addStyleSheetToHeader(iwc, web2.getMoodalboxStyleFilePath());
		} catch (RemoteException e) {
			e.printStackTrace();
		}	
	}
	
	public String getParentPath() {
		return parentPath;
	}

	public void setParentPath(String parentPath) {
		this.parentPath = parentPath;
	}

	public String getComponentToRerenderId() {
		return componentToRerenderId;
	}

	public void setComponentToRerenderId(String componentToRerenderId) {
		this.componentToRerenderId = componentToRerenderId;
	}

	@Override
	public String getBundleIdentifier() {
		return ContentConstants.IW_BUNDLE_IDENTIFIER;
	}
	
}
