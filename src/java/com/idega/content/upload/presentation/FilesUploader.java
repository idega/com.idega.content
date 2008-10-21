package com.idega.content.upload.presentation;

import java.rmi.RemoteException;
import java.util.ArrayList;
import java.util.List;

import com.idega.block.web2.business.Web2Business;
import com.idega.block.web2.business.Web2BusinessBean;
import com.idega.builder.bean.AdvancedProperty;
import com.idega.content.business.ContentConstants;
import com.idega.core.builder.business.BuilderService;
import com.idega.presentation.Block;
import com.idega.presentation.IWContext;
import com.idega.presentation.Layer;
import com.idega.presentation.Span;
import com.idega.presentation.text.Link;
import com.idega.presentation.text.Text;
import com.idega.util.CoreConstants;
import com.idega.util.CoreUtil;
import com.idega.util.PresentationUtil;
import com.idega.util.expression.ELUtil;

public class FilesUploader extends Block {

	@Override
	public void main(IWContext iwc) {
		Layer container = new Layer();
		add(container);
		container.setStyleClass("filesUploaderContainerStyle");
		
		Link uploadButton = new Link(new Span(new Text(getResourceBundle(iwc).getLocalizedString("upload", "Upload"))));
		container.add(uploadButton);
		uploadButton.setStyleClass("filesUploaderUploadButtonStyle");
		uploadButton.setURL(getUriToComponent(iwc));
		uploadButton.setMarkupAttribute("rel", "sexylightbox");
		uploadButton.setOnClick(new StringBuilder("$('").append(uploadButton.getId())
									.append("').href += '&height=' + Math.round(window.getHeight() * 0.5) + '&width=' + Math.round(window.getWidth() * 0.8);")
								.toString());
		
		String imagesDir = iwc.getIWMainApplication().getBundle(Web2BusinessBean.WEB2_BUNDLE_IDENTIFIER).getVirtualPathWithFileNameString(
			new StringBuilder("javascript/").append(Web2BusinessBean.SEXY_LIGHT_BOX_FOLDER_NAME_PREFIX).append(CoreConstants.SLASH)
			.append(Web2BusinessBean.SEXY_LIGHT_BOX_LATEST_VERSION).append("/sexyimages").toString());
		StringBuilder initAction = new StringBuilder("new SexyLightBox({imagesdir: '").append(imagesDir).append("'});");
		if (!CoreUtil.isSingleComponentRenderingProcess(iwc)) {
			initAction = new StringBuilder("window.addEvent('domready', function() { ").append(initAction.toString()).append(" });");
		}
		PresentationUtil.addJavaScriptActionToBody(iwc, initAction.toString());
		
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
		
		List<AdvancedProperty> parameters = new ArrayList<AdvancedProperty>();	//	TODO: do we need any?
		
		return builderService.getUriToObject(FilesUploaderForm.class, parameters);
	}
	
	private void addResources(IWContext iwc, boolean useCompressedScript) {
		Web2Business web2 = ELUtil.getInstance().getBean(Web2Business.SPRING_BEAN_IDENTIFIER);
		PresentationUtil.addJavaScriptSourcesLinesToHeader(iwc, web2.getBundleURIsToSexyLightBoxScriptFiles(useCompressedScript));
		PresentationUtil.addStyleSheetToHeader(iwc, web2.getBundleURIToSexyLightBoxStyleFile());		
	}
	
	@Override
	public String getBundleIdentifier() {
		return ContentConstants.IW_BUNDLE_IDENTIFIER;
	}
	
}
