package com.idega.content.upload.presentation;

import com.idega.content.business.ContentConstants;
import com.idega.presentation.Block;
import com.idega.presentation.IWContext;
import com.idega.presentation.Layer;

public class FilesUploaderForm extends Block {
	
	@Override
	public void main(IWContext iwc) {
		Layer container = new Layer();
		add(container);
		container.setStyleClass("filesUploaderFormStyle");
		
		Layer uploaderContainer = new Layer();
		uploaderContainer.setStyleClass("filesUploaderUploaderContainerStyle");
		container.add(uploaderContainer);
		FileUploadViewer uploader = new FileUploadViewer();
		uploaderContainer.add(uploader);
		uploader.setAllowMultipleFiles(true);
	}

	@Override
	public String getBundleIdentifier() {
		return ContentConstants.IW_BUNDLE_IDENTIFIER;
	}
}
