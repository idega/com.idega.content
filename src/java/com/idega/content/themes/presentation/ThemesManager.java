package com.idega.content.themes.presentation;

import javax.faces.context.FacesContext;

import com.idega.content.presentation.ContentBlock;
import com.idega.content.presentation.WebDAVUpload;
import com.idega.content.themes.bean.ThemesManagerBean;
import com.idega.presentation.IWContext;
import com.idega.presentation.Layer;
import com.idega.presentation.text.Heading1;

public class ThemesManager extends ContentBlock {
	
	private static final String FILE_TYPE = "zip";
	private static final String MIME_TYPE = "application/x-zip-compressed";
	
	protected void initializeComponent(FacesContext context) {
		IWContext iwc = IWContext.getIWContext(context);
		Layer uploadContainer = new Layer();
		uploadContainer.setStyleClass("uploadForm");
		Heading1 heading = new Heading1(getBundle().getResourceBundle(iwc).getLocalizedString("upload_theme", "Upload theme"));
		WebDAVUpload upload = new WebDAVUpload();
		String idExtension = getId() + "UploadTheme";
		upload.setId(idExtension);
		upload.setOnClickAction(getOnClickAction(idExtension));
		upload.setAccept(MIME_TYPE);
		upload.setPathProviderBeanWithMethod("#{"+ThemesManagerBean.THEMES_MANAGER_BEAN_ID+".getThemesPath}");
		upload.setUploadMethod("uploadZipFileContents");
		uploadContainer.add(heading);
		uploadContainer.add(upload);
		
		add(uploadContainer);
	}
	
	private String getOnClickAction(String idExtension) {
		StringBuffer action = new StringBuffer();
		String paramsSep = "', '";
		action.append("return isCorrectFileType('uploadForm:");
		action.append(idExtension);
		action.append("_fileupload");
		action.append(paramsSep);
		action.append(FILE_TYPE);
		action.append(paramsSep);
		action.append(getBundle().getLocalizedString("select_zip_file"));
		action.append(paramsSep);
		action.append(getBundle().getLocalizedString("incorrect_file_type"));
		action.append("')");
		return action.toString();
	}

}
