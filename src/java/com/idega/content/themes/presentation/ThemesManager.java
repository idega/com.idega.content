package com.idega.content.themes.presentation;

import javax.faces.context.FacesContext;

import com.idega.content.presentation.ContentBlock;
import com.idega.content.presentation.WebDAVUpload;
import com.idega.content.themes.bean.ThemesManagerBean;
import com.idega.webface.WFBlock;
import com.idega.webface.WFTitlebar;

public class ThemesManager extends ContentBlock {
	
	private static final String FILE_TYPE = "zip";
	private static final String MIME_TYPE = "application/x-zip-compressed";
	
	protected void initializeComponent(FacesContext context) {
		WFBlock uploadBlock = new WFBlock();
		WFTitlebar uploadBar = new WFTitlebar();
		uploadBar.addTitleText(getBundle().getLocalizedText("upload_theme"));
		uploadBlock.setTitlebar(uploadBar);
		WebDAVUpload upload = new WebDAVUpload();
		String idExtension = getId() + "UploadTheme";
		upload.setId(idExtension);
		upload.setOnClickAction(getOnClickAction(idExtension));
		upload.setAccept(MIME_TYPE);
		upload.setPathProviderBeanWithMethod("#{"+ThemesManagerBean.THEMES_MANAGER_BEAN_ID+".getThemesPath}");
		upload.setUploadMethod("uploadZipFileContents");
		uploadBlock.add(upload);
		
		add(uploadBlock);
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
