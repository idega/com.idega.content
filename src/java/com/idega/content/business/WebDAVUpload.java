/*
 * $Id: WebDAVUpload.java,v 1.1 2004/12/30 12:39:34 gimmi Exp $
 * Created on 30.12.2004
 *
 * Copyright (C) 2004 Idega Software hf. All Rights Reserved.
 *
 * This software is the proprietary information of Idega hf.
 * Use is subject to license terms.
 */
package com.idega.content.business;

import javax.faces.component.html.HtmlCommandButton;
import javax.faces.component.html.HtmlForm;
import javax.faces.component.html.HtmlGraphicImage;
import javax.faces.component.html.HtmlInputText;
import javax.faces.component.html.HtmlOutputLink;
import javax.faces.component.html.HtmlOutputText;
import net.sourceforge.myfaces.custom.fileupload.HtmlInputFileUpload;
import com.idega.content.presentation.ContentBlock;
import com.idega.content.presentation.WebDAVList;
import com.idega.webface.WFUtil;


/**
 * 
 *  Last modified: $Date: 2004/12/30 12:39:34 $ by $Author: gimmi $
 * 
 * @author <a href="mailto:gimmi@idega.com">gimmi</a>
 * @version $Revision: 1.1 $
 */
public class WebDAVUpload extends ContentBlock {

	protected static final String BEAN_ID = "WebDAVUploadBean";
	
	protected void initializeContent() {
		
		HtmlForm form = new HtmlForm();
		form.setId("uploadForm");
		form.setEnctype("multipart/form-data");

		HtmlOutputText selectFile = getText("select_a_file_to_upload");
		selectFile.setId(getId()+"_sel");
		
		HtmlInputFileUpload fileupload = new HtmlInputFileUpload();
		fileupload.setId(getId()+"_fileupload");
		fileupload.setAccept("*");
		fileupload.setValueBinding("value", WFUtil.createValueBinding("#{"+BEAN_ID+".uploadFile}"));
		fileupload.setStorage("file");
		fileupload.setStyleClass("fileUploadInput");

		HtmlOutputText giveName = getText("give_it_a_name_optional");
		giveName.setId(getId()+"_giveName");
		HtmlInputText fileName = new HtmlInputText();
		fileName.setId(getId()+"_fileName");
		fileName.setValueBinding("value", WFUtil.createValueBinding("#{"+BEAN_ID+".fileName}"));
		
		HtmlOutputText versionText = getText("version_comment");
		versionText.setId(getId()+"_versionText");
		HtmlInputText comment = new HtmlInputText();
		comment.setId(getId()+"_comment");
		comment.setValueBinding("value", WFUtil.createValueBinding("#{"+BEAN_ID+".comment}"));

		HtmlOutputText folder = getText("select_folder_optional");
		folder.setId(getId()+"_folder");
		HtmlInputText uploadPath = new HtmlInputText();
		uploadPath.setId(getId()+"_uploadPath");
		uploadPath.setValueBinding("value", WFUtil.createValueBinding("#{"+WebDAVList.WEB_DAV_LIST_BEAN_ID+".webDAVPath}"));
		
		HtmlOutputLink fileLink = new HtmlOutputLink();
		fileLink.setId(getId()+"_fileLink");
		fileLink.setValueBinding("value", WFUtil.createValueBinding("#{"+BEAN_ID+".downloadPath}"));
		fileLink.setTarget("_new");
		fileLink.setValueBinding("rendered", WFUtil.createValueBinding("#{"+BEAN_ID+".isUploaded}"));
		fileLink.getChildren().add(getText("click_to_get_the_file"));
		
		HtmlGraphicImage imagePreview = new HtmlGraphicImage();
		imagePreview.setId(getId()+"_imagePreview");
		imagePreview.setValueBinding("value", WFUtil.createValueBinding("#{"+BEAN_ID+".imagePath}"));
		
		HtmlCommandButton upload = new HtmlCommandButton();
		upload.setId(getId()+"_uploadCmd");
		upload.setAction(WFUtil.createMethodBinding("#{WebDAVUploadBean.upload}", null));
		
//		form.getChildren().add("<br>");
		form.getChildren().add(selectFile);
		form.getChildren().add(fileupload);
		form.getChildren().add(giveName);
		form.getChildren().add(fileName);
		form.getChildren().add(versionText);
		form.getChildren().add(comment);
		form.getChildren().add(folder);
		form.getChildren().add(fileLink);
		form.getChildren().add(imagePreview);
		form.getChildren().add(upload);

		getChildren().add(form);

//    <f:verbatim><br/></f:verbatim>
//    <h:outputText value="Select a file to upload : "/>
// <cmf:inputFileUpload id="fileupload"
//                       accept="*"
//                       value="#{WebDAVUploadBean.uploadFile}"
//                       storage="file"
//                       styleClass="fileUploadInput"/>
//<f:verbatim><br/></f:verbatim>
//    <h:outputText id ="nametext" value="and give it a name (optional) : "/>
//    <h:inputText id="filename" value="#{WebDAVUploadBean.fileName}"/>
//       <f:verbatim><br/></f:verbatim>
//    <h:outputText id="versiontext" value="and a version comment : "/>
//       <h:inputText id="comment" value="#{WebDAVUploadBean.comment}"/>
//    <f:verbatim><br/></f:verbatim>
//    <h:outputText id ="outtext" value="and select the folder to upload to (optional) : "/>
//     <h:inputText id="uploadPath" value="#{WebDAVListBean.webDAVPath}"/>
//    <h:commandButton value="Upload" action="#{WebDAVUploadBean.upload}"/>
//
//<f:verbatim><br/></f:verbatim>
//<c:WebDAVFileDetails id="id" webDAVPath="#{WebDAVListBean.getClickedFilePath}"/>
//
//<h:outputLink id="filelink" value="#{WebDAVUploadBean.downloadPath}" target="_new" rendered="#{WebDAVUploadBean.isUploaded}" >
//<f:verbatim>Click here to get the file</f:verbatim>
//</h:outputLink>
//<f:verbatim><br/></f:verbatim>
//<h:graphicImage id="imagePreview" value="#{WebDAVUploadBean.imagePath}"/>
		
		
		
	}


}
