/*
 * $Id: WebDAVUpload.java,v 1.3 2005/01/13 15:53:35 gimmi Exp $
 * Created on 30.12.2004
 *
 * Copyright (C) 2004 Idega Software hf. All Rights Reserved.
 *
 * This software is the proprietary information of Idega hf.
 * Use is subject to license terms.
 */
package com.idega.content.presentation;

import javax.faces.component.html.HtmlCommandButton;
import javax.faces.component.html.HtmlGraphicImage;
import javax.faces.component.html.HtmlInputText;
import javax.faces.component.html.HtmlOutputLink;
import javax.faces.component.html.HtmlOutputText;
import javax.faces.event.ActionEvent;
import net.sourceforge.myfaces.custom.fileupload.HtmlInputFileUpload;
import com.idega.presentation.Table;
import com.idega.webface.WFContainer;
import com.idega.webface.WFUtil;


/**
 * 
 *  Last modified: $Date: 2005/01/13 15:53:35 $ by $Author: gimmi $
 * 
 * @author <a href="mailto:gimmi@idega.com">gimmi</a>
 * @version $Revision: 1.3 $
 */
public class WebDAVUpload extends ContentBlock {

	protected static final String BEAN_ID = "WebDAVUploadBean";
	
	protected void initializeContent() {

		Table table = new Table();
		int row = 1;
		HtmlOutputText selectFile = getText("select_a_file_to_upload","wf_inputtext");
		selectFile.setId(getId()+"_sel");
		
		HtmlInputFileUpload fileupload = new HtmlInputFileUpload();
		fileupload.setId(getId()+"_fileupload");
		fileupload.setAccept("*");
		fileupload.setValueBinding("value", WFUtil.createValueBinding("#{"+BEAN_ID+".uploadFile}"));
		fileupload.setStorage("file");
		fileupload.setStyleClass("fileUploadInput");

		HtmlOutputText giveName = getText("give_it_a_name_optional", "wf_inputtext");
		giveName.setId(getId()+"_giveName");
		HtmlInputText fileName = new HtmlInputText();
		fileName.setId(getId()+"_fileName");
		fileName.setValueBinding("value", WFUtil.createValueBinding("#{"+BEAN_ID+".fileName}"));
		
		HtmlOutputText versionText = getText("version_comment", "wf_inputtext");
		versionText.setId(getId()+"_versionText");
		HtmlInputText comment = new HtmlInputText();
		comment.setId(getId()+"_comment");
		comment.setValueBinding("value", WFUtil.createValueBinding("#{"+BEAN_ID+".comment}"));

		HtmlOutputText folder = getText("select_folder_optional", "wf_inputtext");
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
		upload.setStyleClass("wf_webdav_upload_button");
		upload.setActionListener(WFUtil.createMethodBinding("#{WebDAVUploadBean.upload}", new Class[]{ActionEvent.class}));
		getBundle().getLocalizedUIComponent("upload", upload);
		
//		form.getChildren().add("<br>");
		table.add(selectFile, 1, row);
		table.add(fileupload, 2, row++);
//		table.add(giveName, 1, row);
//		table.add(fileName, 2, row++);
		table.add(versionText, 1, row);
		table.add(comment, 2, row++);
		table.add(folder, 1, row);
		table.add(uploadPath, 2, row++);
		table.add(upload, 2, row++);
		table.setBorder(1);
		
		WFContainer line1 = new WFContainer();
		line1.setStyleClass("wf_webdav_upload");
		line1.setId("upload_file");
		line1.getChildren().add(selectFile);
		line1.getChildren().add(fileupload);

		WFContainer line2 = new WFContainer();
		line2.setStyleClass("wf_webdav_upload");
		line2.setId("upload_comment");
		line2.getChildren().add(versionText);
		line2.getChildren().add(comment);
		
//		WFContainer line4 = new WFContainer();
//		line4.setStyleClass("wf_webdav_upload");
//		line4.setId("upload_button");
		line2.getChildren().add(upload);

//		WFContainer line3 = new WFContainer();
//		line3.setStyleClass("wf_webdav_upload");
//		line3.setId("upload_path");
//		line3.getChildren().add(folder);
//		line3.getChildren().add(uploadPath);
		
		
		
//		WFContainer line5 = new WFContainer();
//		line5.getChildren().add(fileLink);
//		line5.getChildren().add(imagePreview);
		
		getChildren().add(line1);
		getChildren().add(line2);
//		getChildren().add(line3);
		//getChildren().add(line4);
		//getChildren().add(line5);

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
