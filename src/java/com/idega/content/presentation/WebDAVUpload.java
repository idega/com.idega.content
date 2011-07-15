/*
 * $Id: WebDAVUpload.java,v 1.13 2008/02/04 12:13:13 valdas Exp $
 * Created on 30.12.2004
 *
 * Copyright (C) 2004 Idega Software hf. All Rights Reserved.
 *
 * This software is the proprietary information of Idega hf.
 * Use is subject to license terms.
 */
package com.idega.content.presentation;

import java.util.ArrayList;
import java.util.List;

import javax.faces.component.UICommand;
import javax.faces.component.UIComponent;
import javax.faces.component.html.HtmlCommandButton;
import javax.faces.component.html.HtmlCommandLink;
import javax.faces.component.html.HtmlForm;
import javax.faces.component.html.HtmlGraphicImage;
import javax.faces.component.html.HtmlInputText;
import javax.faces.component.html.HtmlOutputLink;
import javax.faces.component.html.HtmlOutputText;
import javax.faces.context.FacesContext;
import javax.faces.el.ValueBinding;
import javax.faces.event.ActionEvent;

import org.apache.myfaces.custom.fileupload.HtmlInputFileUpload;
import org.apache.myfaces.custom.savestate.UISaveState;

import com.idega.content.business.WebDAVUploadBean;
import com.idega.webface.WFContainer;
import com.idega.webface.WFUtil;


/**
 *
 *  Last modified: $Date: 2008/02/04 12:13:13 $ by $Author: valdas $
 *
 * @author <a href="mailto:gimmi@idega.com">gimmi</a>
 * @version $Revision: 1.13 $
 */
public class WebDAVUpload extends ContentBlock {

	public static final String BEAN_ID = "WebDAVUploadBean";

	protected static final String DEFAULT_OUTPUT_TEXT_STYLE = "wf_inputtext";

	protected static final String DEFAULT_BUTTON_STYLE = "wf_webdav_upload_button";

	protected static final String DEFAULT_FORM_STYLE = "wf_webdav_upload_form";

	protected static final String DEFAULT_INPUT_FILE_STYLE = "fileUploadInput";

	protected static final String DEFAULT_WF_CONTAINER_LINE_STYLE = "wf_webdav_upload";

	protected static final String DEFAULT_FILE_ACCEPT_PATTERN = "*";

	protected static final String DEFAULT_FILE_LINK_TARGET = "_new";

	protected static final String DEFAULT_STORAGE = "file";

	protected static final String DEFAULT_UPLOAD_METHOD = "upload";

	private String uploadMethod = null;

	private String styleClassSelectFile = null;

	private String styleClassFileUploadInput = null;

	private String accept = null;

	private String storage = null;

	private String styleClassGiveName = null;

	private String styleClassVersionText = null;

	private String styleClassFolder = null;

	private String fileLinkTarget = null;

	private String styleClassButton = null;

	private String styleClassWFContainerLine = null;

	private boolean useFileName;

	private boolean useVersionComment;

	private boolean useUploadPath;

	private boolean useFileLink;

	private boolean useImagePreview;

	private String pathProviderBeanWithMethod;

	private String onClickAction;

	private List<WFContainer> WFContainerLines = null;

	private String uploadPath;

	private boolean embedInForm = false;

	private boolean useUserHomeFolder = false;

	private HtmlForm form;

	private boolean showStatusAfterUploadAttempt = false;
	private String redirectOnSuccessURI = null;
	private boolean useLinkAsSubmit = false;

	@Override
	protected void initializeComponent(FacesContext context) {


		WebDAVUploadBean bean = (WebDAVUploadBean) WFUtil.getBeanInstance(BEAN_ID);
		WFContainerLines = new ArrayList<WFContainer>();

		if ( showStatusAfterUploadAttempt && bean.wasUploadAttemped()) {
			String message = bean.getUploadMessage();
			Boolean success = bean.isUploadSuccessful();

			HtmlOutputText status = null;
			String sStatus = "failed";
			if (success.booleanValue()) {
				status = getText("file_uploaded_successfully", getStyleClassGiveName());
				sStatus = "success";
			} else {
				status = getText("file_upload_failed", getStyleClassGiveName());
			}
			status.setId(getId()+"_status");
			status.setTitle(message);

			addLineToContainer(new UIComponent[] {status}, getStyleClassWFContainerLine()+ " "+sStatus, "status");
			bean.setWasUploadAttempted(null);
		}

		HtmlOutputText selectFile = getText("select_a_file_to_upload", getStyleClassSelectFile());
		selectFile.setId(getId()+"_sel");

		HtmlInputFileUpload fileUpload = new HtmlInputFileUpload();
		fileUpload.setId(getId()+"_fileupload");
		fileUpload.setAccept(getAccept());
		fileUpload.setValueBinding("value", WFUtil.createValueBinding("#{"+BEAN_ID+".uploadFile}"));
		fileUpload.setStorage(getStorage());
		fileUpload.setStyleClass(getStyleClassFileUploadInput());

		HtmlOutputText giveName = null;
		HtmlInputText fileName = null;
		if (useFileName) {
			giveName = getText("give_it_a_name_optional", getStyleClassGiveName());
			giveName.setId(getId()+"_giveName");

			fileName = new HtmlInputText();
			fileName.setId(getId()+"_fileName");
			fileName.setValueBinding("value", WFUtil.createValueBinding("#{"+BEAN_ID+".fileName}"));
		}

		HtmlOutputText versionText = null;
		HtmlInputText comment = null;
		if (useVersionComment) {
			versionText = getText("version_comment", getStyleClassVersionText());
			versionText.setId(getId()+"_versionText");

			comment = new HtmlInputText();
			comment.setId(getId()+"_comment");
			comment.setValueBinding("value", WFUtil.createValueBinding("#{"+BEAN_ID+".comment}"));
		}

		HtmlOutputText folder = null;
		HtmlInputText uploadPath = null;
		if (useUploadPath) {
			folder = getText("select_folder_optional", getStyleClassFolder());
			folder.setId(getId()+"_folder");

			uploadPath = getUploadPathElement(getId()+"_uploadPath", WFUtil.createValueBinding("#{"+WebDAVList.WEB_DAV_LIST_BEAN_ID+".webDAVPath}"));
		}
		if (pathProviderBeanWithMethod != null) {
			String path = (String) WFUtil.invoke(pathProviderBeanWithMethod);
			WFUtil.invoke(BEAN_ID, "setUploadFilePath", path, String.class);
		}
		if (this.useUserHomeFolder) {
			try {
				String homeFolder = getRepositorySession().getUserHomeFolder();
				bean.setUploadFilePath(homeFolder);
			} catch (Exception e) {
				e.printStackTrace();
			}
		} else if (this.uploadPath != null) {
			bean.setUploadFilePath(this.uploadPath);
		}
		if (redirectOnSuccessURI != null) {
			bean.setRedirectOnSuccessURI(redirectOnSuccessURI);
		}
		HtmlOutputLink fileLink = null;
		if (useFileLink) {
			fileLink = new HtmlOutputLink();
			fileLink.setId(getId()+"_fileLink");
			fileLink.setValueBinding("value", WFUtil.createValueBinding("#{"+BEAN_ID+".downloadPath}"));
			fileLink.setTarget(getFileLinkTarget());
			fileLink.setValueBinding("rendered", WFUtil.createValueBinding("#{"+BEAN_ID+".isUploaded}"));
			fileLink.getChildren().add(getText("click_to_get_the_file"));
		}

		HtmlGraphicImage imagePreview = null;
		if (useImagePreview) {
			imagePreview = new HtmlGraphicImage();
			imagePreview.setId(getId()+"_imagePreview");
			imagePreview.setValueBinding("value", WFUtil.createValueBinding("#{"+BEAN_ID+".imagePath}"));
		}

		UICommand upload = null;
		if (!useLinkAsSubmit) {
			upload = new HtmlCommandButton();
			((HtmlCommandButton) upload).setStyleClass(getStyleClassButton());
			if (onClickAction != null) {
				((HtmlCommandButton) upload).setOnclick(onClickAction);
			}
			getBundle().getLocalizedUIComponent("upload", upload);
		} else {
			upload = new HtmlCommandLink();
			((HtmlCommandLink) upload).setStyleClass(getStyleClassButton());
			if (onClickAction != null) {
				((HtmlCommandLink) upload).setOnclick(onClickAction);
			}
			HtmlOutputText text = getBundle().getLocalizedText("upload");
			text.setStyleClass("forcespan");
			upload.getChildren().add(text);

		}
		upload.setId(getId()+"_uploadCmd");
		upload.setActionListener(WFUtil.createMethodBinding("#{"+BEAN_ID+"."+getUploadMethod()+"}", new Class[]{ActionEvent.class}));



		addLineToContainer(new UIComponent[] {selectFile, fileUpload}, getStyleClassWFContainerLine(), "upload_file");

		if (useFileName) {
			addLineToContainer(new UIComponent[] {giveName, fileName}, getStyleClassWFContainerLine()+" filename", "file_name");
		}

		if (useVersionComment) {
			addLineToContainer(new UIComponent[] {versionText, comment}, getStyleClassWFContainerLine()+" comment", "upload_comment");
		}

		if (useUploadPath) {
			addLineToContainer(new UIComponent[] {folder, uploadPath}, getStyleClassWFContainerLine()+" uploadpath", "upload_path");
		}

		if (useFileLink && useImagePreview) {
			addLineToContainer(new UIComponent[] {fileLink, imagePreview}, getStyleClassWFContainerLine()+ "filelink_imgprev", "file_link_image_preview");
		}
		else {
			if (useFileLink) {
				addLineToContainer(new UIComponent[] {fileLink}, getStyleClassWFContainerLine()+" filelink", "file_link");
			}
			if (useImagePreview) {
				addLineToContainer(new UIComponent[] {imagePreview}, getStyleClassWFContainerLine()+" imgprev", "image_preview");
			}
		}

		addElementToLastLine(upload);

		if (embedInForm) {
			add(getForm());
		}

		addLines();


//		Saving the state of the webuploadbean specially because the scope
		//of this bean now is 'request' not 'session'
		UISaveState beanSaveState = new UISaveState();
		ValueBinding binding = WFUtil.createValueBinding("#{"+BEAN_ID+"}");
		beanSaveState.setId("webdavuploadIds");
		beanSaveState.setValueBinding("value",binding);

		if (embedInForm) {
			form.getChildren().add(beanSaveState);
		} else {
			add(beanSaveState);
		}
	}

	private HtmlForm getForm() {
		if (form == null) {
			form = new HtmlForm();
			form.setId("webdavuploadform_"+getId());
			form.setEnctype("multipart/form-data");
		}
		return form;
	}
	private HtmlInputText getUploadPathElement(String ID, ValueBinding valueBinding) {
		HtmlInputText uploadPath = new HtmlInputText();
		uploadPath.setId(ID);
		uploadPath.setValueBinding("value", valueBinding);
		return uploadPath;
	}

	private void addLineToContainer(UIComponent[] lineElements, String styleClass, String ID) {
		if (lineElements == null) {
			return;
		}
		WFContainer line = new WFContainer();
		line.setStyleClass(styleClass);
		line.setId(ID);
		for (int i = 0; i < lineElements.length; i++) {
			line.add(lineElements[i]);
		}
		WFContainerLines.add(line);
	}

	private void addElementToLastLine(UIComponent element) {
		if (WFContainerLines == null) {
			return;
		}
		WFContainer line = WFContainerLines.get(WFContainerLines.size() - 1);
		line.add(element);
	}

	private void addLines() {
		if (WFContainerLines == null) {
			return;
		}
		if (embedInForm) {
			for (int i = 0; i < WFContainerLines.size(); i++) {
				getForm().getChildren().add(WFContainerLines.get(i));
			}
		} else {
			for (int i = 0; i < WFContainerLines.size(); i++) {
				add(WFContainerLines.get(i));
			}
		}
	}

	public String getUploadMethod() {
		return uploadMethod == null ? DEFAULT_UPLOAD_METHOD : uploadMethod;
	}

	public void setUploadMethod(String uploadMethod) {
		this.uploadMethod = uploadMethod;
	}

	public String getAccept() {
		return accept == null ? DEFAULT_FILE_ACCEPT_PATTERN : accept;
	}

	public void setAccept(String accept) {
		this.accept = accept;
	}

	public String getFileLinkTarget() {
		return fileLinkTarget == null ? DEFAULT_FILE_LINK_TARGET : fileLinkTarget;
	}

	public void setFileLinkTarget(String fileLinkTarget) {
		this.fileLinkTarget = fileLinkTarget;
	}

	public String getStyleClassFileUploadInput() {
		return styleClassFileUploadInput == null ? DEFAULT_INPUT_FILE_STYLE : styleClassFileUploadInput;
	}

	public void setStyleClassFileUploadInput(String styleClassFileUploadInput) {
		this.styleClassFileUploadInput = styleClassFileUploadInput;
	}

	public String getStyleClassFolder() {
		return styleClassFolder == null ? DEFAULT_OUTPUT_TEXT_STYLE : styleClassFolder;
	}

	public void setStyleClassFolder(String styleClassFolder) {
		this.styleClassFolder = styleClassFolder;
	}

	public String getStyleClassGiveName() {
		return styleClassGiveName == null ? DEFAULT_OUTPUT_TEXT_STYLE : styleClassGiveName;
	}

	public void setStyleClassGiveName(String styleClassGiveName) {
		this.styleClassGiveName = styleClassGiveName;
	}

	public String getStyleClassSelectFile() {
		return styleClassSelectFile == null ? DEFAULT_OUTPUT_TEXT_STYLE : styleClassSelectFile;
	}

	public void setStyleClassSelectFile(String styleClassSelectFile) {
		this.styleClassSelectFile = styleClassSelectFile;
	}

	public String getStyleClassButton() {
		return styleClassButton == null ? DEFAULT_BUTTON_STYLE : styleClassButton;
	}

	public void setStyleClassButton(String styleClassButton) {
		this.styleClassButton = styleClassButton;
	}

	public String getStyleClassVersionText() {
		return styleClassVersionText == null ? DEFAULT_OUTPUT_TEXT_STYLE : styleClassVersionText;
	}

	public void setStyleClassVersionText(String styleClassVersionText) {
		this.styleClassVersionText = styleClassVersionText;
	}

	public String getStyleClassWFContainerLine() {
		return styleClassWFContainerLine == null ? DEFAULT_WF_CONTAINER_LINE_STYLE : styleClassWFContainerLine;
	}

	public void setStyleClassWFContainerLine(String styleClassWFContainerLine) {
		this.styleClassWFContainerLine = styleClassWFContainerLine;
	}

	public String getStorage() {
		return storage == null ? DEFAULT_STORAGE : storage;
	}

	public void setStorage(String storage) {
		this.storage = storage;
	}

	public boolean isUseFileLink() {
		return useFileLink;
	}

	public void setUseFileLink(boolean useFileLink) {
		this.useFileLink = useFileLink;
	}

	public boolean isUseFileName() {
		return useFileName;
	}

	public void setUseFileName(boolean useFileName) {
		this.useFileName = useFileName;
	}

	public boolean isUseImagePreview() {
		return useImagePreview;
	}

	public void setUseImagePreview(boolean useImagePreview) {
		this.useImagePreview = useImagePreview;
	}

	public boolean isUseUploadPath() {
		return useUploadPath;
	}

	public void setUseUploadPath(boolean useUploadPath) {
		this.useUploadPath = useUploadPath;
	}

	public boolean isUseVersionComment() {
		return useVersionComment;
	}

	public void setUseVersionComment(boolean useVersionComment) {
		this.useVersionComment = useVersionComment;
	}

	public String getPathProviderBeanWithMethod() {
		return pathProviderBeanWithMethod;
	}

	public void setPathProviderBeanWithMethod(String fileUploadFolder) {
		this.pathProviderBeanWithMethod = fileUploadFolder;
	}

	public String getOnClickAction() {
		return onClickAction;
	}

	public void setOnClickAction(String onClickAction) {
		this.onClickAction = onClickAction;
	}

	public void setUploadPath(String uploadPath) {
		this.uploadPath = uploadPath;
	}

	public String getUploadPath() {
		return uploadPath;
	}

	public boolean getEmbeddedInForm() {
		return embedInForm;
	}

	public void setEmbeddedInForm(boolean embedInForm) {
		this.embedInForm = embedInForm;
	}

	/**
	 * Sets the viewer to view the current users home folder content. Overrides
	 * the setStartingPointURI method
	 * @param useUserHomeFolder
	 */
	public void setUseUserHomeFolder(boolean useUserHomeFolder) {
		this.useUserHomeFolder = useUserHomeFolder;
	}


	public boolean getUseUserHomeFolder() {
		return useUserHomeFolder;
	}

	public boolean getShowStatusAfterUploadAttempt() {
		return showStatusAfterUploadAttempt;
	}

	public void setShowStatusAfterUploadAttempt(boolean showStatusAfterUploadAttempt) {
		this.showStatusAfterUploadAttempt = showStatusAfterUploadAttempt;
	}
	public void setRedirectOnSuccessURI(String uri) {
		this.redirectOnSuccessURI = uri;
	}

	public String getRedirectOnSuccessURI() {
		return redirectOnSuccessURI;
	}

	public boolean getUseLinkAsSubmit() {
		return useLinkAsSubmit;
	}

	public void setUseLinkAsSubmit(boolean useLinkAsSubmit) {
		this.useLinkAsSubmit = useLinkAsSubmit;
	}
	@Override
	public Object saveState(FacesContext ctx) {
		Object values[] = new Object[6];
		values[0] = super.saveState(ctx);
		values[1] = new Boolean(useUserHomeFolder);
		values[2] = new Boolean(embedInForm);
		values[3] = new Boolean(showStatusAfterUploadAttempt);
		values[4] = redirectOnSuccessURI;
		values[5] = new Boolean(useLinkAsSubmit);
		return values;
	}

	@Override
	public void restoreState(FacesContext ctx, Object state) {
		Object values[] = (Object[]) state;
		super.restoreState(ctx, values[0]);
		this.useUserHomeFolder = ((Boolean) values[1]).booleanValue();
		this.embedInForm = ((Boolean) values[2]).booleanValue();
		showStatusAfterUploadAttempt = ((Boolean) values[3]).booleanValue();
		redirectOnSuccessURI = (String) values[4];
		useLinkAsSubmit = ((Boolean) values[5]).booleanValue();
	}

}
