package com.idega.content.upload.presentation;

import java.io.IOException;
import java.util.Arrays;
import java.util.UUID;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.faces.context.FacesContext;

import org.springframework.beans.factory.annotation.Autowired;

import com.idega.block.web2.business.JQuery;
import com.idega.block.web2.business.Web2Business;
import com.idega.builder.business.BuilderLogicWrapper;
import com.idega.content.business.ContentConstants;
import com.idega.content.business.ContentUtil;
import com.idega.content.themes.helpers.business.ThemesHelper;
import com.idega.content.upload.business.FileUploader;
import com.idega.content.upload.servlet.ContentFileUploadServlet;
import com.idega.core.builder.business.BuilderService;
import com.idega.idegaweb.IWBundle;
import com.idega.idegaweb.IWResourceBundle;
import com.idega.presentation.IWBaseComponent;
import com.idega.presentation.IWContext;
import com.idega.presentation.Layer;
import com.idega.presentation.PresentationObjectContainer;
import com.idega.presentation.Span;
import com.idega.presentation.ui.Form;
import com.idega.presentation.ui.GenericButton;
import com.idega.presentation.ui.HiddenInput;
import com.idega.util.CoreConstants;
import com.idega.util.CoreUtil;
import com.idega.util.PresentationUtil;
import com.idega.util.StringUtil;
import com.idega.util.expression.ELUtil;

public class FileUploadViewer extends IWBaseComponent {
	
	private String actionAfterUpload, actionAfterCounterReset, actionAfterUploadedToRepository = null;
	
	private String uploadPath = CoreConstants.PUBLIC_PATH + CoreConstants.SLASH;
	private String formId, componentToRerenderId, uploadId, maxUploadSize = String.valueOf(ContentFileUploadServlet.MAX_UPLOAD_SIZE);
	
	private boolean zipFile = false;
	private boolean extractContent = false;
	private boolean themePack = false;
	private boolean showProgressBar = true;
	private boolean showLoadingMessage = false;
	private boolean allowMultipleFiles = false;
	private boolean autoAddFileInput = true;
	private boolean autoUpload, showUploadedFiles, fakeFileDeletion, stripNonRomanLetters;
	
	@Autowired
	private FileUploader fileUploader;
	
	@Autowired
	private JQuery jQuery;
	
	@Autowired
	private Web2Business web2;
	
	@Autowired
	private ThemesHelper themesHelper;
	
	@Autowired
	private BuilderLogicWrapper builderLogic;
	
	public FileUploadViewer() {
		ELUtil.getInstance().autowire(this);
		
		this.uploadId = UUID.randomUUID().toString();
	}
	
	@Override
	public void restoreState(FacesContext context, Object state) {
		Object values[] = (Object[]) state;
		super.restoreState(context, values[0]);
		
		this.actionAfterUpload = values[1] == null ? null : values[1].toString();
		this.uploadPath = values[2] == null ? null : values[2].toString();
		
		this.zipFile = values[3] == null ? Boolean.FALSE : (Boolean) values[3];
		this.extractContent = values[4] == null ? Boolean.FALSE : (Boolean) values[4];
		this.themePack = values[5] == null ? Boolean.FALSE : (Boolean) values[5];
		this.showProgressBar = values[6] == null ? Boolean.TRUE : (Boolean) values[6];
		this.showLoadingMessage = values[7] == null ? Boolean.FALSE : (Boolean) values[7];
		this.allowMultipleFiles = values[8] == null ? Boolean.FALSE : (Boolean) values[8];
		this.autoAddFileInput = values[9] == null ? Boolean.FALSE : (Boolean) values[9];
		
		this.componentToRerenderId = values[10] == null ? null : values[10].toString();
		
		this.autoUpload = values[11] == null ? Boolean.FALSE : (Boolean) values[11];
		this.showUploadedFiles = values[12] == null ? Boolean.FALSE : (Boolean) values[12];
		this.fakeFileDeletion = values[15] == null ? Boolean.FALSE : (Boolean) values[15];
		this.stripNonRomanLetters = values[16] == null ? Boolean.FALSE : (Boolean) values[16];
		
		this.uploadId = values[13] == null ? null : values[13].toString();
		
		this.actionAfterUploadedToRepository = values[14] == null ? null : values[14].toString();
		
		this.maxUploadSize = values[17] == null ? null : values[17].toString();
	}
	
	@Override
	public Object saveState(FacesContext context) {
		Object values[] = new Object[18];
		values[0] = super.saveState(context);
		
		values[1] = this.actionAfterUpload;
		values[2] = this.uploadPath;
		
		values[3] = this.zipFile;
		values[4] = this.extractContent;
		values[5] = this.themePack;
		values[6] = this.showProgressBar;
		values[7] = this.showLoadingMessage;
		values[8] = this.allowMultipleFiles;
		values[9] = this.autoAddFileInput;
		
		values[10] = this.componentToRerenderId;
		
		values[11] = this.autoUpload;
		values[12] = this.showUploadedFiles;
		values[15] = this.fakeFileDeletion;
		values[16] = this.stripNonRomanLetters;
		
		values[13] = this.uploadId;
		
		values[14] = this.actionAfterUploadedToRepository;
		
		values[17] = this.maxUploadSize;
		
		return values;
	}
	
	@Override
	public void initializeComponent(FacesContext context) {
		IWContext iwc = IWContext.getIWContext(context);
		try {
			getFileUploader().initializeUploader(iwc);
		} catch (Exception e) {
			e.printStackTrace();
			return;
		}
		
		if (themePack) {
			themesHelper.getSlideService(iwc);
			zipFile = true;
			extractContent = true;
		}
		
		IWBundle bundle = ContentUtil.getBundle();
		IWResourceBundle iwrb = bundle.getResourceBundle(iwc);
		
		Layer container = new Layer();
		add(container);
		container.setStyleClass("fileUploadViewerMainLayerStyle");
		
		PresentationObjectContainer mainContainer = container;
		if (formId == null) {
			Form form = new Form();
			form.setMultiPart();
			form.setAction("/servlet/ContentFileUploadServlet");
			form.setMethod("post");
			container.add(form);
			mainContainer = form;
			formId = form.getId();
		}
		
		addProperty(mainContainer, ContentConstants.UPLOADER_PATH, getUploadPath(iwc));
		addProperty(mainContainer, ContentConstants.UPLOADER_UPLOAD_ZIP_FILE, String.valueOf(zipFile));
		addProperty(mainContainer, ContentConstants.UPLOADER_UPLOAD_THEME_PACK, String.valueOf(themePack));
		addProperty(mainContainer, ContentConstants.UPLOADER_UPLOAD_EXTRACT_ARCHIVED_FILE, String.valueOf(extractContent));
		addProperty(mainContainer, ContentConstants.UPLOADER_UPLOAD_IDENTIFIER, uploadId);
		addProperty(mainContainer, ContentConstants.UPLOADER_STRIP_NON_ROMAN_LETTERS, String.valueOf(stripNonRomanLetters));
		
		Layer fileInputs = new Layer();
		String id = fileInputs.getId();
		fileInputs.setStyleClass("fileUploadInputsContainerStyle");
		// Not adding 'remove' image - at least one file input should remain
		fileInputs.add(getFileUploader().getFileInput(iwc, id, false, isShowProgressBar(), !StringUtil.isEmpty(componentToRerenderId), isAutoAddFileInput(),
				isAutoUpload()));
		mainContainer.add(fileInputs);
		
		Layer buttonsContainer = new Layer();
		buttonsContainer.setStyleClass("fileUploadButtonsContainerStyle");
		
		if (allowMultipleFiles) {
			GenericButton addFileInput = new GenericButton(iwrb.getLocalizedString("add_file", "Add file"));
			addFileInput.setOnClick(getFileUploader().getActionToLoadFilesAndExecuteCustomAction(getFileUploader()
					.getAddFileInputJavaScriptAction(id, iwrb, isShowProgressBar(), !StringUtil.isEmpty(componentToRerenderId), isAutoAddFileInput(),
							isAutoUpload()), isShowProgressBar(), !StringUtil.isEmpty(componentToRerenderId)));
			buttonsContainer.add(addFileInput);
		}
		
		Layer progressBarBox = new Layer();
		Span progressBar = new Span();
		progressBarBox.add(progressBar);
		progressBar.setStyleClass("progressBar");
		String progressBarId = progressBar.getId();
		mainContainer.add(progressBarBox);
		
		GenericButton upload = new GenericButton(iwrb.getLocalizedString("upload", "Upload"));
		upload.setStyleClass("fileUploadButtonStyle");
		if (autoUpload) {
			upload.setStyleAttribute("display", "none");
		}
		upload.setOnClick(getFileUploader().getUploadAction(iwc, id, progressBarId, uploadId, isShowProgressBar(), isShowLoadingMessage(), isZipFile(),
				getFormId(), getActionAfterUpload(), getActionAfterCounterReset(), isAutoUpload(), isShowUploadedFiles(), getComponentToRerenderId(),
				isFakeFileDeletion(), getActionAfterUploadedToRepository(), isStripNonRomanLetters(), getMaxUploadSize(context)
		));
		buttonsContainer.add(upload);
		mainContainer.add(buttonsContainer);
		
		//	JavaScript
		PresentationUtil.addJavaScriptSourcesLinesToHeader(iwc, Arrays.asList(
				ContentUtil.getBundle().getVirtualPathWithFileNameString("javascript/FileUploadHelper.js"),
				jQuery.getBundleURIToJQueryLib(),
				web2.getBundleUriToHumanizedMessagesScript()
		));
		String initAction = getFileUploader().getPropertiesAction(iwc, id, progressBarId, uploadId, isShowProgressBar(), isShowLoadingMessage(), isZipFile(),
				getFormId(), getActionAfterUpload(), getActionAfterCounterReset(), isAutoUpload(), isShowUploadedFiles(), getComponentToRerenderId(),
				isFakeFileDeletion(), getActionAfterUploadedToRepository(), isStripNonRomanLetters(), getMaxUploadSize(context)
		);
		if (!CoreUtil.isSingleComponentRenderingProcess(iwc)) {
			initAction = new StringBuilder("jQuery(window).load(function() {").append(initAction).append("});").toString();
		}
		PresentationUtil.addJavaScriptActionToBody(iwc, initAction);
		
		//	CSS
		PresentationUtil.addStyleSheetToHeader(iwc, web2.getBundleUriToHumanizedMessagesStyleSheet());
	}
	
	private void addProperty(PresentationObjectContainer container, String name, String value) {
		HiddenInput property = new HiddenInput(name, value);
		property.setStyleClass(name);
		container.add(property);
	}
	
	@Override
	public void encodeBegin(FacesContext context) throws IOException {
		super.encodeBegin(context);
	}
	
	public String getActionAfterUpload() {
		if (StringUtil.isEmpty(componentToRerenderId)) {
			return actionAfterUpload;
		}
		
		String reRenderAction = new StringBuilder("FileUploadHelper.reRenderComponent('").append(componentToRerenderId).append("');").toString();
		if (StringUtil.isEmpty(actionAfterUpload)) {
			return reRenderAction;
		}
		
		if (!actionAfterUpload.endsWith(CoreConstants.SEMICOLON)) {
			actionAfterUpload += CoreConstants.SEMICOLON;
		}
		return actionAfterUpload + reRenderAction;
	}
	
	public void setActionAfterUpload(String actionAfterUpload) {
		this.actionAfterUpload = actionAfterUpload;
	}
	
	public String getUploadPath() {
		return uploadPath;
	}
	
	private String getProperty(FacesContext context, String methodName) {
		try {
			IWContext iwc = IWContext.getIWContext(context);
			BuilderService builderService = builderLogic.getBuilderService(iwc);
			String pageKey = String.valueOf(iwc.getCurrentIBPageID());
			return builderService.getProperty(pageKey, getId(), methodName);
		} catch (Exception e) {
			Logger.getLogger(getClass().getName()).log(Level.WARNING, "Error getting value for property: " + methodName, e);
		}
		return null;
	}
	
	public String getUploadPath(FacesContext ctx) {
		String uploadPath = getExpressionValue(ctx, "uploadPath");
		if (uploadPath == null)
			uploadPath = getProperty(ctx, "uploadPath");
		if (uploadPath != null)
			this.uploadPath = uploadPath;
		
		return this.uploadPath;
	}
	
	public void setUploadPath(String uploadPath) {
		this.uploadPath = uploadPath;
	}
	
	public boolean isZipFile() {
		return zipFile;
	}
	
	public void setZipFile(boolean zipFile) {
		this.zipFile = zipFile;
	}
	
	public boolean isExtractContent() {
		return extractContent;
	}
	
	public void setExtractContent(boolean extractContent) {
		this.extractContent = extractContent;
	}
	
	public boolean isThemePack() {
		return themePack;
	}
	
	public void setThemePack(boolean themePack) {
		this.themePack = themePack;
	}
	
	public boolean isShowProgressBar() {
		return showProgressBar;
	}
	
	public void setShowProgressBar(boolean showProgressBar) {
		this.showProgressBar = showProgressBar;
	}
	
	public boolean isShowLoadingMessage() {
		return showLoadingMessage;
	}
	
	public void setShowLoadingMessage(boolean showLoadingMessage) {
		this.showLoadingMessage = showLoadingMessage;
	}
	
	public boolean isAllowMultipleFiles() {
		return allowMultipleFiles;
	}
	
	public void setAllowMultipleFiles(boolean allowMultipleFiles) {
		this.allowMultipleFiles = allowMultipleFiles;
	}
	
	public String getFormId() {
		return formId;
	}
	
	public void setFormId(String formId) {
		this.formId = formId;
	}
	
	public String getActionAfterCounterReset() {
		return actionAfterCounterReset;
	}
	
	public void setActionAfterCounterReset(String actionAfterCounterReset) {
		this.actionAfterCounterReset = actionAfterCounterReset;
	}
	
	public boolean isAutoAddFileInput() {
		return autoAddFileInput;
	}
	
	public void setAutoAddFileInput(boolean autoAddFileInput) {
		this.autoAddFileInput = autoAddFileInput;
	}
	
	public String getComponentToRerenderId() {
		return componentToRerenderId;
	}
	
	public void setComponentToRerenderId(String componentToRerenderId) {
		this.componentToRerenderId = componentToRerenderId;
	}

	public boolean isAutoUpload() {
		return autoUpload;
	}

	public void setAutoUpload(boolean autoUpload) {
		this.autoUpload = autoUpload;
	}

	public boolean isShowUploadedFiles() {
		return showUploadedFiles;
	}

	public void setShowUploadedFiles(boolean showUploadedFiles) {
		this.showUploadedFiles = showUploadedFiles;
	}

	public FileUploader getFileUploader() {
		return fileUploader;
	}

	public void setFileUploader(FileUploader fileUploader) {
		this.fileUploader = fileUploader;
	}

	public JQuery getJQuery() {
		return jQuery;
	}

	public void setJQuery(JQuery query) {
		jQuery = query;
	}

	public Web2Business getWeb2() {
		return web2;
	}

	public void setWeb2(Web2Business web2) {
		this.web2 = web2;
	}

	public boolean isFakeFileDeletion() {
		return fakeFileDeletion;
	}

	public void setFakeFileDeletion(boolean fakeFileDeletion) {
		this.fakeFileDeletion = fakeFileDeletion;
	}

	public String getUploadId() {
		return uploadId;
	}

	public String getActionAfterUploadedToRepository() {
		return actionAfterUploadedToRepository;
	}

	public void setActionAfterUploadedToRepository(String actionAfterUploadedToRepository) {
		this.actionAfterUploadedToRepository = actionAfterUploadedToRepository;
	}

	public boolean isStripNonRomanLetters() {
		return stripNonRomanLetters;
	}

	public void setStripNonRomanLetters(boolean stripNonRomanLetters) {
		this.stripNonRomanLetters = stripNonRomanLetters;
	}

	public String getMaxUploadSize(FacesContext context) {
		String maxUploadSize = getExpressionValue(context, "maxUploadSize");
		if (maxUploadSize == null)
			maxUploadSize = getProperty(context, "maxUploadSize");
		if (maxUploadSize != null) {
			maxUploadSize = String.valueOf(Long.valueOf(maxUploadSize) * 1024 * 1024);	//	Converting from mega bytes to bytes
			this.maxUploadSize = maxUploadSize;
		}
		return this.maxUploadSize;
	}

	public void setMaxUploadSize(String maxUploadSize) {
		this.maxUploadSize = maxUploadSize;
	}
}