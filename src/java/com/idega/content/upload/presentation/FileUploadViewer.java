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
import com.idega.content.upload.business.FileUploader;
import com.idega.content.upload.servlet.ContentFileUploadServlet;
import com.idega.core.builder.business.BuilderService;
import com.idega.core.component.business.ICObjectBusiness;
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

	private String	actionAfterUpload,
					actionAfterCounterReset,
					actionAfterUploadedToRepository = null,
					uploadPath = CoreConstants.PUBLIC_PATH + CoreConstants.SLASH,
					formId,
					componentToRerenderId,
					uploadId,
					maxUploadSize = String.valueOf(ContentFileUploadServlet.MAX_UPLOAD_SIZE),
					style,
					uploadService = "/servlet/ContentFileUploadServlet",
					onFail = null;

	private boolean zipFile = false,
					extractContent = false,
					themePack = false,
					showProgressBar = true,
					showLoadingMessage = false,
					allowMultipleFiles = false,
					autoAddFileInput = true,
					autoUpload, showUploadedFiles, fakeFileDeletion, stripNonRomanLetters, jQueryUploader;

	@Autowired
	private FileUploader fileUploader;

	@Autowired
	private JQuery jQuery;

	@Autowired
	private Web2Business web2;

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
		this.uploadService = values[18] == null ? null : values[18].toString();
		this.onFail = values[19] == null ? null : values[19].toString();
	}

	@Override
	public Object saveState(FacesContext context) {
		Object values[] = new Object[20];
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
		values[18] = this.uploadService;
		values[19] = this.onFail;

		return values;
	}

	private void addProperties(PresentationObjectContainer container, IWContext iwc) {
		addProperty(container, ContentConstants.UPLOADER_PATH, getUploadPath(iwc));
		addProperty(container, ContentConstants.UPLOADER_UPLOAD_ZIP_FILE, String.valueOf(zipFile));
		addProperty(container, ContentConstants.UPLOADER_UPLOAD_THEME_PACK, String.valueOf(themePack));
		addProperty(container, ContentConstants.UPLOADER_UPLOAD_EXTRACT_ARCHIVED_FILE, String.valueOf(extractContent));
		addProperty(container, ContentConstants.UPLOADER_UPLOAD_IDENTIFIER, uploadId);
		addProperty(container, ContentConstants.UPLOADER_STRIP_NON_ROMAN_LETTERS, String.valueOf(stripNonRomanLetters));
	}

	@Override
	public void initializeComponent(FacesContext context) {
		IWContext iwc = IWContext.getIWContext(context);
		Layer container = new Layer();
		add(container);

		if (isjQueryUploader() && iwc.isIE() && iwc.getBrowserVersion() < 10)
			jQueryUploader = false;

		if (isjQueryUploader()) {
			addProperties(container, iwc);
			UploadArea uploadArea = new UploadArea();
			uploadArea.setAutoUpload(true);
			uploadArea.setMultipleFiles(isAllowMultipleFiles());
			uploadArea.setUploadPath(getUploadPath(iwc));
			uploadArea.setDropZonesSelectionFunction("jQuery('#" + container.getId() + "');");
			uploadArea.setUseThumbnail(false);
			uploadArea.getUploadAreaBean().setMaxFileSize(Long.valueOf(getMaxUploadSize(context)));
			container.add(uploadArea);

			return;
		}

		ELUtil.getInstance().autowire(this);

		try {
			getFileUploader().initializeUploader(iwc);
		} catch (Exception e) {
			e.printStackTrace();
			return;
		}

		if (themePack) {
			zipFile = true;
			extractContent = true;
		}

		IWBundle bundle = ContentUtil.getBundle();
		IWResourceBundle iwrb = bundle.getResourceBundle(iwc);

		container.setStyleClass("fileUploadViewerMainLayerStyle");

		PresentationObjectContainer mainContainer = container;
		if (formId == null) {
			Form form = new Form();
			form.setMultiPart();
			form.setAction(getUploadService());
			form.setMethod("post");
			container.add(form);
			mainContainer = form;
			formId = form.getId();
		}

		addProperties(mainContainer, iwc);

		Layer fileInputs = new Layer();
		String id = fileInputs.getId();
		fileInputs.setStyleClass("fileUploadInputsContainerStyle");
		// Not adding 'remove' image - at least one file input should remain
		fileInputs.add(getFileUploader().getFileInput(iwc, id, false, isShowProgressBar(), !StringUtil.isEmpty(componentToRerenderId),
				isAutoAddFileInput(), isAllowMultipleFiles(), isAutoUpload(), getStyle()));
		mainContainer.add(fileInputs);

		Layer buttonsContainer = new Layer();
		buttonsContainer.setStyleClass("fileUploadButtonsContainerStyle");
		if (allowMultipleFiles && !iwc.isUserAgentHtml5()) {
           GenericButton addFileInput = new GenericButton(iwrb.getLocalizedString("add_file", "Add file"));
           addFileInput.setStyleClass("fileUploadAddInputStyle");
           addFileInput.setOnClick(getFileUploader().getActionToLoadFilesAndExecuteCustomAction(getFileUploader()
                           .getAddFileInputJavaScriptAction(id, iwrb, isShowProgressBar(), !StringUtil.isEmpty(componentToRerenderId),
                        		   isAutoAddFileInput(), isAutoUpload()), isShowProgressBar(), !StringUtil.isEmpty(componentToRerenderId)));
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
				isFakeFileDeletion(), getActionAfterUploadedToRepository(context), isStripNonRomanLetters(), getMaxUploadSize(context), getOnFail(context)
		));
		buttonsContainer.add(upload);
		mainContainer.add(buttonsContainer);

		//	JavaScript
		PresentationUtil.addJavaScriptSourcesLinesToHeader(iwc, Arrays.asList(
				ContentUtil.getBundle().getVirtualPathWithFileNameString("javascript/FileUploadHelper.js"),
				jQuery.getBundleURIToJQueryLib(),
				web2.getBundleUriToHumanizedMessagesScript()
		));
		if (iwc.isIE()) {
			PresentationUtil.addJavaScriptSourcesLinesToHeader(iwc, Arrays.asList(
				web2.getSWFUploadObjectScript(),
				web2.getSWFUploadScript()
			));
		}
		String initAction = getFileUploader().getPropertiesAction(iwc, id, progressBarId, uploadId, isShowProgressBar(), isShowLoadingMessage(),
				isZipFile(), getFormId(), getActionAfterUpload(), getActionAfterCounterReset(), isAutoUpload(), isShowUploadedFiles(),
				getComponentToRerenderId(), isFakeFileDeletion(), getActionAfterUploadedToRepository(iwc), isStripNonRomanLetters(),
				getMaxUploadSize(context), getOnFail(context)
		);

		StringBuilder initializAtion = new StringBuilder(getFileUploader()
				.getActionToLoadFilesAndExecuteCustomAction(initAction, false, false));

		String initializationFunction =
				new StringBuilder("var FileUploaderInitializer = {}; \n FileUploaderInitializer.initFileUploadHelper = function(){")
				.append(initializAtion).append("};").toString();
		String actionString = PresentationUtil.getJavaScriptAction(initializationFunction.toString());
		mainContainer.add(actionString);
		boolean addOnLoad = !CoreUtil.isSingleComponentRenderingProcess(iwc);
		if (addOnLoad || (iwc.isParameterSet("uiObject") && Boolean.valueOf(iwc.getParameter("uiObject")))) {
			addOnLoad = false;
		}
		if (addOnLoad) {
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

	public void setUploadService(String uploadService) {
		this.uploadService = uploadService;
	}

	public String getUploadService() {
		uploadService = "/servlet/blueimp-upload";

		if (uploadService != null)
			return uploadService;

		uploadService = getProperty(getFacesContext(), "uploadService");
		return uploadService;
	}

	private String getProperty(FacesContext context, String methodName) {
		try {
			String value = getExpressionValue(context, methodName);
			if (value != null)
				return value;

			IWContext iwc = IWContext.getIWContext(context);
			BuilderService builderService = builderLogic.getBuilderService(iwc);
			String pageKey = String.valueOf(iwc.getCurrentIBPageID());
			if (StringUtil.isEmpty(pageKey) || pageKey.equals(String.valueOf(-1)))
				return null;
			String id = getId();
			if (StringUtil.isEmpty(id) || !id.startsWith(ICObjectBusiness.UUID_PREFIX))
				return null;
			return builderService.getProperty(pageKey, id, methodName);
		} catch (Exception e) {
			Logger.getLogger(getClass().getName()).log(Level.WARNING, "Error getting value for property: " + methodName, e);
		}
		return null;
	}

	public String getUploadPath(FacesContext ctx) {
		String uploadPath = getProperty(ctx, "uploadPath");
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

	public String getActionAfterUploadedToRepository(FacesContext context) {
		if(actionAfterUploadedToRepository != null){
			return actionAfterUploadedToRepository;
		}
		actionAfterUploadedToRepository = getExpressionValue(context == null ? CoreUtil.getIWContext() : context, "actionAfterUploadedToRepository");

		return actionAfterUploadedToRepository;
	}

	public void setActionAfterUploadedToRepository(String actionAfterUploadedToRepository) {
		this.actionAfterUploadedToRepository = actionAfterUploadedToRepository;
	}
	
	public String getOnFail(FacesContext context) {
		if(onFail != null){
			return onFail;
		}
		onFail = getExpressionValue(context == null ? CoreUtil.getIWContext() : context, "onFail");
		return actionAfterUploadedToRepository;
	}

	public void setOnFail(String onFail) {
		this.onFail = onFail;
	}

	public boolean isStripNonRomanLetters() {
		return stripNonRomanLetters;
	}

	public void setStripNonRomanLetters(boolean stripNonRomanLetters) {
		this.stripNonRomanLetters = stripNonRomanLetters;
	}

	public String getStyle() {
		return style;
	}

	public void setStyle(String style) {
		this.style = style;
	}

	public String getMaxUploadSize(FacesContext context) {
		String maxUploadSize = getProperty(context, "maxUploadSize");
		if (maxUploadSize != null) {
			maxUploadSize = String.valueOf(Long.valueOf(maxUploadSize) * 1024 * 1024);	//	Converting from mega bytes to bytes
			this.maxUploadSize = maxUploadSize;
		}
		return this.maxUploadSize;
	}

	public void setMaxUploadSize(String maxUploadSize) {
		this.maxUploadSize = maxUploadSize;
	}

	public boolean isjQueryUploader() {
		return jQueryUploader;
	}

	public void setjQueryUploader(boolean jQueryUploader) {
		this.jQueryUploader = jQueryUploader;
	}

}