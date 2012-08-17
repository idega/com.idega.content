package com.idega.content.upload.presentation;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;

import com.google.gson.Gson;
import com.idega.block.web2.business.JQuery;
import com.idega.block.web2.business.Web2Business;
import com.idega.content.business.ContentConstants;
import com.idega.content.upload.business.UploadAreaBean;
import com.idega.content.upload.servlet.BlueimpUploadServlet;
import com.idega.core.idgenerator.business.UUIDGenerator;
import com.idega.idegaweb.IWBundle;
import com.idega.idegaweb.IWResourceBundle;
import com.idega.presentation.IWBaseComponent;
import com.idega.presentation.IWContext;
import com.idega.presentation.Layer;
import com.idega.presentation.Span;
import com.idega.presentation.ui.CheckBox;
import com.idega.util.CoreConstants;
import com.idega.util.PresentationUtil;
import com.idega.util.StringUtil;
import com.idega.util.expression.ELUtil;
import com.idega.webface.WFUtil;

public class UploadArea  extends IWBaseComponent{
	
	
	private String name = "uploaded-files";
	private StringBuilder scriptOnLoad = null;
	
	private String dropZonesSelectionFunction = "jQuery('document')";
	
	private String uploadPath = null;
	
	private String styleClass = "idega-upload-area";
	
	private UploadAreaBean uploadAreaBean = null;
	
	private boolean autoUpload = false;
	
	private Boolean addStartUploadsButton = null;
	
	private boolean addCancelUploadsButton = false;
	
	private boolean addDeleteUploadsButton = false;
	
	private boolean addUploadsProgressLayer = false;
	
	
	public UploadAreaBean getUploadAreaBean(){
		if(uploadAreaBean == null){
			uploadAreaBean = ELUtil.getInstance().getBean(UploadAreaBean.BEAN_NAME);
		}
		return uploadAreaBean;
	}
	
	@Override
	protected void initializeComponent(FacesContext context) {
		super.initializeComponent(context);
		IWContext iwc = IWContext.getIWContext(context);
		IWResourceBundle iwrb = iwc.getIWMainApplication().getBundle(ContentConstants.IW_BUNDLE_IDENTIFIER).getResourceBundle(iwc);
		addFileInput(iwc, iwrb);
		addScriptOnLoad(iwc);
		addFiles(iwc);
	}
	
	private UIComponent getAddfilesButton(IWResourceBundle iwrb){
		Span addFiles = new Span();
		addFiles.setStyleClass("btn btn-success fileinput-button");
		
		Span icon = new Span();
		addFiles.add(icon);
		icon.setStyleClass("icon-plus icon-white");
		
		Span text = new Span();
		addFiles.add(text);
		text.add(iwrb.getLocalizedString("add_files", "Add files"));
		
		addFiles.add("<input type=\"file\" name=\"files[]\" />");
		
		return addFiles;
	}
	
	private UIComponent getStartUploadButton(IWResourceBundle iwrb){
		Span startUpload = new Span();
		startUpload.setStyleClass("btn btn-primary start");
		
		Span icon = new Span();
		startUpload.add(icon);
		icon.setStyleClass("icon-upload icon-white");
		
		Span text = new Span();
		startUpload.add(text);
		text.add(iwrb.getLocalizedString("start_upload", "Start upload"));
		
		return startUpload;
	}
	
	private UIComponent getCanceltUploadButton(IWResourceBundle iwrb){
		Span button = new Span();
		button.setStyleClass("btn btn-warning cancel");
		
		Span icon = new Span();
		button.add(icon);
		icon.setStyleClass("icon-ban-circle icon-white");
		
		Span text = new Span();
		button.add(text);
		text.add(iwrb.getLocalizedString("cancel_upload", "cancel upload"));
		
		return button;
	}
	
	private UIComponent getDeleteButton(IWResourceBundle iwrb){
		Span button = new Span();
		button.setStyleClass("btn btn-danger delete");
		
		Span icon = new Span();
		button.add(icon);
		icon.setStyleClass("icon-trash icon-white");
		
		Span text = new Span();
		button.add(text);
		text.add(iwrb.getLocalizedString("delete", "delete"));
		
		return button;
	}
	
	private Layer getProgressLayer(IWResourceBundle iwrb){
		Layer container = new Layer();
		container.setStyleClass("span5 fileupload-progress fade");
		
		Layer progress = new Layer();
		container.add(progress);
		progress.setStyleClass("progress progress-success progress-striped active");
		HashMap<String,String> markupAttributes = new HashMap<String,String>();
		markupAttributes.put("role", "progressbar");
		markupAttributes.put("aria-valuemin", "0");
		markupAttributes.put("aria-valuemax", "100");
		progress.addMarkupAttributes(markupAttributes);
		
		Layer barr = new Layer();
		progress.add(barr);
		barr.setStyleClass("bar");
		barr.setStyleAttribute("width:0%;");
		
		Layer progressExtension = new Layer();
		container.add(progressExtension);
		progressExtension.setStyleClass("progress-extended");
		progressExtension.add("&nbsp;");
		
		progress.setStyleClass("span5 fileupload-progress fade");
		return container;
	}
	
	protected Map<String, Object> getUploaderOptions(IWContext iwc){
		UploadAreaBean uploadAreaBean = getUploadAreaBean();
		HashMap<String, Object> options = new HashMap<String, Object>();
		options.put("dropZone",getDropZonesSelectionFunction());
		options.put("maxFileSize", uploadAreaBean.getMaxFileSize(iwc));
		options.put("url", uploadAreaBean.getServletPath());
		options.put("autoUpload", isAutoUpload());
		options.put("paramName", getName());
		
		HashMap<String, Object> uploadAreaOptions = new HashMap<String, Object>();
		options.put("uploadAreaOptions", uploadAreaOptions);
		uploadAreaOptions.put("checkboxNeeded", isCheckboxNeeded());
		
		
		String uploadPath = getUploadPath();
		ArrayList<Object> formData = new ArrayList<Object>();
		if(!StringUtil.isEmpty(uploadPath)){
			HashMap<String, String> uploadPathParameter = new HashMap<String, String>(2);
			uploadPathParameter.put("name", BlueimpUploadServlet.PARAMETER_UPLOAD_PATH);
			uploadPathParameter.put("value", uploadPath);
			formData.add(uploadPathParameter);
		}
		options.put("formData", formData);
		return options;
	}
	
	private boolean isCheckboxNeeded(){
		return isAddStartUploadsButton() || isAddCancelUploadsButton() || isAddDeleteUploadsButton();
	}
	private void addFileInput(IWContext iwc, IWResourceBundle iwrb){
		setStyleClass(getStyleClass());
		getScriptOnLoad().append("\n\tjQuery('#").append(getId()).append("').uploadAreaHelper(").append(new Gson().toJson(getUploaderOptions(iwc))).append(");");
		setStyleAttribute("width:100%");
		
		
		Layer inputs = new Layer();
		add(inputs);
		inputs.setStyleClass("row fileupload-buttonbar");
		
		Layer buttons = new Layer();
		inputs.add(buttons);
		buttons.setStyleClass("span7");
		
		buttons.add(getAddfilesButton(iwrb));
		
		if(isAddStartUploadsButton()){
			buttons.add(getStartUploadButton(iwrb));
		}
		if(isAddCancelUploadsButton()){
			buttons.add(getCanceltUploadButton(iwrb));
		}
		if(isAddDeleteUploadsButton()){
			buttons.add(getDeleteButton(iwrb));
		}
		if(isCheckboxNeeded()){
			CheckBox checkbox = new CheckBox();
			buttons.add(checkbox);
			checkbox.setStyleClass("toggle");
		}
		
		if(isAddUploadsProgressLayer()){
			inputs.add(getProgressLayer(iwrb));
		}
		
		Layer loadingLayer = new Layer();
		add(loadingLayer);
		loadingLayer.setStyleClass("fileupload-loading");
		
		Layer presentation = new Layer();
		presentation.setStyleAttribute("width:100%");
		add(presentation);
		presentation.add("<table role='presentation' class='table table-striped uploaded-files-table'  ><tbody class='files' data-toggle='modal-gallery' data-target='#modal-gallery'></tbody></table>");
		
		
		//Fake templates for script to load I don't know how to remove them		
		Layer fake = new Layer();
		add(fake);
		fake.setStyleAttribute("display:none");
		fake.add("<script id='template-upload' type='text/x-tmpl'>alert('fake');</script>");
		fake.add("<script id='template-download' type='text/x-tmpl'>alert('fake');</script>");
	}
	
	private void addScriptOnLoad(IWContext iwc){
		IWResourceBundle iwrb = iwc.getIWMainApplication().getBundle(ContentConstants.IW_BUNDLE_IDENTIFIER).getResourceBundle(iwc);
		Layer scriptLayer = new Layer();
		add(scriptLayer);
		StringBuilder scriptOnLoad = getScriptOnLoad();
		
		//localize
		scriptOnLoad.append("{var local = locale.fileupload;");
		scriptOnLoad.append("local.destroy = '").append(iwrb.getLocalizedString("delete", "Delete")).append(CoreConstants.JS_STR_INITIALIZATION_END);
		scriptOnLoad.append("local.cancel = '").append(iwrb.getLocalizedString("cancel", "Cancel")).append(CoreConstants.JS_STR_INITIALIZATION_END);
		scriptOnLoad.append("var errors = local.errors;");
		scriptOnLoad.append("errors.maxFileSize = '").append(iwrb.getLocalizedString("file_is_too_big", "File is too big")).append(CoreConstants.JS_STR_INITIALIZATION_END);
		scriptOnLoad.append("}");
		
		
		scriptOnLoad.append("\n});");
		scriptLayer.add(PresentationUtil.getJavaScriptAction(scriptOnLoad.toString()));
		
	}
	
	public List<String> getScriptFiles(IWContext iwc){
		List<String> scripts = new ArrayList<String>();

		scripts.add(CoreConstants.DWR_ENGINE_SCRIPT);
		scripts.add(CoreConstants.DWR_UTIL_SCRIPT);

		Web2Business web2 = WFUtil.getBeanInstance(iwc, Web2Business.SPRING_BEAN_IDENTIFIER);
		try{
			if (web2 != null) {
				JQuery  jQuery = web2.getJQuery();
				scripts.add(jQuery.getBundleURIToJQueryLib());
				scripts.add(jQuery.getBundleURIToJQueryUILib("1.8.17","js/jquery-ui-1.8.17.custom.min.js"));
				scripts.addAll(web2.getBundleUrisToBlueimpFileUploadScriptFiles("6.9.2"));
	
				
			}else{
				Logger.getLogger(UploadArea.class.getName()).log(Level.WARNING, "Failed getting Web2Business no jQuery and it's plugins files were added");
			}
		}
		catch (Exception e) {
			Logger.getLogger(UploadArea.class.getName()).log(Level.WARNING, "Failed adding scripts no jQuery and it's plugins files were added");
		}
		
		IWBundle iwb = iwc.getIWMainApplication().getBundle(ContentConstants.IW_BUNDLE_IDENTIFIER);
		scripts.add(iwb.getVirtualPathWithFileNameString("javascript/upload-area-helper.js"));

		return scripts;
	}
	
	public List<String> getStyleFiles(IWContext iwc){
		List<String> styles = new ArrayList<String>();

		Web2Business web2 = WFUtil.getBeanInstance(iwc, Web2Business.SPRING_BEAN_IDENTIFIER);
		if (web2 != null) {
			JQuery  jQuery = web2.getJQuery();
			styles.add(jQuery.getBundleURIToJQueryUILib("1.8.17","themes/smoothness/ui-1.8.17.custom.css"));
			styles.addAll(web2.getBundleUrisToBlueimpFileUploadStyleFiles("6.9.2"));
		}else{
			Logger.getLogger(UploadArea.class.getName()).log(Level.WARNING, "Failed getting Web2Business no jQuery and it's plugins files were added");
		}
		IWBundle iwb = iwc.getIWMainApplication().getBundle(ContentConstants.IW_BUNDLE_IDENTIFIER);
		styles.add(iwb.getVirtualPathWithFileNameString("style/upload-area.css"));
		
		return styles;
	}
	
	private void addFiles(IWContext iwc){
		PresentationUtil.addStyleSheetsToHeader(iwc, getStyleFiles(iwc));
		PresentationUtil.addJavaScriptSourcesLinesToHeader(iwc, getScriptFiles(iwc));
	}


	protected StringBuilder getScriptOnLoad() {
		if(scriptOnLoad == null){
			scriptOnLoad = new StringBuilder("jQuery(document).ready(function(){");
		}
		return scriptOnLoad;
	}

	protected void setScriptOnLoad(StringBuilder scriptOnLoad) {
		this.scriptOnLoad = scriptOnLoad;
	}


	public String getUploadPath() {
		return uploadPath;
	}

	public void setUploadPath(String uploadPath) {
		this.uploadPath = uploadPath;
	}

	public String getDropZonesSelectionFunction() {
		return dropZonesSelectionFunction;
	}

	public void setDropZonesSelectionFunction(String dropZonesSelectionFunction) {
		this.dropZonesSelectionFunction = dropZonesSelectionFunction;
	}

	public String getStyleClass() {
		return styleClass;
	}

	public void setStyleClass(String styleClass) {
		this.styleClass = styleClass;
	}

	public boolean isAutoUpload() {
		return autoUpload;
	}

	public void setAutoUpload(boolean autoUpload) {
		this.autoUpload = autoUpload;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}
	
//	private Layer getMainLayer(){
//		if(main == null){
//			
//		}
//		return main;
//	}
//
//	public String getIdOfMainLayer() {
//		if(id == null){
//			id = getMainLayer().getId(); 
//		}
//		return id;
//	}
//
//	public void setIdOfMainLayer(String id) {
//		getMainLayer().setId(id);
//	}
	
	@Override
	public String getId(){
		String id = super.getId();
		if(StringUtil.isEmpty(id)){
			id = generateId();
			setId(id);
		}
		return id;
	}
	
	protected String generateId()
	{
		String UUID = UUIDGenerator.getInstance().generateId();
		return "iwid" + UUID.substring(UUID.lastIndexOf("-") + 1);
	}
	
	@Override
	public void encodeBegin(FacesContext context) throws IOException {
		super.encodeBegin(context);
		ResponseWriter responseWriter = context.getResponseWriter();
		responseWriter.startElement("div", this);
		responseWriter.writeAttribute("id", this.getId(), null);
		responseWriter.writeAttribute("class", this.styleClass, null);
	}

	@Override
	public void encodeEnd(FacesContext context) throws IOException {
		ResponseWriter responseWriter = context.getResponseWriter();
		responseWriter.endElement("div");
	}

	public boolean isAddStartUploadsButton() {
		if(addStartUploadsButton != null){
			return addStartUploadsButton;
		}
		return false;
	}

	public void setAddStartUploadsButton(boolean addStartUploadsButton) {
		this.addStartUploadsButton = addStartUploadsButton;
	}

	public boolean isAddCancelUploadsButton() {
		return addCancelUploadsButton;
	}

	public void setAddCancelUploadsButton(boolean addCancelUploadsButton) {
		this.addCancelUploadsButton = addCancelUploadsButton;
	}

	public boolean isAddDeleteUploadsButton() {
		return addDeleteUploadsButton;
	}

	public void setAddDeleteUploadsButton(boolean addDeleteUploadsButton) {
		this.addDeleteUploadsButton = addDeleteUploadsButton;
	}

	public boolean isAddUploadsProgressLayer() {
		return addUploadsProgressLayer;
	}

	public void setAddUploadsProgressLayer(boolean addUploadsProgressLayer) {
		this.addUploadsProgressLayer = addUploadsProgressLayer;
	}

}

