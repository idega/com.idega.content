package com.idega.content.upload.presentation;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;

import com.google.gson.Gson;
import com.idega.block.web2.business.JQuery;
import com.idega.block.web2.business.Web2Business;
import com.idega.content.business.ContentConstants;
import com.idega.content.upload.business.UploadAreaBean;
import com.idega.content.upload.servlet.BlueimpUploadServlet;
import com.idega.idegaweb.IWBundle;
import com.idega.idegaweb.IWResourceBundle;
import com.idega.presentation.IWBaseComponent;
import com.idega.presentation.IWContext;
import com.idega.presentation.Layer;
import com.idega.presentation.Span;
import com.idega.presentation.ui.CheckBox;
import com.idega.presentation.ui.HiddenInput;
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
	
	private String id = null;
	
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
		options.put("autoUpload", getAutoUpload());
		options.put("paramName", getName());
		return options;
	}
	private void addFileInput(IWContext iwc, IWResourceBundle iwrb){
//		Layer container = new Layer();
//		container.setStyleAttribute("width:100%");
//		add(container);
//		container.setStyleClass("uploader-container");
		
		Layer uploaderForm = new Layer();
		uploaderForm.setId(getId());
		uploaderForm.setStyleClass(getStyleClass());
		getScriptOnLoad().append("\n\tjQuery('#").append(uploaderForm.getId()).append("').uploadAreaHelper(").append(new Gson().toJson(getUploaderOptions(iwc))).append(");");
		uploaderForm.setStyleAttribute("width:100%");
		add(uploaderForm);
		
		String uploadPath = getUploadPath();
		if(!StringUtil.isEmpty(uploadPath)){
			HiddenInput uploadPathInput = new HiddenInput(BlueimpUploadServlet.PARAMETER_UPLOAD_PATH, uploadPath);
			uploaderForm.add(uploadPathInput);
		}
		
		Layer inputs = new Layer();
		uploaderForm.add(inputs);
		inputs.setStyleClass("row fileupload-buttonbar");
		
		Layer buttons = new Layer();
		inputs.add(buttons);
		buttons.setStyleClass("span7");
		
		buttons.add(getAddfilesButton(iwrb));
		buttons.add(getStartUploadButton(iwrb));
		buttons.add(getCanceltUploadButton(iwrb));
		buttons.add(getDeleteButton(iwrb));
		
		CheckBox checkbox = new CheckBox();
		buttons.add(checkbox);
		checkbox.setStyleClass("toggle");
		
		inputs.add(getProgressLayer(iwrb));
		
		Layer loadingLayer = new Layer();
		uploaderForm.add(loadingLayer);
		loadingLayer.setStyleClass("fileupload-loading");
		
		Layer presentation = new Layer();
		presentation.setStyleAttribute("width:100%");
		uploaderForm.add(presentation);
		presentation.add("<table role='presentation' class='table table-striped'  ><tbody class='files' data-toggle='modal-gallery' data-target='#modal-gallery'></tbody></table>");
		
		
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
		scriptOnLoad.append("var errors = locale.fileupload.errors;");
		scriptOnLoad.append("errors.maxFileSize = '").append(iwrb.getLocalizedString("file_is_too_big", "File is too big")).append(CoreConstants.JS_STR_INITIALIZATION_END);
		
		
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

	public boolean getAutoUpload() {
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

	@Override
	public String getId() {
		if(id == null){
			Layer layer = new Layer();
			id = layer.getId() + "id"; 
		}
		return id;
	}

	@Override
	public void setId(String id) {
		this.id = id;
	}

}

