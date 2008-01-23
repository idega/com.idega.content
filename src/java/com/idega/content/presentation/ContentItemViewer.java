/*
 * $Id: ContentItemViewer.java,v 1.30 2008/01/23 12:11:59 valdas Exp $ Created
 * on 26.1.2005
 * 
 * Copyright (C) 2005 Idega Software hf. All Rights Reserved.
 * 
 * This software is the proprietary information of Idega hf. Use is subject to
 * license terms.
 */
package com.idega.content.presentation;

import java.io.IOException;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import javax.faces.component.UICommand;
import javax.faces.component.UIComponent;
import javax.faces.component.UIOutput;
import javax.faces.component.UIParameter;
import javax.faces.component.html.HtmlGraphicImage;
import javax.faces.component.html.HtmlOutputLink;
import javax.faces.component.html.HtmlOutputText;
import javax.faces.context.FacesContext;
import javax.faces.el.ValueBinding;
import com.idega.content.bean.ContentItem;
import com.idega.content.business.ContentConstants;
import com.idega.content.business.ContentUtil;
import com.idega.core.cache.UIComponentCacher;
import com.idega.core.uri.IWActionURIManager;
import com.idega.presentation.IWContext;
import com.idega.util.CoreConstants;
import com.idega.webface.WFContainer;
import com.idega.webface.WFUtil;

/**
 * 
 * Last modified: $Date: 2008/01/23 12:11:59 $ by $Author: valdas $
 * 
 * @author <a href="mailto:gummi@idega.com">Gudmundur Agust Saemundsson</a>
 * @version $Revision: 1.30 $
 */
public class ContentItemViewer extends WFContainer {

	private Map<String, Object> fieldPrefixValueMap = new HashMap<String, Object>();
	private Map<String, Object> fieldLocalValueMap = new HashMap<String, Object>();
	private Map<String, Object> fieldSuffixValueMap = new HashMap<String, Object>();
	private Map<String, Boolean> fieldValueChangedMap = new HashMap<String, Boolean>();
	public static final String FACET_TOOLBAR = "item_toolbar";
	public static final String FACET_ITEM_HEADER = "item_header";
	public static final String FACET_ITEM_FOOTER = "item_footer";
	public static final String FACET_ITEM_DETAILS_COMMAND = "item_details_command";
	public static final String FACET_ITEM_COMMENTS = "item_comments";
	public static final String FACET_COMMENTS_SCRIPTS = "item_comments_java_scripts";
	public static final String FACET_FEED_SCRIPT = "item_feed_java_scripts";
	public static final String DEFAULT_RENDERER_TYPE = "content_item_viewer";
	public static final String FACET_JAVA_SCRIPT = "content_item_java_script";
	public static final String FACET_STYLE_SHEET = "content_item_style_sheet";
	
	/*
	 * The field requestedResourcePath is used to store the path that is
	 * requested by a http request
	 */
	//private String requestedResourcePath = null;
	/*
	 * The field resourcePath is used to store the path to the resource this
	 * instance currently renders
	 */
	private String resourcePath = null;
	private boolean showRequestedItem = true;
	private Boolean renderDetailsCommand = null;
	private ContentItem contentItemCach = null;
	private String detailsViewerPath;
	private boolean autoCreateResource=true;
	
	public ContentItemViewer() {
		super();
		this.setStyleClass("content_item_viewer");
		setRendererType(DEFAULT_RENDERER_TYPE);
	}

	public String getFamily() {
		return ContentUtil.FAMILY_CONTENT;
	}

	public String[] getViewerFieldNames() {
		ContentItem item = getContentItem();
		if (item != null) {
			return item.getContentFieldNames();
		}
		else {
			return new String[0];
		}
	}

	/**
	 * @return Returns the styleClassPrefix.
	 */
	protected String getDefaultStyleClassPrefix() {
		ContentItem item = getContentItem();
		if (item != null) {
			return item.getContentItemPrefix();
		}
		else {
			return "";
		}
	}

	protected UIComponent createPrefixComponent(String fieldName) {
		return new HtmlOutputText();
	}

	protected UIComponent createFieldComponent(String fieldName) {
		return new HtmlOutputText();
	}

	protected UIComponent getSuffixComponent(String fieldName) {
		return new HtmlOutputText();
	}

	protected boolean hasLocalValueChanged(String fieldName) {
		Boolean changed = this.fieldValueChangedMap.get(fieldName);
		if (changed != null) {
			return changed.booleanValue();
		}
		else {
			return false;
		}
	}

	protected void setLocalValueHasChanged(String fieldName, boolean value) {
		this.fieldValueChangedMap.put(fieldName, Boolean.valueOf(value));
	}

	protected void setFieldPrefixValue(String fieldName, Object value) {
		this.fieldPrefixValueMap.put(fieldName, value);
	}

	protected Object getFieldPrefixValue(String fieldName) {
		return this.fieldPrefixValueMap.get(fieldName);
	}

	protected void setFieldLocalValue(String fieldName, Object value) {
		this.fieldLocalValueMap.put(fieldName, value);
	}

	protected Object getFieldLocalValue(String fieldName) {
		return this.fieldLocalValueMap.get(fieldName);
	}

	protected void setFieldSuffixValue(String fieldName, Object value) {
		this.fieldSuffixValueMap.put(fieldName, value);
	}

	protected Object getFieldSuffixValue(String fieldName) {
		return this.fieldSuffixValueMap.get(fieldName);
	}

	protected ContentItemFieldViewer getFieldViewer(String fieldName) {
		return (ContentItemFieldViewer) getFacet(getFacetName(fieldName));
	}

	protected UIComponent getFieldViewerPrefixComponent(String fieldName) {
		return getFieldViewer(fieldName).getPrefixComponent();
	}

	protected UIComponent getFieldViewerComponent(String fieldName) {
		return getFieldViewer(fieldName).getMainComponent();
	}

	protected UIComponent getFieldViewerSuffixComponent(String fieldName) {
		return getFieldViewer(fieldName).getSuffixComponent();
	}

	public String getFacetName(String fieldName) {
		return getFacetIdPrefix() + fieldName;
	}

	/**
	 * @return Returns the facetIdPrefix.
	 */
	protected String getFacetIdPrefix() {
		return "";
	}

	/**
	 * 
	 */
	protected void componentHasBeenUpdatedWithNewValue(String fieldName) {
		setLocalValueHasChanged(fieldName, false);
	}

	protected void initializeComponent(FacesContext context) {
		String attr[] = getViewerFieldNames();
		for (int i = 0; i < attr.length; i++) {
			initializeComponent(attr[i]);
		}
		initializeToolbar();
		initializeComments(context);
	}

	/**
	 * 
	 */
	protected void initializeToolbar() {
		ContentItem item = getContentItem();
		if (item != null) {
			String[] actions = item.getToolbarActions();
			if (actions != null && actions.length > 0) {
				ContentItemToolbar toolbar = new ContentItemToolbar(this.getClass().equals(CoreConstants.getArticleItemViewerClass()));
				toolbar.setId(this.getId()+"_toolbar");
				toolbar.setToolbarActions(actions);
				this.setToolbar(toolbar);
			}
		}
	}
	
	protected void initializeComments(FacesContext context) {
		// Override and implement this method
	}

	/**
	 * This method returns the name of the attribute that should be value binded
	 * to in the component that presents the field. This is used when the
	 * component is initialized and updated. By default the binding attribute is
	 * "value" as is used by most of the components but this can be overwitten
	 * if needed.
	 * 
	 * @param fieldName
	 * @return Returns the name of the attribute that should be value binded to
	 *         in the component that presents the field.
	 */
	protected String getBindingAttribute(String fieldName) {
		return "value";
	}

	/**
	 * 
	 */
	protected void initializeComponent(String fieldName) {
		ContentItemFieldViewer div = new ContentItemFieldViewer();
		div.setPrefixClass(getPrefixStyleClass());
		div.setStyleClass(getStyleClass(fieldName));
		div.setSuffixClass(getSuffixStyleClass());
		UIComponent output = createFieldComponent(fieldName);
		UIComponent uiValueBound = getValueBindingComponent(output);
		ValueBinding binding = getValueBinding(fieldName);
		String bindingAttribute = getBindingAttribute(fieldName);
		if (binding != null) {
			uiValueBound.setValueBinding(bindingAttribute, binding);
		}
		else {
			boolean invoked = false;
			if ("value".equals(bindingAttribute)) {
				if (uiValueBound instanceof UIOutput) {
					((UIOutput) uiValueBound).setValue(getValue(fieldName));
					invoked = true;
				}
				else if (uiValueBound instanceof UICommand) {
					((UICommand) uiValueBound).setValue(getValue(fieldName));
					invoked = true;
				}
				else if (uiValueBound instanceof HtmlGraphicImage) {
					((HtmlGraphicImage) uiValueBound).setValue(getValue(fieldName));
					invoked = true;
				}
			}
			else if ("url".equals(bindingAttribute)) {
				if (uiValueBound instanceof HtmlGraphicImage) {
					((HtmlGraphicImage) uiValueBound).setUrl((String) getValue(fieldName));
					invoked = true;
				}
			}
			if (!invoked) {
				// TODO use reflection
				throw new UnsupportedOperationException("Not implemented yet");
			}
			componentHasBeenUpdatedWithNewValue(fieldName);
		}
		div.setValueComponent(output);
		getFacets().put(getFacetName(fieldName), div);
	}
/**
	 * <p>
	 * TODO tryggvil describe method getValueBindingComponent
	 * </p>
	 * @param output
	 * @return
	 */
	private UIComponent getValueBindingComponent(UIComponent output) {
		if(output instanceof HtmlOutputLink){
			//This is the case when a headline is a link
			//then we want the child HtmlOutputText to be valuebound
			return (UIComponent) output.getChildren().get(0);
		}
		else{
			return output;
		}
	}


	/**
	 * 
	 */
	protected void setViewerFacet(String facetName, UIComponent output) {
		ContentItemFieldViewer div = new ContentItemFieldViewer();
		div.setPrefixClass(getPrefixStyleClass());
		div.setStyleClass(getViewerFacetStyleClass(facetName));
		div.setSuffixClass(getSuffixStyleClass());
		div.setValueComponent(output);
		getFacets().put(facetName, div);
	}

	public ContentItemFieldViewer getViewerFacetWrapper(String facetName) {
		return (ContentItemFieldViewer) getFacet(facetName);
	}

	public UIComponent getViewerFacet(String facetName) {
		ContentItemFieldViewer v = getViewerFacetWrapper(facetName);
		if (v != null) {
			return v.getMainComponent();
		}
		else {
			return null;
		}
	}

	/**
	 * @param fieldName
	 * @return
	 */
	protected String getViewerFacetStyleClass(String facetName) {
		return getViwerFacetStyleClassPrefix() + facetName;
	}

	/**
	 * @return
	 */
	protected String getViwerFacetStyleClassPrefix() {
		return "";
	}

	/**
	 * @param fieldName
	 * @return
	 */
	protected String getStyleClass(String fieldName) {
		if (ContentConstants.ATTRIBUTE_HEADLINE.equals(fieldName)) {
			return ContentConstants.ARTICLE_ITEM_HEADLINE_STYLE_CLASS;
		}
		if (ContentConstants.ATTRIBUTE_CREATION_DATE.equals(fieldName)) {
			return ContentConstants.ARTICLE_ITEM_DATE_STYLE_CLASS;
		}
		if (ContentConstants.ATTRIBUTE_BODY.equals(fieldName)) {
			return ContentConstants.ARTICLE_ITEM_BODY_STYLE_CLASS;
		}
		
		return getDefaultStyleClassPrefix() + fieldName;
	}

	/**
	 * @param fieldName
	 * @return
	 */
	protected boolean isLocalValueSet(String fieldName) {
		return this.fieldLocalValueMap.containsKey(fieldName);
	}

	/**
	 * To value bind to this use the value of fieldName variable. For instance,
	 * getHeadline() in some subclass would be implemented as: public String
	 * getHeadline(){return (String)getValue("headline");} and the value binding
	 * will be to "headline" and can be used in tag classes to for value
	 * bindings in jsf pages.
	 * 
	 * @return Returns the author.
	 */
	public Object getValue(String fieldName) {
		if (isLocalValueSet(fieldName)) {
			return getFieldLocalValue(fieldName);
		}
		ContentItem item = getContentItem();
		if (item != null) {
			return item.getValue(fieldName);
		}
		ValueBinding vb = getValueBinding(fieldName);
		return vb != null ? (Object) vb.getValue(getFacesContext()) : null;
	}

	public void setValue(String fieldName, Object value) {
		if (value != null) {
			setLocalValueHasChanged(fieldName, (value.equals(getFieldLocalValue(fieldName))));
		}
		else {
			setLocalValueHasChanged(fieldName, (getFieldLocalValue(fieldName) != null));
		}
		setFieldLocalValue(fieldName, value);
	}

	public boolean isRendered() {
		/*ContentItem item = getContentItem();
		if (item != null) {
			if(isAutoCreateResource()){
				return true;
			}
			else{
				Boolean renderd = item.getRendered();
				if (renderd != null) {
					return renderd.booleanValue();
				}
			}
		}
		return super.isRendered();*/
		return true;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see javax.faces.component.UIComponent#decode(javax.faces.context.FacesContext)
	 */
	public void decode(FacesContext context) {
		// TODO USE DECODE RATHER THAN ENCODEBEGIN! not working because->NEVER
		// CALLED!
		if (this.showRequestedItem) {
			IWContext iwc = IWContext.getIWContext(context);
			String paramResourcePath = iwc.getParameter(ContentViewer.PARAMETER_CONTENT_RESOURCE);
			if (paramResourcePath != null) {
				setResourcePath(paramResourcePath);
			}
		}
		updateValues();
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see javax.faces.component.UIComponent#encodeBegin(javax.faces.context.FacesContext)
	 */
	public void encodeBegin(FacesContext context) throws IOException {
		
		UIComponentCacher cacher = getCacher(context);
		/*if(cacher.existsInCache(this,context)){
			// do nothing:
		}
		else{
			if(cacher.isCacheEnbled(this,context)){
				cacher.beginCache(this,context);
			}*/
			if (this.showRequestedItem) {
				IWContext iwc = IWContext.getIWContext(context);
				String paramResourcePath = iwc.getParameter(ContentViewer.PARAMETER_CONTENT_RESOURCE);
				if(paramResourcePath!=null){
					setResourcePath(paramResourcePath);
				}
			}
			
			
			super.encodeBegin(context);
			if(cacher.isCacheEnbled(this,context)){
				if(!cacher.existsInCache(this,context)){
					updateValues();
				}
			}
			else{
				updateValues();
			}
			
		/*}*/
		
	}

	public void encodeChildren(FacesContext context) throws IOException {
		/*UIComponentCacher cacher = getCacher(context);
		if(cacher.existsInCache(this,context)){
			//do nothing
		}
		else{
			super.encodeChildren(context);
		}*/
		super.encodeChildren(context);
	}
	
	public void encodeEnd(FacesContext context) throws IOException {
		/*UIComponentCacher cacher = getCacher(context);
		if(cacher.existsInCache(this,context)){
			cacher.encodeCached(this,context);
		}
		else{
			super.encodeEnd(context);
			if(cacher.isCacheEnbled(this,context)){
				cacher.endCache(this,context);
			}
		}*/
		super.encodeEnd(context);
	}

	/**
	 * 
	 */
	protected void updateValues() {
		String attr[] = getViewerFieldNames();
		for (int i = 0; i < attr.length; i++) {
			updateComponentValue(attr[i]);
		}
		updateComments();
	}

	/**
	 * 
	 */
	protected void updateComponentValue(String fieldName) {
		ContentItem item = getContentItem();
		if (item != null || hasLocalValueChanged(fieldName)) {
			UIComponent component = getFacet(getFacetName(fieldName));
			if (component != null) {
				if (component instanceof ContentItemFieldViewer) {
					UIComponent main = ((ContentItemFieldViewer) component).getMainComponent();
					if(main instanceof HtmlOutputLink){
						//this is then the headline is a link
						updateDetailsLink(main);
					}
					UIComponent value = getValueBindingComponent(main);
					String bindingAttribute = getBindingAttribute(fieldName);
					boolean invoked = false;
					if ("value".equals(bindingAttribute)) {
						if (value instanceof UIOutput) {
							Object setValue = getValue(fieldName);
							((UIOutput) value).setValue(setValue);
							invoked = true;
						}
						else if (value instanceof UICommand) {
							((UICommand) value).setValue(getValue(fieldName));
							invoked = true;
						}
						else if (value instanceof HtmlGraphicImage) {
							((HtmlGraphicImage) value).setValue(getValue(fieldName));
							invoked = true;
						}
					}
					else if ("url".equals(bindingAttribute)) {
						if (value instanceof HtmlGraphicImage) {
							((HtmlGraphicImage) value).setUrl((String) getValue(fieldName));
							invoked = true;
						}
					}
					if (!invoked) {
						// TODO use reflection
						throw new UnsupportedOperationException("Not implemented yet");
					}
				}
				else if (component instanceof UIOutput) {
					((UIOutput) component).setValue(getValue(fieldName));
				}
				else if (component instanceof UICommand) {
					((UICommand) component).setValue(getValue(fieldName));
				}
				else {
					// TODO Use reflection and getBindingAttribute(fieldName)
					throw new UnsupportedOperationException("Not implemented yet");
				}
			}
		}
		updateDetailsCommand();
		updateToolbar();
	}

	public void processDecodes(FacesContext context) {
		super.processDecodes(context);
		// if(showRequestedItem){
		// IWContext iwc = IWContext.getIWContext(context);
		// String resourcePath = iwc.getParameter(PARAMETER_CONTENT_RESOURCE);
		// if(resourcePath!=null){
		// requestedResourcePath = resourcePath;
		// }
		// }
		ContentItem item = getContentItem();
		if (item != null) {
			String attr[] = getViewerFieldNames();
			for (int i = 0; i < attr.length; i++) {
				item.setValue(attr[i], getValue(attr[i]));
			}
			this.resourcePath = item.getResourcePath();
		}
	}

	/**
	 * @return
	 */
	public ContentItem getContentItem() {
		if (this.resourcePath != null) {
			if (this.contentItemCach == null) {
				this.contentItemCach = loadContentItem(this.resourcePath);
			}
			return this.contentItemCach;
		}
		ValueBinding vb = getValueBinding("contentItem");
		return (ContentItem) (vb != null ? (Object) vb.getValue(getFacesContext()) : null);
	}

	/**
	 * This method should return an instance of ContentItem which represents the
	 * resource that the itemResourcePath parameter points to. This is
	 * implemented to return null by default but should overwritten in
	 * subclasses to activate the functionality to show a content item that is
	 * defined by parameter 'iwcontent' in the request.
	 * 
	 * @param itemResourcePath
	 * @return
	 */
	public ContentItem loadContentItem(String itemResourcePath) {
		System.out.println("[WARNING]["
				+ this.getClass().getName()
				+ "]: method loadContentItem(String itemResourcePath) is not implemented but is required when rendering content spcified by request parameter '"
				+ ContentViewer.PARAMETER_CONTENT_RESOURCE + "'.");
		return null;
	}

	public void setContentItemValueBinding(String fieldName) {
		WFUtil.setValueBinding(this, "contentItem", fieldName);
		// Might need to add value binding between #getValue(String) and the
		// FieldComponent get/set-Value() somehow for editable elements to
		// update
	}

	/**
	 * This method decides if this viewer should hande http requests that
	 * request it to show a spesific content item.
	 * 
	 * @param value
	 */
	public void setShowRequestedItem(boolean value) {
		this.showRequestedItem = value;
	}
	public void setShowRequestedItem(Boolean value) {
		setShowRequestedItem(value.booleanValue());
	}

	public boolean getShowRequestedItem() {
		return this.showRequestedItem;
	}

	public void setRenderDetailsCommand(Boolean value) {
		this.renderDetailsCommand = value;
	}

	public void setRenderDetailsCommand(boolean value) {
		this.renderDetailsCommand = Boolean.valueOf(value);
	}

	public boolean getRenderDetailsCommand() {
		if (this.renderDetailsCommand != null) {
			return this.renderDetailsCommand.booleanValue();
		}
		ValueBinding vb = getValueBinding("renderDetailsCommand");
		Boolean render = (Boolean) (vb != null ? (Object) vb.getValue(getFacesContext()) : null);
		if (render != null) {
			return render.booleanValue();
		}
		else {
			return false;
		}
	}

	/**
	 * This sets the UIComponent that should handle the action to go to the
	 * details view. This command does not have to have a parameter with the
	 * name ContentItemViewer#PARAMETER_CONTENT_RESOURCE since this object adds
	 * it if it is missing and updates it otherwise. The value that will be used
	 * is ContentItemViewer#getResourcePath() and can be value binded using
	 * 'resourcePath'.
	 * 
	 * @param command
	 */
	public void setDetailsCommand(UIComponent command) {
		setViewerFacet(FACET_ITEM_DETAILS_COMMAND, command);
	}

	public void updateDetailsCommand() {
		UIComponent command = getViewerFacet(FACET_ITEM_DETAILS_COMMAND);
		updateDetailsLink(command);
	}

	public void updateDetailsLink(UIComponent link) {
		if (link != null) {
			boolean updated = false;
			List childrens = link.getChildren();
			for (Iterator iter = childrens.iterator(); iter.hasNext();) {
				UIComponent component = (UIComponent) iter.next();
				if (component instanceof UIParameter) {
					UIParameter parameter = (UIParameter) component;
					if (ContentViewer.PARAMETER_CONTENT_RESOURCE.equals(parameter.getName())) {
						parameter.setValue(getResourcePath());
						updated = true;
					}
				}
			}
			if (!updated) {
				UIParameter parameter = new UIParameter();
				parameter.setName(ContentViewer.PARAMETER_CONTENT_RESOURCE);
				parameter.setValue(getResourcePath());
				childrens.add(parameter);
			}
		}
	}

	/**
	 * 
	 */
	public void updateToolbar() {
		ContentItemToolbar toolbar = getToolbar();
		if (toolbar != null) {
			toolbar.setResourcePath(getResourcePath());
		}
	}
	
	protected void updateComments() {
		// Override and implement this method
	}

	public void setFooter(UIComponent footer) {
		setViewerFacet(FACET_ITEM_FOOTER, footer);
	}

	public void setHeader(UIComponent header) {
		setViewerFacet(FACET_ITEM_HEADER, header);
	}

	public void setToolbar(ContentItemToolbar toolbar) {
		// setViewerFacet(FACET_TOOLBAR, toolbar);
		getFacets().put(FACET_TOOLBAR, toolbar);
	}

	public ContentItemToolbar getToolbar() {
		// ContentItemToolbar bar = getViewerFacet(FACET_TOOLBAR);
		ContentItemToolbar bar = (ContentItemToolbar) getFacets().get(FACET_TOOLBAR);
		return bar;
	}

	/**
	 * @see javax.faces.component.UIComponentBase#saveState(javax.faces.context.FacesContext)
	 */
	public Object saveState(FacesContext ctx) {
		Object values[] = new Object[10];
		values[0] = super.saveState(ctx);
		values[1] = this.fieldPrefixValueMap;
		values[2] = this.fieldLocalValueMap;
		values[3] = this.fieldSuffixValueMap;
		values[4] = this.fieldValueChangedMap;
		values[5] = Boolean.valueOf(this.showRequestedItem);
		values[6] = this.renderDetailsCommand;
		values[7] = this.resourcePath;
		values[8] = this.detailsViewerPath;
		values[9] = Boolean.valueOf(this.autoCreateResource);
		return values;
	}

	/**
	 * @see javax.faces.component.UIComponentBase#restoreState(javax.faces.context.FacesContext,
	 *      java.lang.Object)
	 */
	public void restoreState(FacesContext ctx, Object state) {
		Object values[] = (Object[]) state;
		super.restoreState(ctx, values[0]);
		this.fieldPrefixValueMap = (Map<String, Object>) values[1];
		this.fieldLocalValueMap = (Map<String, Object>) values[2];
		this.fieldSuffixValueMap = (Map<String, Object>) values[3];
		this.fieldValueChangedMap = (Map<String, Boolean>) values[4];
		this.showRequestedItem = ((Boolean) values[5]).booleanValue();
		this.renderDetailsCommand = (Boolean) values[6];
		this.resourcePath = (String) values[7];
		this.detailsViewerPath=(String)values[8];
		this.autoCreateResource=((Boolean)values[9]).booleanValue();
	}

	public String getPrefixStyleClass() {
		return "prefix";
	}

	public String getSuffixStyleClass() {
		return "suffix";
	}

	/**
	 * @return Returns the resourcePath.
	 */
	public String getResourcePath() {
		if (this.resourcePath != null) {
			return this.resourcePath;
		}
		ContentItem item = getContentItem();
		if (item != null) {
			return item.getResourcePath();
		}
		ValueBinding vb = getValueBinding("resourcePath");
		return vb != null ? (String) vb.getValue(getFacesContext()) : null;
	}

	/**
	 * This method just sets the information about the resource path but does
	 * not load other information according to it.
	 * 
	 * @param resourcePath
	 *            The resourcePath to set.
	 */
	public void setResourcePath(String resourcePath) {
		this.resourcePath = resourcePath;
	}

	public String[] getToolbarActions() {
		ContentItem item = getContentItem();
		if (item != null) {
			return item.getToolbarActions();
		}
		else {
			return null;
		}
	}

	public String getActionURIPath(String action) {
		return IWActionURIManager.getInstance().getActionURIPrefixWithContext(action, getResourcePath());
	}
	
	public HtmlOutputLink getEmptyMoreLink(){
		HtmlOutputLink moreLink = new HtmlOutputLink();
		IWContext iwc = IWContext.getInstance();
		
		String appContext = iwc.getIWMainApplication().getApplicationContextURI();
		if (appContext.endsWith("/")){
			appContext = appContext.substring(0, appContext.lastIndexOf("/"));			
		}
		moreLink.setValue(appContext+getDetailsViewerPath());
		return moreLink;
	}
	
	public void setDetailsViewerPath(String path){
		this.detailsViewerPath=path;
	}
	
	public String getDetailsViewerPath(){
		return this.detailsViewerPath;
	}

	
	/**
	 * @return Returns the autoCreateResource.
	 */
	public boolean isAutoCreateResource() {
		return this.autoCreateResource;
	}

	
	/**
	 * @param autoCreateResource The autoCreateResource to set.
	 */
	public void setAutoCreateResource(boolean autoCreateResource) {
		this.autoCreateResource = autoCreateResource;
	}
	

	/* (non-Javadoc)
	 * @see com.idega.core.cache.CacheableUIComponent#getViewState(javax.faces.context.FacesContext)
	 */
	public String getViewState(FacesContext context) {
		IWContext iwc = IWContext.getIWContext(context);
		StringBuffer state = new StringBuffer();
		if(ContentUtil.hasContentEditorRoles(iwc)){
			state.append("edit");
		}
		else{
			state.append("view");
		}
		if (this.showRequestedItem) {
			String resourceUrl = iwc.getParameter(ContentViewer.PARAMETER_CONTENT_RESOURCE);
			if(resourceUrl!=null){//&&showRequestedItem){
				state.append(resourceUrl);
			}
		}
		if (this.detailsViewerPath!=null) {
			state.append(this.detailsViewerPath);
		}
		if (this.resourcePath!=null) {
			state.append(this.resourcePath);
		}
		return state.toString();
	}

}
