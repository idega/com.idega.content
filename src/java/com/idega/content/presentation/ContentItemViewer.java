/*
 * $Id: ContentItemViewer.java,v 1.2 2005/02/21 16:12:45 gummi Exp $
 * Created on 26.1.2005
 *
 * Copyright (C) 2005 Idega Software hf. All Rights Reserved.
 *
 * This software is the proprietary information of Idega hf.
 * Use is subject to license terms.
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
import javax.faces.component.html.HtmlOutputText;
import javax.faces.context.FacesContext;
import javax.faces.el.ValueBinding;
import com.idega.content.bean.ContentItem;
import com.idega.content.business.ContentUtil;
import com.idega.presentation.IWContext;
import com.idega.webface.WFContainer;
import com.idega.webface.WFUtil;


/**
 * 
 *  Last modified: $Date: 2005/02/21 16:12:45 $ by $Author: gummi $
 * 
 * @author <a href="mailto:gummi@idega.com">Gudmundur Agust Saemundsson</a>
 * @version $Revision: 1.2 $
 */
public class ContentItemViewer extends WFContainer {
	
	private Map fieldPrefixValueMap = new HashMap();
	private Map fieldLocalValueMap = new HashMap();
	private Map fieldSuffixValueMap = new HashMap();
	private Map fieldValueChangedMap = new HashMap();
	
	public static final String PARAMETER_CONTENT_RESOURCE = "iwcontent";
	
	public static final String FACET_TOOLBAR = "item_toolbar";
	public static final String FACET_ITEM_HEADER = "item_header";
	public static final String FACET_ITEM_FOOTER = "item_footer";
	public static final String FACET_ITEM_DETAILS_COMMAND = "item_details_command";
	
	public static final String DEFAULT_RENDERER_TYPE = "content_item_viewer";
	
	/*
	 * The field requestedResourcePath is used to store the path that is requested by a http request
	 */
	private String requestedResourcePath = null;
	
	/*
	 * The field resourcePath is used to store the path to the resource this instance currently renders 
	 */
	private String resourcePath = null;
	
	private boolean showRequestedItem = true;
	
	private Boolean renderDetailsCommand = null;
	
	private ContentItem contentItemCach = null;
	
	
	public ContentItemViewer() {
		super();
		this.setStyleClass("content_item_viewer");
		setRendererType(DEFAULT_RENDERER_TYPE);
	}
	
	public String getFamily(){
		return ContentUtil.FAMILY_CONTENT;
	}
	
	public String[] getViewerFieldNames(){
		ContentItem item = getContentItem();
		if(item!=null){
			return item.getContentFieldNames();
		} else {
			return new String[0];
		}
	}
	
	/**
	 * @return Returns the styleClassPrefix.
	 */
	protected String getDefaultStyleClassPrefix() {
		ContentItem item = getContentItem();
		if(item!=null){
			return item.getContentItemPrefix();
		} else {
			return "";
		}
	}
	
	protected UIComponent createPrefixComponent(String fieldName){
		return new HtmlOutputText();
	}
	
	protected UIComponent createFieldComponent(String fieldName){
		return new HtmlOutputText();
	}
	
	protected UIComponent getSuffixComponent(String fieldName){
		return new HtmlOutputText();
	}
	
	protected boolean hasLocalValueChanged(String fieldName){
		Boolean changed = (Boolean)fieldValueChangedMap.get(fieldName);
		if(changed != null){
			return changed.booleanValue();
		} else {
			return false;
		}
	}
	
	protected void setLocalValueHasChanged(String fieldName, boolean value){
		fieldValueChangedMap.put(fieldName,Boolean.valueOf(value));
	}
	
	protected void setFieldPrefixValue(String fieldName, Object value){
		fieldPrefixValueMap.put(fieldName,value);
	}
	
	protected Object getFieldPrefixValue(String fieldName){
		return fieldPrefixValueMap.get(fieldName);
	}
	
	protected void setFieldLocalValue(String fieldName, Object value){
		fieldLocalValueMap.put(fieldName,value);
	}
	
	protected Object getFieldLocalValue(String fieldName){
		return fieldLocalValueMap.get(fieldName);
	}
	
	protected void setFieldSuffixValue(String fieldName, Object value){
		fieldSuffixValueMap.put(fieldName,value);
	}
	
	protected Object getFieldSuffixValue(String fieldName){
		return fieldSuffixValueMap.get(fieldName);
	}
	
	protected ContentItemFieldViewer getFieldViewer(String fieldName){
		return (ContentItemFieldViewer)getFacet(getFacetName(fieldName));
	}
	
	protected UIComponent getFieldViewerPrefixComponent(String fieldName){
		return getFieldViewer(fieldName).getPrefixComponent();
	}
	
	protected UIComponent getFieldViewerComponent(String fieldName){
		return getFieldViewer(fieldName).getMainComponent();
	}
	
	protected UIComponent getFieldViewerSuffixComponent(String fieldName){
		return getFieldViewer(fieldName).getSuffixComponent();
	}
	
	
	public String getFacetName(String fieldName){
		return getFacetIdPrefix()+fieldName;
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
		setLocalValueHasChanged(fieldName,false);
	}
	
	protected void initializeContent() {		
		String attr[] = getViewerFieldNames();
		for (int i = 0; i < attr.length; i++) {
			initializeComponent(attr[i]);
		}
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
		ValueBinding binding = getValueBinding(fieldName);
		if(binding != null){
			output.setValueBinding("value",binding);
		} else {
			if(output instanceof UIOutput) {
				((UIOutput)output).setValue(getValue(fieldName));
			} else if(output instanceof UICommand){
				((UICommand)output).setValue(getValue(fieldName));
			}
			componentHasBeenUpdatedWithNewValue(fieldName);
		}
		div.setValueComponent(output);
		getFacets().put(getFacetName(fieldName), div);
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
	
	public ContentItemFieldViewer getViewerFacetWrapper(String facetName){
		return (ContentItemFieldViewer)getFacet(facetName);
	}

	public UIComponent getViewerFacet(String facetName){
		ContentItemFieldViewer v = getViewerFacetWrapper(facetName);
		if(v!=null){
			return v.getMainComponent();
		} else {
			return null;
		}
	}

	
	/**
	 * @param fieldName
	 * @return
	 */
	protected String getViewerFacetStyleClass(String facetName) {
		return getViwerFacetStyleClassPrefix()+facetName;
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
		return getDefaultStyleClassPrefix()+fieldName;
	}

	/**
	 * @param fieldName
	 * @return
	 */
	protected boolean isLocalValueSet(String fieldName) {
		return fieldLocalValueMap.containsKey(fieldName);
	}

	/**
	 * @return Returns the author.
	 */
	public Object getValue(String fieldName) {
		if (isLocalValueSet(fieldName)) {
			return getFieldLocalValue(fieldName);
		}
		ContentItem item = getContentItem();
		if (item != null){
			return item.getValue(fieldName);
		}
		ValueBinding vb = getValueBinding(fieldName);
		return (String) (vb != null ? (Object)vb.getValue(getFacesContext()) : null);
	}	
	

	/**
	 * @param author The author to set.
	 */
	public void setValue(String fieldName, Object value) {
		if(value!=null){
			setLocalValueHasChanged(fieldName, (value.equals(getFieldLocalValue(fieldName))));
		} else {
			setLocalValueHasChanged(fieldName, (getFieldLocalValue(fieldName) != null));
		}
		setFieldLocalValue(fieldName,value);
	}
	
	
	/* (non-Javadoc)
	 * @see javax.faces.component.UIComponent#encodeBegin(javax.faces.context.FacesContext)
	 */
	public void encodeBegin(FacesContext context) throws IOException {
			
		if(showRequestedItem){
			IWContext iwc = IWContext.getIWContext(context);
			String resourcePath = iwc.getParameter(PARAMETER_CONTENT_RESOURCE);
			if(resourcePath!=null){
				requestedResourcePath = resourcePath;
			}
		}
		
		super.encodeBegin(context);
		updateValues();
		
	}
	
	/**
	 * 
	 */
	protected void updateValues() {
		String attr[] = getViewerFieldNames();
		for (int i = 0; i < attr.length; i++) {
			updateComponentValue(attr[i]);
		}
		
	}
	
	/**
	 * 
	 */
	protected void updateComponentValue(String fieldName) {
		ContentItem item = getContentItem();
		if(item != null || hasLocalValueChanged(fieldName)){
			UIComponent component = getFacet(getFacetName(fieldName));
			if(component != null){
				if(component instanceof ContentItemFieldViewer){
					UIComponent value = ((ContentItemFieldViewer)component).getMainComponent();
					if(value instanceof UIOutput){
						((UIOutput)value).setValue(getValue(fieldName));
					} else if(value instanceof UICommand){
						((UICommand)value).setValue(getValue(fieldName));
					}
				} else if(component instanceof UIOutput){
					((UIOutput)component).setValue(getValue(fieldName));
				} else  if(component instanceof UICommand){
					((UICommand)component).setValue(getValue(fieldName));
				}
			}
		}
		updateDetailsCommand();
	}

	public void processDecodes(FacesContext context){
		super.processDecodes(context);
		
//		if(showRequestedItem){
//			IWContext iwc = IWContext.getIWContext(context);
//			String resourcePath = iwc.getParameter(PARAMETER_CONTENT_RESOURCE);
//			if(resourcePath!=null){
//				requestedResourcePath = resourcePath;
//			}
//		}
		
		ContentItem item = getContentItem();
		if(item != null){ 
			String attr[] = getViewerFieldNames();
			for (int i = 0; i < attr.length; i++) {
				item.setValue(attr[i],getValue(attr[i]));
			}
			resourcePath = item.getResourcePath();
		}
	}

	/**
	 * @return
	 */
	public ContentItem getContentItem() {
		if(requestedResourcePath != null && showRequestedItem){
			if(contentItemCach==null){
				contentItemCach = loadContentItem(requestedResourcePath);
			}
			return contentItemCach;
		}
		ValueBinding vb = getValueBinding("contentItem");
		return (ContentItem) (vb != null ? (Object)vb.getValue(getFacesContext()) : null);
	}
	
	/**
	 * This method should return an instance of ContentItem which represents the resource that the 
	 * itemResourcePath parameter points to.  This is implemented to return null by default but should
	 * overwritten in subclasses to activate the functionality to show a content item that is defined by
	 * parameter 'iwcontent' in the request.
	 * 
	 * @param itemResourcePath
	 * @return
	 */
	public ContentItem loadContentItem(String itemResourcePath) {
		System.out.println("[WARNING]["+this.getClass().getName()+"]: method loadContentItem(String itemResourcePath) is not implemented but is required when rendering content spcified by request parameter '"+PARAMETER_CONTENT_RESOURCE+"'.");
		return null;
	}

	public void setContentItemValueBinding(String fieldName){
		WFUtil.setValueBinding(this,"contentItem",fieldName);
		//Might need to add value binding between #getValue(String) and the 
		//FieldComponent get/set-Value() somehow for editable elements to update
	}
	
	
	/**
	 * This method decides if this viewer should hande http requests that request it to show a spesific content item. 
	 * @param value
	 */
	public void setShowRequestedItem(boolean value){
		showRequestedItem = value;
	}
	
	
	public void setRenderDetailsCommand(boolean value){
		renderDetailsCommand = Boolean.valueOf(value);
	}
	
	public boolean getRenderDetailsCommand(){
		if(renderDetailsCommand != null){
			return renderDetailsCommand.booleanValue();
		}
		ValueBinding vb = getValueBinding("renderDetailsCommand");
		Boolean render =  (Boolean) (vb != null ? (Object)vb.getValue(getFacesContext()) : null);
		if(render!= null){
			return render.booleanValue();
		} else {
			return false;
		}
	}
	
	
	/**
	 * This sets the UIComponent that should handle the action to go to the details view.  This
	 * command does not have to have a parameter with the name ContentItemViewer#PARAMETER_CONTENT_RESOURCE
	 * since this object adds it if it is missing and updates it otherwise.  The value that will be
	 * used is ContentItemViewer#getResourcePath() and can be value binded using 'resourcePath'.
	 * @param command
	 */
	public void setDetailsCommand(UIComponent command){
		setViewerFacet(FACET_ITEM_DETAILS_COMMAND,command);
	}
	
	public void updateDetailsCommand(){
		UIComponent command = (UIComponent)getViewerFacet(FACET_ITEM_DETAILS_COMMAND);
		if(command!=null){
			boolean updated = false;
			List childrens = command.getChildren();
			for (Iterator iter = childrens.iterator(); iter.hasNext();) {
				UIComponent component = (UIComponent) iter.next();
				if(component instanceof UIParameter){
					UIParameter parameter = (UIParameter)component;
					if(PARAMETER_CONTENT_RESOURCE.equals(parameter.getName())){
						parameter.setValue(getResourcePath());
						updated = true;
					}
				}
			}
			
			if(!updated){
				UIParameter parameter = new UIParameter();
				parameter.setName(PARAMETER_CONTENT_RESOURCE);
				parameter.setValue(getResourcePath());
				childrens.add(parameter);
			}
		}
	}
	
	public void setFooter(UIComponent footer){
		setViewerFacet(FACET_ITEM_FOOTER, footer);
    }

    public void setHeader(UIComponent header){
    		setViewerFacet(FACET_ITEM_HEADER, header);
    }
    
    public void setToolbar(ContentItemToolbar toolbar){
    		setViewerFacet(FACET_TOOLBAR, toolbar);
    }
	
	

	/**
	 * @see javax.faces.component.UIComponentBase#saveState(javax.faces.context.FacesContext)
	 */
	public Object saveState(FacesContext ctx) {
		Object values[] = new Object[9];
		values[0] = super.saveState(ctx);
		values[1] = fieldPrefixValueMap;
		values[2] = fieldLocalValueMap;
		values[3] = fieldSuffixValueMap;
		values[4] = fieldValueChangedMap;
		values[5] = requestedResourcePath;
		values[6] = Boolean.valueOf(showRequestedItem);
		values[7] = renderDetailsCommand;
		values[8] = resourcePath;
		return values;
	}
	
	/**
	 * @see javax.faces.component.UIComponentBase#restoreState(javax.faces.context.FacesContext, java.lang.Object)
	 */
	public void restoreState(FacesContext ctx, Object state) {
		Object values[] = (Object[])state;
		super.restoreState(ctx, values[0]);
		fieldPrefixValueMap = (Map)values[1];
		fieldLocalValueMap = (Map)values[2];
		fieldSuffixValueMap = (Map)values[3];
		fieldValueChangedMap = (Map)values[4];
		requestedResourcePath = (String)values[5];
		showRequestedItem = ((Boolean)values[6]).booleanValue();
		renderDetailsCommand = (Boolean)values[7];
		resourcePath = (String)values[8];
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
        if (resourcePath != null) return resourcePath;
        
        ContentItem item = getContentItem();
		if (item != null){
			return item.getResourcePath();
		}
        
        ValueBinding vb = getValueBinding("resourcePath");
        return vb != null ? (String)vb.getValue(getFacesContext()) : null;
	}
	/**
	 * This method just sets the information about the resource path but does not load other information according to it.
	 * 
	 * @param resourcePath The resourcePath to set.
	 */
	public void setResourcePath(String resourcePath) {
		this.resourcePath = resourcePath;
	}
}
