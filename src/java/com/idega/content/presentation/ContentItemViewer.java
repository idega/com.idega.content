/*
 * $Id: ContentItemViewer.java,v 1.1 2005/02/07 10:59:41 gummi Exp $
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
import java.util.Map;
import javax.faces.component.UIComponent;
import javax.faces.component.UIOutput;
import javax.faces.component.html.HtmlOutputText;
import javax.faces.context.FacesContext;
import javax.faces.el.ValueBinding;
import com.idega.content.bean.ContentItem;
import com.idega.util.RenderUtils;
import com.idega.webface.WFContainer;
import com.idega.webface.WFUtil;


/**
 * 
 *  Last modified: $Date: 2005/02/07 10:59:41 $ by $Author: gummi $
 * 
 * @author <a href="mailto:gummi@idega.com">Gudmundur Agust Saemundsson</a>
 * @version $Revision: 1.1 $
 */
public class ContentItemViewer extends WFContainer {
	
	private Map fieldPrefixValueMap = new HashMap();
	private Map fieldLocalValueMap = new HashMap();
	private Map fieldSuffixValueMap = new HashMap();
	private Map fieldValueChangedMap = new HashMap();
	
	public ContentItemViewer() {
		this.setStyleClass("wf_content_item_viewer");
	}
	
	protected String[] getViewerAttributes(){
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
	
	protected UIOutput createPrefixComponent(String fieldName){
		return new HtmlOutputText();
	}
	
	protected UIOutput createUIOutputComponent(String fieldName){
		return new HtmlOutputText();
	}
	
	protected UIOutput getSuffixComponent(String fieldName){
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
	
	protected UIOutput getFieldViewerPrefixComponent(String fieldName){
		return getFieldViewer(fieldName).getPrefixComponent();
	}
	
	protected UIOutput getFieldViewerComponent(String fieldName){
		return getFieldViewer(fieldName).getMainComponent();
	}
	
	protected UIOutput getFieldViewerSuffixComponent(String fieldName){
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
		String attr[] = getViewerAttributes();
		for (int i = 0; i < attr.length; i++) {
			initializeComponent(attr[i]);
		}
	}

	/**
	 * @return
	 */
	private void initializeComponent(String fieldName) {
		ContentItemFieldViewer div = new ContentItemFieldViewer();
		div.setPrefixClass(getPrefixStyleClass());
		div.setStyleClass(getStyleClass(fieldName));
		div.setSuffixClass(getSuffixStyleClass());
		UIOutput output = createUIOutputComponent(fieldName);
		ValueBinding binding = getValueBinding(fieldName);
		if(binding != null){
			output.setValueBinding("value",binding);
		} else {
			output.setValue(getValue(fieldName));
			componentHasBeenUpdatedWithNewValue(fieldName);
		}
		div.setValueComponent(output);
		getFacets().put(getFacetName(fieldName), div);
	}



	/**
	 * @param fieldName
	 * @return
	 */
	private String getStyleClass(String fieldName) {
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
		super.encodeBegin(context);
		updateValues();
	}
	
	/**
	 * 
	 */
	protected void updateValues() {
		String attr[] = getViewerAttributes();
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
					}
				} else if(component instanceof UIOutput){
					((UIOutput)component).setValue(getValue(fieldName));
				}
			}
		}
	}

	
	/* (non-Javadoc)
	 * @see javax.faces.component.UIComponent#encodeChildren(javax.faces.context.FacesContext)
	 */
	public void encodeChildren(FacesContext context) throws IOException {
		super.encodeChildren(context);
		String attr[] = getViewerAttributes();
		for (int i = 0; i < attr.length; i++) {
			RenderUtils.renderFacet(context,this,getFacetName(attr[i]));
		}
	}

	public void processDecodes(FacesContext context){
		super.processDecodes(context);
		ContentItem item = getContentItem();
		if(item != null){ 
			String attr[] = getViewerAttributes();
			for (int i = 0; i < attr.length; i++) {
				item.setValue(attr[i],getValue(attr[i]));
			}
		}
	}

	/**
	 * @return
	 */
	public ContentItem getContentItem() {
		ValueBinding vb = getValueBinding("contentItem");
		return (ContentItem) (vb != null ? (Object)vb.getValue(getFacesContext()) : null);
	}
	
	public void setContentItemValueBinding(String fieldName){
		WFUtil.setValueBinding(this,"contentItem",fieldName);
		//Might need to add value binding between #getValue(String) and the 
		//FieldComponent get/set-Value() somehow for editable elements to update
	}

	/**
	 * @see javax.faces.component.UIComponentBase#saveState(javax.faces.context.FacesContext)
	 */
	public Object saveState(FacesContext ctx) {
		Object values[] = new Object[5];
		values[0] = super.saveState(ctx);
		values[1] = fieldPrefixValueMap;
		values[2] = fieldLocalValueMap;
		values[3] = fieldSuffixValueMap;
		values[4] = fieldValueChangedMap;
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
	}
	
	public String getPrefixStyleClass() {
		return "prefix";
	}
	
	public String getSuffixStyleClass() {
		return "suffix";
	}

}
