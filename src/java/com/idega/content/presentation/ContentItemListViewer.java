/*
 * $Id: ContentItemListViewer.java,v 1.1 2005/02/07 10:59:41 gummi Exp $
 * Created on 27.1.2005
 *
 * Copyright (C) 2005 Idega Software hf. All Rights Reserved.
 *
 * This software is the proprietary information of Idega hf.
 * Use is subject to license terms.
 */
package com.idega.content.presentation;

import java.io.IOException;
import java.util.List;
import java.util.ListIterator;
import javax.faces.component.UIColumn;
import javax.faces.component.UIData;
import javax.faces.context.FacesContext;
import javax.faces.el.ValueBinding;
import com.idega.content.bean.ContentItem;
import com.idega.webface.WFUtil;
import com.idega.webface.model.WFDataModel;


/**
 * 
 *  Last modified: $Date: 2005/02/07 10:59:41 $ by $Author: gummi $
 * 
 * @author <a href="mailto:gummi@idega.com">Gudmundur Agust Saemundsson</a>
 * @version $Revision: 1.1 $
 */
public class ContentItemListViewer extends UIData {

	private String managedBeanId;
	private String resourcePath;

	private String _styleClass;
	private String _style;
	private String _columnClasses;
	private String _rowClasses;
	
	private WFDataModel model=null;
	
	private static final String DEFAULT_RENDERER_TYPE = "content_list_viewer";

	/**
	 * 
	 */
	public ContentItemListViewer() {
		super();
		setRendererType(DEFAULT_RENDERER_TYPE);
	}	
	
	/**
	 * Constructs a new WFViewerList component with the specified list managed bean as data source.
	 */
	public ContentItemListViewer(String managedBeanId) {
		this();
		setBeanIdentifier(managedBeanId);
	}
	
	/**
	 * This method is for initalization only but is available e.g. for use in jsp pages.
	 * 
	 * @param managedBeanId
	 */
	public void setBeanIdentifier(String managedBeanId) {
		this.managedBeanId = managedBeanId;
		String var = managedBeanId + "_var";
		setVar(var);
		
		ContentItemViewer viewer = (ContentItemViewer)WFUtil.invoke(this.managedBeanId,"getContentViewer");
		addContentItemViewer(viewer);
		
		List attachementViewers = (List)WFUtil.invoke(this.managedBeanId,"getAttachmentViewers");
		if(attachementViewers!=null){
			for (ListIterator iter = attachementViewers.listIterator(); iter.hasNext();) {
				ContentItemViewer attachmentViewer = (ContentItemViewer) iter.next();
				int index = iter.nextIndex();
				addAttachmentViewer(attachmentViewer,index);
			}
		}
		
		notifyManagedBeanOfResourcePath(this.resourcePath);
	}
	
	public void setResourcePath(String path){
		this.resourcePath=path;
		notifyManagedBeanOfResourcePath(path);
	}

	protected void addContentItemViewer(ContentItemViewer viewer){
		UIColumn c = new UIColumn();
		viewer.setContentItemValueBinding(getVar()+".contentItem");  //binded with ContentItemBindingBean#getContentItem()
		c.getChildren().add(viewer);
		this.getChildren().add(c);
	}
	
	protected void addAttachmentViewer(ContentItemViewer viewer, int index){
		UIColumn c = new UIColumn();
		viewer.setContentItemValueBinding(getVar()+".attachedments["+index+"]");  //binded with ContentItemBindingBean#getContentItem()
		c.getChildren().add(viewer);
		this.getChildren().add(c);
	}
	
	public Object getValue(){
		if(model==null){
			List items = (List)WFUtil.invoke(this.managedBeanId,"getContentItems");
			if(items!=null){
				model = new WFDataModel();
				for (ListIterator iter = items.listIterator(); iter.hasNext();) {
					int index = iter.nextIndex();
					ContentItem item = (ContentItem) iter.next();
					ContentItemBindingBean bean = new ContentItemBindingBean(item);
					model.set(bean,index);
				}
				return model;
			}
			return super.getValue();
		}
		return model;
	}
	
	public void encodeBegin(FacesContext context) throws IOException{
		super.encodeBegin(context);
	}
	
	public void encodeChildren(FacesContext context) throws IOException{
		super.encodeChildren(context);
	}
	
	public void encodeEnd(FacesContext context) throws IOException{
		super.encodeEnd(context);
	}
	
	public String getFamily(){
		return "iw_content";
	}
	
	public String getDefultStyleClass(){
		return "content_list";
	}
	
	public String getDefultRowClass(){
		if(getChildCount() > 1){
			return "content_list_item";
		} else {
			return null;
		}
		
	}
	
	public void setStyle(String style)
    {
        _style = style;
    }

    public String getStyle()
    {
        if (_style != null) return _style;
        ValueBinding vb = getValueBinding("style");
        return vb != null ? (String)vb.getValue(getFacesContext()) : null;
    }

    public void setStyleClass(String styleClass)
    {
        _styleClass = styleClass;
    }

    public String getStyleClass()
    {
        if (_styleClass != null) return _styleClass;
        ValueBinding vb = getValueBinding("styleClass");
        String sClass = vb != null ? (String)vb.getValue(getFacesContext()) : null;
        return (sClass != null)? sClass : getDefultStyleClass(); 
    }
	
	public void setColumnClasses(String columnClasses)
    {
        _columnClasses = columnClasses;
    }

    public String getColumnClasses()
    {
        if (_columnClasses != null) return _columnClasses;
        ValueBinding vb = getValueBinding("columnClasses");
        return vb != null ? (String)vb.getValue(getFacesContext()) : null;
    }
    
    public void setRowClasses(String rowClasses)
    {
        _rowClasses = rowClasses;
    }

    public String getRowClasses()
    {
        if (_rowClasses != null) return _rowClasses;
        ValueBinding vb = getValueBinding("rowClasses");
        String sClass = vb != null ? (String)vb.getValue(getFacesContext()) : null;
        return (sClass != null)? sClass : getDefultRowClass();
    }
	
	/**
	 * @see javax.faces.component.StateHolder#saveState(javax.faces.context.FacesContext)
	 */
	public Object saveState(FacesContext ctx) {
		Object values[] = new Object[7];
		values[0] = super.saveState(ctx);
		values[1] = this.managedBeanId;
		values[2] = this.resourcePath;
		values[3] = _styleClass;
		values[4] = _style;
		values[5] = _columnClasses;
		values[6] = _rowClasses;
		return values;
	}
	
	/**
	 * @see javax.faces.component.StatHolder#restoreState(javax.faces.context.FacesContext, java.lang.Object)
	 */
	public void restoreState(FacesContext ctx, Object state) {
		Object values[] = (Object[])state;
		super.restoreState(ctx, values[0]);
		this.managedBeanId = (String) values[1];
		this.resourcePath = (String) values[2];
		this._styleClass = (String) values[3];
		this._style = (String) values[4];
		this._columnClasses = (String) values[5];
		this._rowClasses = (String) values[6];
		
		notifyManagedBeanOfResourcePath(resourcePath);
		
	}

	/**
	 * @param resourcePath
	 */
	private void notifyManagedBeanOfResourcePath(String resourcePath) {
		if(this.managedBeanId!=null){
			WFUtil.invoke(this.managedBeanId,"setResourcePath",resourcePath,String.class);
		}
	}

	public class ContentItemBindingBean {
		
		private ContentItem item;
		
		public ContentItemBindingBean(ContentItem item){
			this.item = item;
		}
		
		public ContentItem getContentItem(){
			return item;
		}
		
		public void setContentItem(Object obj){
			//does nothing
		}
		
		public List getAttachments(){
			return item.getAttachments();
		}
		
		public void setAttachments(){
			//does nothing
		}
		
	}
	
}
