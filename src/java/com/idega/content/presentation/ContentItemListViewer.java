/*
 * $Id: ContentItemListViewer.java,v 1.25 2008/02/21 08:57:47 laddi Exp $
 * Created on 27.1.2005
 *
 * Copyright (C) 2005 Idega Software hf. All Rights Reserved.
 *
 * This software is the proprietary information of Idega hf.
 * Use is subject to license terms.
 */
package com.idega.content.presentation;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;
import java.util.StringTokenizer;

import javax.faces.component.UIColumn;
import javax.faces.component.UIData;
import javax.faces.context.FacesContext;
import javax.faces.el.ValueBinding;
import javax.servlet.http.HttpServletRequest;

import com.idega.content.bean.ContentItem;
import com.idega.content.bean.ContentListViewerManagedBean;
import com.idega.content.business.ContentUtil;
import com.idega.content.business.categories.CategoryBean;
import com.idega.core.cache.CacheableUIComponent;
import com.idega.core.cache.UIComponentCacher;
import com.idega.presentation.IWContext;
import com.idega.webface.WFUtil;
import com.idega.webface.model.WFDataModel;


/**
 * 
 * Last modified: $Date: 2008/02/21 08:57:47 $ by $Author: laddi $
 * 
 * @author <a href="mailto:gummi@idega.com">Gudmundur Agust Saemundsson</a>
 * @version $Revision: 1.25 $
 */
public class ContentItemListViewer extends UIData implements CacheableUIComponent{

	private String managedBeanId;
	private String resourcePath;
	private String detailsViewerPath;

	private String _styleClass;
	private String _style;
	private String _columnClasses;
	private String _rowClasses;
	
	private List<String> categoriesList = null;
	private WFDataModel model=null;
	private String firstArticleItemStyleClass = null;
	private boolean initialized = false;
	
	private static final String DEFAULT_RENDERER_TYPE = "content_list_viewer";
	private int maxNumberOfDisplayed=-1;
	
	public static final String ITEMS_CATEGORY_VIEW = "items_list_category_view";
	
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

		//notifyManagedBeanOfVariableValues();
	}
	
	protected String[] getToolbarActions(){
		return new String[] {"create"};
	}
	
	protected void initializeInEncodeBegin(){
		
		notifyManagedBeanOfVariableValues();
		ContentListViewerManagedBean bean = getManagedBean();
		ContentItemViewer viewer = bean.getContentViewer();//WFUtil.invoke(this.managedBeanId,"getContentViewer");
		viewer.setShowRequestedItem(false);
		//viewer.setHeadlineAsLink(getHeadlineAsLink());
		addContentItemViewer(viewer);
		
		String[] actions = getToolbarActions();
		if(actions != null && actions.length > 0){
			ContentItemToolbar toolbar = new ContentItemToolbar(true);
			toolbar.setMenuStyleClass(toolbar.getMenuStyleClass() + " " + toolbar.getMenuStyleClass() + "_top");
			for (int i = 0; i < actions.length; i++) {
				toolbar.addToolbarButton(actions[i]);
			}
			String categories = this.getCategories();
			if(categories!=null){
				toolbar.setCategories(categories);
			}
			String basePath = getBaseFolderPath();
			if(basePath!=null){
				toolbar.setBaseFolderPath(basePath);
			}
			
			//toolbar.setResourcePath(getResourcePath());
			toolbar.setActionHandlerIdentifier(bean.getIWActionURIHandlerIdentifier());//WFUtil.invoke(this.managedBeanId,"getIWActionURIHandlerIdentifier"));
			this.setHeader(toolbar);
		}
		
		List attachementViewers = bean.getAttachmentViewers();//WFUtil.invoke(this.managedBeanId,"getAttachmentViewers");
		if(attachementViewers!=null){
			for (ListIterator iter = attachementViewers.listIterator(); iter.hasNext();) {
				ContentItemViewer attachmentViewer = (ContentItemViewer) iter.next();
				int index = iter.nextIndex();
				addAttachmentViewer(attachmentViewer,index);
			}
		}
		this.initialized = true;
	}
	
	
	@Override
	public String getFamily(){
		return ContentUtil.FAMILY_CONTENT;
	}
	
	/**
	 * @deprecated replaced with setBaseFolderPath
	 */
	@Deprecated
	public void setResourcePath(String resourcePath){
		setBaseFolderPath(resourcePath);
	}
	
	public void setBaseFolderPath(String path){
		this.resourcePath=path;
		notifyManagedBeanOfBaseFolderPath(path);
	}
	
	public String getBaseFolderPath(){
		if (this.resourcePath != null) {
			return this.resourcePath;
		}
        ValueBinding vb = getValueBinding("baseFolderPath");
        String path = vb != null ? (String)vb.getValue(getFacesContext()) : null;
        if(path==null){
	        	if(this.managedBeanId!=null){
	        		//path = (String)WFUtil.invoke(this.managedBeanId,"getResourcePath");
	        		path = getManagedBean().getBaseFolderPath();
	        	}
        }
        return path;
	}

	protected void addContentItemViewer(ContentItemViewer viewer){
		UIColumn c = new UIColumn();
		viewer.setContentItemValueBinding(getVar()+".contentItem");  //binded with ContentItemBindingBean#getContentItem()
		WFUtil.setValueBinding(viewer,"rendered",getVar()+".rendered");
		c.getChildren().add(viewer);
		this.getChildren().add(c);
	}
	
	protected void addAttachmentViewer(ContentItemViewer viewer, int index){
		UIColumn c = new UIColumn();
		viewer.setContentItemValueBinding(getVar()+".attachedments["+index+"]");  //binded with ContentItemBindingBean#getContentItem()
		c.getChildren().add(viewer);
		this.getChildren().add(c);
	}
	
	@Override
	public Object getValue(){
		if(this.model==null){
			//List items = (List)WFUtil.invoke(this.managedBeanId,"getContentItems");
			List items = getManagedBean().getContentItems();
			if(items!=null){
				this.model = new WFDataModel();
				for (ListIterator iter = items.listIterator(); iter.hasNext();) {
					int index = iter.nextIndex();
					ContentItem item = (ContentItem) iter.next();
					ContentItemBindingBean bean = new ContentItemBindingBean(item);
					this.model.set(bean,index);
				}
				return this.model;
			}
			return super.getValue();
		}
		return this.model;
	}
	
	@Override
	public void encodeBegin(FacesContext context) throws IOException{
		UIComponentCacher cacher = getCacher(context);
		setItemCategoryFromRequest(context);
		if(cacher.existsInCache(this,context)){
			// do nothing:
		}
		else{
			if(cacher.isCacheEnbled(this,context)){
				cacher.beginCache(this,context);
			}
			
			if(!this.initialized){
				initializeInEncodeBegin();
			}
			super.encodeBegin(context);
		}
	}
	
	@Override
	public void encodeChildren(FacesContext context) throws IOException{
		UIComponentCacher cacher = getCacher(context);
		if(cacher.existsInCache(this,context)){
			// do nothing:
		}
		else{
			super.encodeChildren(context);
		}	
	}
	
	@Override
	public void encodeEnd(FacesContext context) throws IOException{
		UIComponentCacher cacher = getCacher(context);
		if(cacher.existsInCache(this,context)){
			cacher.encodeCached(this,context);
		}
		else{
			super.encodeEnd(context);
			if(cacher.isCacheEnbled(this,context)){
				cacher.endCache(this,context);
			}
		}
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
        this._style = style;
    }

    public String getStyle()
    {
        if (this._style != null) {
					return this._style;
				}
        ValueBinding vb = getValueBinding("style");
        return vb != null ? (String)vb.getValue(getFacesContext()) : null;
    }

    public void setStyleClass(String styleClass)
    {
        this._styleClass = styleClass;
    }

    public String getStyleClass()
    {
        if (this._styleClass != null) {
					return this._styleClass;
				}
        ValueBinding vb = getValueBinding("styleClass");
        String sClass = vb != null ? (String)vb.getValue(getFacesContext()) : null;
        return (sClass != null)? sClass : getDefultStyleClass(); 
    }
	
	public void setColumnClasses(String columnClasses)
    {
        this._columnClasses = columnClasses;
    }

    public String getColumnClasses()
    {
        if (this._columnClasses != null) {
					return this._columnClasses;
				}
        ValueBinding vb = getValueBinding("columnClasses");
        return vb != null ? (String)vb.getValue(getFacesContext()) : null;
    }
    
    public void setRowClasses(String rowClasses)
    {
        this._rowClasses = rowClasses;
    }

    public String getRowClasses()
    {
        if (this._rowClasses != null) {
					return this._rowClasses;
				}
        ValueBinding vb = getValueBinding("rowClasses");
        String sClass = vb != null ? (String)vb.getValue(getFacesContext()) : null;
        return (sClass != null)? sClass : getDefultRowClass();
    }
	
	/**
	 * @see javax.faces.component.StateHolder#saveState(javax.faces.context.FacesContext)
	 */
	@Override
	public Object saveState(FacesContext ctx) {
		Object values[] = new Object[11];
		values[0] = super.saveState(ctx);
		values[1] = this.managedBeanId;
		values[2] = this.resourcePath;
		values[3] = this._styleClass;
		values[4] = this._style;
		values[5] = this._columnClasses;
		values[6] = this._rowClasses;
		values[7] = this.detailsViewerPath;
		values[8] = Boolean.valueOf(this.initialized);
		values[9] = this.categoriesList;
		values[10] = new Integer(this.maxNumberOfDisplayed);
		return values;
	}
	
	/**
	 * @see javax.faces.component.StatHolder#restoreState(javax.faces.context.FacesContext, java.lang.Object)
	 */
	@Override
	public void restoreState(FacesContext ctx, Object state) {
		Object values[] = (Object[])state;
		super.restoreState(ctx, values[0]);
		this.managedBeanId = (String) values[1];
		this.resourcePath = (String) values[2];
		this._styleClass = (String) values[3];
		this._style = (String) values[4];
		this._columnClasses = (String) values[5];
		this._rowClasses = (String) values[6];
		this.detailsViewerPath = (String)values[7];
		this.initialized = ((Boolean)values[8]).booleanValue();
		this.categoriesList = (List<String>) values[9];
		this.maxNumberOfDisplayed=((Integer)values[10]).intValue();
		notifyManagedBeanOfVariableValues();
		
	}
	
	protected void notifyManagedBeanOfVariableValues(){
		notifyManagedBeanOfBaseFolderPath(this.resourcePath);
		notifyManagedBeanOfDetailsViewerPath(this.detailsViewerPath);
		notifyManagedBeanOfCategories(this.categoriesList);
		int maxItems = getMaxNumberOfDisplayed();
		if(maxItems!=-1){
			getManagedBean().setMaxNumberOfDisplayed(maxItems);
		}
	}

	/**
	 * @param resourcePath
	 */
	private void notifyManagedBeanOfBaseFolderPath(String resourcePath) {
		if(this.managedBeanId!=null){
			//WFUtil.invoke(this.managedBeanId,"setResourcePath",resourcePath,String.class);
			getManagedBean().setBaseFolderPath(resourcePath);
		}
	}
	
	/**
	 * @param resourcePath
	 */
	private void notifyManagedBeanOfDetailsViewerPath(String path) {
		if(this.managedBeanId!=null){
			//WFUtil.invoke(this.managedBeanId,"setDetailsViewerPath",path,String.class);
			getManagedBean().setDetailsViewerPath(this.detailsViewerPath);
		}
	}
	
	/**
	 * @param resourcePath
	 */
	private void notifyManagedBeanOfCategories(List categories) {
		if(this.managedBeanId!=null){
			//WFUtil.invoke(this.managedBeanId,"setCategories",categoriesList,List.class);
			getManagedBean().setCategories(categories);
		}
	}

	public class ContentItemBindingBean {
		
		private ContentItem item;
		
		public ContentItemBindingBean(ContentItem item){
			this.item = item;
		}
		
		public ContentItem getContentItem(){
			return this.item;
		}
		
		public void setContentItem(Object obj){
			//does nothing
		}
		
		public void setAttachments(){
			//does nothing
		}
		
	}
	
	/**
	 * @return Returns the detailsViewerPath.
	 */
	public String getDetailsViewerPath() {
		return this.detailsViewerPath;
	}
	/**
	 * @param detailsViewerPath The path to set.
	 */
	public void setDetailsViewerPath(String path) {
		this.detailsViewerPath = path;
		notifyManagedBeanOfDetailsViewerPath(this.detailsViewerPath);
	}
	/**
	 * @return Returns the categoriesList.
	 */
	public List getCategoriesList() {
		return this.categoriesList;
	}
	/**
	 * <p>
	 * Sets the categoriesList as a comma separated list
	 * </p>
	 * @param categoriesList
	 */
	public void setCategories(String categories){
		if(categories!=null){
			List<String> cats = new ArrayList<String>();
			StringTokenizer tokenizer = new StringTokenizer(categories, CategoryBean.CATEGORY_DELIMETER);
			while(tokenizer.hasMoreTokens()){
				cats.add(tokenizer.nextToken().trim());
			}
			List<String> cats2 = (cats.isEmpty())?null:cats;
			setCategoriesList(cats2);
		} else {	
			//null
		}
	}
	
	public String getCategories() {
		if(this.categoriesList!=null){
			Iterator iter = this.categoriesList.iterator();
			if(iter.hasNext()){
				StringBuffer catString = new StringBuffer();
				catString.append(iter.next());
				while(iter.hasNext()){
					catString.append(CategoryBean.CATEGORY_DELIMETER);
					catString.append(iter.next());
				}
				return catString.toString();
			}
		}
		return null;
	}
	
	
	/**
	 * @param categoriesList The categoriesList to set.
	 */
	public void setCategoriesList(List<String> categories) {
		this.categoriesList = categories;
		notifyManagedBeanOfCategories(categories);
	}

	public String getFirstArticleItemStyleClass() {
		return this.firstArticleItemStyleClass;
	}

	public void setFirstArticleItemStyleClass(String firstArticleItemStyleClass) {
		this.firstArticleItemStyleClass = firstArticleItemStyleClass;
	}
	
	public ContentListViewerManagedBean getManagedBean(){
		ContentListViewerManagedBean bean = (ContentListViewerManagedBean) WFUtil.getBeanInstance(this.managedBeanId);
		return bean;
	}

	
	/**
	 * @return Returns the maxNumberOfItems.
	 */
	public int getMaxNumberOfDisplayed() {
		return this.maxNumberOfDisplayed;
	}

	
	/**
	 * @param maxNumberOfItems The maxNumberOfItems to set.
	 */
	public void setMaxNumberOfDisplayed(int maxNumberOfItems) {
		this.maxNumberOfDisplayed = maxNumberOfItems;
	}


	public UIComponentCacher getCacher(FacesContext context){
		return UIComponentCacher.getDefaultCacher(context);
	}

	/* (non-Javadoc)
	 * @see com.idega.core.cache.CacheableUIComponent#getViewState(javax.faces.context.FacesContext)
	 */
	public String getViewState(FacesContext context) {
		IWContext iwc = IWContext.getIWContext(context);
		StringBuffer buf = new StringBuffer();
		if(ContentUtil.hasContentEditorRoles(iwc)){
			buf.append("edit");
		}
		else{
			buf.append("view");
		}
		String categories = this.getCategories();
		if(categories!=null){
			buf.append(UIComponentCacher.UNDERSCORE);
			buf.append(categories);
		}
		buf.append(UIComponentCacher.UNDERSCORE);
		buf.append(this.maxNumberOfDisplayed);
		if(this.detailsViewerPath!=null){
			buf.append(UIComponentCacher.UNDERSCORE);
			buf.append(this.detailsViewerPath);
		}
		if(this._columnClasses!=null){
			buf.append(UIComponentCacher.UNDERSCORE);
			buf.append(this._columnClasses);
		}
		if(this._style!=null){
			buf.append(UIComponentCacher.UNDERSCORE);
			buf.append(this._style);
		}
		if(this._styleClass!=null){
			buf.append(UIComponentCacher.UNDERSCORE);
			buf.append(this._styleClass);
		}
		if(this.firstArticleItemStyleClass!=null){
			buf.append(UIComponentCacher.UNDERSCORE);
			buf.append(this.firstArticleItemStyleClass);
		}
		if(this.resourcePath!=null){
			buf.append(UIComponentCacher.UNDERSCORE);
			buf.append(this.resourcePath);
		}
		return buf.toString();
	}
	
	private void setItemCategoryFromRequest(FacesContext context) {
		if (context == null) {
			return;
		}
		HttpServletRequest request = (HttpServletRequest) context.getExternalContext().getRequest();
		String category = request.getParameter(ITEMS_CATEGORY_VIEW);
		if (category != null) { // Just to be sure not overriding (maybe) existing category
			setCategories(category);
		}
	}
}
