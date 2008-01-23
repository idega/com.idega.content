/*
 * $Id: ContentItemToolbar.java,v 1.15 2008/01/23 12:11:59 valdas Exp $
 * Created on 18.2.2005
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
import java.util.Map;
import java.util.Set;

import javax.faces.component.UIParameter;
import javax.faces.component.html.HtmlOutputLink;
import javax.faces.context.FacesContext;
import javax.faces.el.ValueBinding;

import com.idega.content.business.ContentConstants;
import com.idega.content.business.ContentUtil;
import com.idega.core.accesscontrol.business.AccessController;
import com.idega.core.uri.IWActionURIManager;
import com.idega.presentation.IWContext;
import com.idega.util.CoreUtil;
import com.idega.util.URLUtil;
import com.idega.webface.WFToolbar;


/**
 *  <p>
 *  Toolbar used by new content management system to display editor buttons.
 *  </p>
 *  Last modified: $Date: 2008/01/23 12:11:59 $ by $Author: valdas $
 * 
 * @author <a href="mailto:gummi@idega.com">Gudmundur Agust Saemundsson</a>
 * @version $Revision: 1.15 $
 */
public class ContentItemToolbar extends WFToolbar {
	
	//static constants:
	public final static String PARAMETER_CATEGORIES="content_categories";
	public final static String PARAMETER_BASE_FOLDER_PATH="content_base_path";
	
	//instance variables
	private String resourcePath;
	private String actionHandlerIdentifier;
	private IWActionURIManager manager = null;
	
	private Map<String, String> actions;
	private String[] RolesAllowded = null;
	private Boolean rendered;
	private String categories;
	private String baseFolderPath;
	private Boolean addMoodalBoxRel = false;
	
	/**
	 * 
	 */
	public ContentItemToolbar() {
		super();
		this.setMenuStyleClass("content_item_toolbar");
	}
	
	public ContentItemToolbar(boolean addMoodalBoxRel) {
		this();
		this.addMoodalBoxRel = addMoodalBoxRel;
	}
	
	public IWActionURIManager getIWActionURIManager(){
		if(this.manager==null){
			this.manager = IWActionURIManager.getInstance();
		}
		return this.manager;
	}
	
	private String getUri(String url) {
		if (url == null) {
			return null;
		}
		IWContext iwc = CoreUtil.getIWContext();
		if (iwc == null) {
			return url;
		}
		
			
		Object renderingAttribute = iwc.getSessionAttribute(ContentConstants.RENDERING_COMPONENT_OF_ARTICLE_LIST);
		if (renderingAttribute instanceof Boolean) {
			URLUtil urlUtil = new URLUtil(url);
			urlUtil.addParameter(ContentConstants.RENDERING_COMPONENT_OF_ARTICLE_LIST, ((Boolean) renderingAttribute).toString());
			
			url = urlUtil.toString();
		}
		
		return url;
	}
	
	public HtmlOutputLink addToolbarButton(String action, String url, String menuItemId) {
		url = getUri(url);
		
		ContentItemToolbarButton link = new ContentItemToolbarButton();
		link.setValue(url);
		link.setId(menuItemId);
		addToActionMap(action,menuItemId);
		this.setMenuItem(menuItemId,link);
		
		String title = action;
		link.setResourcePath(getResourcePath());
		link.setAction(action);
		link.setStyleClass(action);
		link.setTitle(title);
		
		if (addMoodalBoxRel && (action.equals(ContentConstants.CONTENT_ITEM_ACTION_EDIT) || action.equals(ContentConstants.CONTENT_ITEM_ACTION_CREATE))) {
			link.setRel("moodalbox");
		}
		
		return link;
	}
	
	public HtmlOutputLink addToolbarButton(String action, String menuItemId){
		String url = getActionURL(action,getResourcePath(),getActionHandlerIdentifier());
		return addToolbarButton(action,url,menuItemId);
	}
	
	public HtmlOutputLink addToolbarButton(String action){
		String url = getActionURL(action, getResourcePath(), getActionHandlerIdentifier());
		String menuItemId = getNextMenuItemId();
		return addToolbarButton(action, url, menuItemId);
	}
	
	protected String getActionURL(String action, String resourcePath,String handlerIdentifier){
		String url = getIWActionURIManager().getActionURIPrefixWithContext(action, resourcePath, handlerIdentifier);
		return url;
	}
	
	public HtmlOutputLink getToolbarButton(String action){
		String menuItemId = (String)getActions().get(action);
		return (HtmlOutputLink)getMenuItem(menuItemId);
	}
	
	public void clear(){
		getChildren().clear();
		getFacets().clear();
		getActions().clear();
	}
	
	
	public void update(){
		Set<String> s = getActions().keySet();
		for (Iterator<String> iter = s.iterator(); iter.hasNext();) {
			String action = iter.next();
			ContentItemToolbarButton link = (ContentItemToolbarButton)getToolbarButton(action);
			if(link==null){
				addToolbarButton(action);
			} else {
				link.setValue(getUri(getActionURL(action,getResourcePath(),getActionHandlerIdentifier())));
				
				link.setResourcePath(getResourcePath());
				String categories = this.getCategories();
				if(categories!=null){
					UIParameter categoriesParameter = new UIParameter();
					categoriesParameter.setName(PARAMETER_CATEGORIES);
					categoriesParameter.setValue(categories);
					link.getChildren().add(categoriesParameter);
				}
				String basePath = this.getBaseFolderPath();
				if(basePath!=null){
					UIParameter basePathParameter = new UIParameter();
					basePathParameter.setName(PARAMETER_BASE_FOLDER_PATH);
					basePathParameter.setValue(basePath);
					link.getChildren().add(basePathParameter);
				}
				link.setAction(action);
			}
		}
	}
	
	public void setToolbarActions(String[] actions){
		clear();
		for (int i = 0; i < actions.length; i++) {
			addToActionMap(actions[i],null);
		}
	}
	
	public Map<String, String> getActions(){
		if(this.actions==null){
			this.actions=new HashMap<String, String>();
		}
		return this.actions;
	}
	
	
	protected void addToActionMap(String action, String menuItemId){
		getActions().put(action,menuItemId);
	}
	
	/**
	 * @return Returns the resourcePath.
	 */
	public String getResourcePath() {
		return this.resourcePath;
	}
	/**
	 * @param resourcePath The resourcePath to set.
	 */
	public void setResourcePath(String resourcePath) {
		this.resourcePath = resourcePath;
	}
	
	public void encodeBegin(FacesContext context) throws IOException {
		update();
		super.encodeBegin(context);
	}
	
	public String[] getRolesAllowed(){
		return this.RolesAllowded;
	}
	
	public boolean isRendered(){
		if (this.rendered != null) {
			return this.rendered.booleanValue();
		}
        ValueBinding vb = getValueBinding("rendered");
        Boolean v = vb != null ? (Boolean)vb.getValue(getFacesContext()) : null;
        if(v==null){
        		IWContext iwc = IWContext.getInstance();
        		if(this.RolesAllowded==null){
        			return ContentUtil.hasContentEditorRoles(iwc);
        		}
        		else{
	        		AccessController ac = iwc.getAccessController();
	        		for (int i = 0; i < this.RolesAllowded.length; i++) {
					if(ac.hasRole(this.RolesAllowded[i],iwc)){
						return true;
					}
				}
        		}
        } else {
        		return v.booleanValue();
        }
        return false; //true when testing, then probably false
	}
	
	public void setRendered(boolean value){
		this.rendered = Boolean.valueOf(value);
	}
	
	/**
	 * @return Returns the actionHandlerIdentifier.
	 */
	public String getActionHandlerIdentifier() {
		return this.actionHandlerIdentifier;
	}
	/**
	 * @param handlerIdentifier The actionHandlerIdentifier to set.
	 */
	public void setActionHandlerIdentifier(String handlerIdentifier) {
		this.actionHandlerIdentifier = handlerIdentifier;
	}
	
	
	/**
	 * @see javax.faces.component.UIComponentBase#saveState(javax.faces.context.FacesContext)
	 */
	public Object saveState(FacesContext ctx) {
		Object values[] = new Object[6];
		values[0] = super.saveState(ctx);
		values[1] = this.resourcePath;
		values[2] = this.actionHandlerIdentifier;
		values[3] = this.categories;
		values[4] = this.baseFolderPath;
		values[5] = this.addMoodalBoxRel;
		return values;
	}
	
	/**
	 * @see javax.faces.component.UIComponentBase#restoreState(javax.faces.context.FacesContext, java.lang.Object)
	 */
	public void restoreState(FacesContext ctx, Object state) {
		Object values[] = (Object[])state;
		super.restoreState(ctx, values[0]);
		this.resourcePath = (String)values[1];
		this.actionHandlerIdentifier = (String)values[2];
		this.categories=(String)values[3];
		this.baseFolderPath=(String)values[4];
		this.addMoodalBoxRel = (Boolean) values[5];
	}
	
	
	
	/**
	 * @return Returns the categories.
	 */
	public String getCategories() {
		return this.categories;
	}

	
	/**
	 * @param categories The categories to set.
	 */
	public void setCategories(String categories) {
		this.categories = categories;
	}
	

	/**
	 * @return Returns the basePath.
	 */
	public String getBaseFolderPath() {
		return this.baseFolderPath;
	}

	
	/**
	 * @param basePath The basePath to set.
	 */
	public void setBaseFolderPath(String basePath) {
		this.baseFolderPath = basePath;
	}

}
