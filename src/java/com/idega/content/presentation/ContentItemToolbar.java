/*
 * $Id: ContentItemToolbar.java,v 1.13 2006/03/28 10:11:51 tryggvil Exp $
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
import com.idega.content.business.ContentUtil;
import com.idega.core.accesscontrol.business.AccessController;
import com.idega.core.uri.IWActionURIManager;
import com.idega.presentation.IWContext;
import com.idega.webface.WFToolbar;


/**
 *  <p>
 *  Toolbar used by new content management system to display editor buttons.
 *  </p>
 *  Last modified: $Date: 2006/03/28 10:11:51 $ by $Author: tryggvil $
 * 
 * @author <a href="mailto:gummi@idega.com">Gudmundur Agust Saemundsson</a>
 * @version $Revision: 1.13 $
 */
public class ContentItemToolbar extends WFToolbar {
	
	//static constants:
	public final static String PARAMETER_CATEGORIES="content_categories";
	public final static String PARAMETER_BASE_FOLDER_PATH="content_base_path";
	
	//instance variables
	private String resourcePath;
	private String actionHandlerIdentifier;
	private IWActionURIManager manager = null;
	
	private Map actions;
	private String[] RolesAllowded = null;
	private Boolean rendered;
	private String categories;
	private String baseFolderPath;

	
	/**
	 * 
	 */
	public ContentItemToolbar() {
		super();
		//this.setStyleClass("content_item_toolbar");
		this.setMenuStyleClass("content_item_toolbar");
	}
	
	public IWActionURIManager getIWActionURIManager(){
		if(manager==null){
			manager = IWActionURIManager.getInstance();
		}
		return manager;
	}
	
	public HtmlOutputLink addToolbarButton(String action, String url, String menuItemId){
		ContentItemToolbarButton link = new ContentItemToolbarButton();
		link.setValue(url);
		link.setId(menuItemId);
		//link.getChildren().add(WFUtil.getText(action)); //TMP
		addToActionMap(action,menuItemId);
		this.setMenuItem(menuItemId,link);
		
		String title = action;
		link.setResourcePath(getResourcePath());
		link.setAction(action);
		link.setStyleClass(action);
		link.setTitle(title);
		
		return link;
	}
	
	public HtmlOutputLink addToolbarButton(String action, String menuItemId){
		String url = getActionURL(action,getResourcePath(),getActionHandlerIdentifier());
		return addToolbarButton(action,url,menuItemId);
	}
	
	public HtmlOutputLink addToolbarButton(String action){
		String url = getActionURL(action,getResourcePath(),getActionHandlerIdentifier());
		String menuItemId = getNextMenuItemId();
		return addToolbarButton(action,url,menuItemId);
	}
	
	protected String getActionURL(String action, String resourcePath,String handlerIdentifier){
		String url = getIWActionURIManager().getActionURIPrefixWithContext(action,resourcePath,handlerIdentifier);
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
		Set s = getActions().keySet();
		for (Iterator iter = s.iterator(); iter.hasNext();) {
			String action = (String) iter.next();
			ContentItemToolbarButton link = (ContentItemToolbarButton)getToolbarButton(action);
			if(link==null){
				addToolbarButton(action);
			} else {
				link.setValue(getActionURL(action,getResourcePath(),getActionHandlerIdentifier()));
				
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
	
	public Map getActions(){
		if(actions==null){
			actions=new HashMap();
		}
		return actions;
	}
	
	
	protected void addToActionMap(String action, String menuItemId){
		getActions().put(action,menuItemId);
	}
	
	/**
	 * @return Returns the resourcePath.
	 */
	public String getResourcePath() {
		return resourcePath;
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
		return RolesAllowded;
	}
	
	public boolean isRendered(){
		if (rendered != null) return rendered.booleanValue();
        ValueBinding vb = getValueBinding("rendered");
        Boolean v = vb != null ? (Boolean)vb.getValue(getFacesContext()) : null;
        if(v==null){
        		IWContext iwc = IWContext.getInstance();
        		if(RolesAllowded==null){
        			return ContentUtil.hasContentEditorRoles(iwc);
        		}
        		else{
	        		AccessController ac = iwc.getAccessController();
	        		for (int i = 0; i < RolesAllowded.length; i++) {
					if(ac.hasRole(RolesAllowded[i],iwc)){
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
		rendered = Boolean.valueOf(value);
	}
	
	/**
	 * @return Returns the actionHandlerIdentifier.
	 */
	public String getActionHandlerIdentifier() {
		return actionHandlerIdentifier;
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
		Object values[] = new Object[5];
		values[0] = super.saveState(ctx);
		values[1] = resourcePath;
		values[2] = actionHandlerIdentifier;
		values[3] = categories;
		values[4] = baseFolderPath;
		return values;
	}
	
	/**
	 * @see javax.faces.component.UIComponentBase#restoreState(javax.faces.context.FacesContext, java.lang.Object)
	 */
	public void restoreState(FacesContext ctx, Object state) {
		Object values[] = (Object[])state;
		super.restoreState(ctx, values[0]);
		resourcePath = (String)values[1];
		actionHandlerIdentifier = (String)values[2];
		categories=(String)values[3];
		baseFolderPath=(String)values[4];
	}
	
	
	
	/**
	 * @return Returns the categories.
	 */
	public String getCategories() {
		return categories;
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
		return baseFolderPath;
	}

	
	/**
	 * @param basePath The basePath to set.
	 */
	public void setBaseFolderPath(String basePath) {
		this.baseFolderPath = basePath;
	}

}
