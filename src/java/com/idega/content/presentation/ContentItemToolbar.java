/*
 * $Id: ContentItemToolbar.java,v 1.2 2005/03/05 18:45:56 gummi Exp $
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
import javax.faces.component.html.HtmlOutputLink;
import javax.faces.context.FacesContext;
import javax.faces.el.ValueBinding;
import com.idega.core.uri.IWActionURIManager;
import com.idega.webface.WFToolbar;
import com.idega.webface.WFUtil;


/**
 * 
 *  Last modified: $Date: 2005/03/05 18:45:56 $ by $Author: gummi $
 * 
 * @author <a href="mailto:gummi@idega.com">Gudmundur Agust Saemundsson</a>
 * @version $Revision: 1.2 $
 */
public class ContentItemToolbar extends WFToolbar {
	
	
	private String resourcePath;
	private IWActionURIManager manager = null;
	
	private Map actions;
	private String[] RolesAllowded = new String[] {"content_editor","content_author"};  //Change to use constants
	private Boolean rendered;
	
	/**
	 * 
	 */
	public ContentItemToolbar() {
		super();
	}
	
	public IWActionURIManager getIWActionURIManager(){
		if(manager==null){
			manager = IWActionURIManager.getInstance();
		}
		return manager;
	}
	
	public HtmlOutputLink addToolbarButton(String action, String url, String menuItemId){
		HtmlOutputLink link = new HtmlOutputLink();
		link.setValue(url);
		link.setId(menuItemId);
		link.getChildren().add(WFUtil.getText(action)); //TMP
		addToActionMap(action,menuItemId);
		this.setMenuItem(menuItemId,link);
		
		return link;
	}
	
	public HtmlOutputLink addToolbarButton(String action, String menuItemId){
		String url = getActionURL(action,getResourcePath());
		return addToolbarButton(action,url,menuItemId);
	}
	
	public HtmlOutputLink addToolbarButton(String action){
		String url = getActionURL(action,getResourcePath());
		String menuItemId = getNextMenuItemId();
		return addToolbarButton(action,url,menuItemId);
	}
	
	protected String getActionURL(String action, String resourcePath){
		return getIWActionURIManager().getActionURIPrefixWithContext(action,resourcePath);
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
			HtmlOutputLink link = getToolbarButton(action);
			if(link==null){
				addToolbarButton(action);
			} else {
				link.setValue(getActionURL(action,getResourcePath()));
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
	}
	
	public String[] getRolesAllowed(){
		return RolesAllowded;
	}
	
	public boolean isRendered(){
		if (rendered != null) return rendered.booleanValue();
        ValueBinding vb = getValueBinding("rendered");
        Boolean v = vb != null ? (Boolean)vb.getValue(getFacesContext()) : null;
        if(v==null){
        		//check for role
        } else {
        		return v.booleanValue();
        }
        return true; //true when testing, then probably false
	}
	
	public void setRendered(boolean value){
		rendered = Boolean.valueOf(value);
	}
	
}
