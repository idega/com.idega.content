/*
 * $Id: ContentItemToolbarButton.java,v 1.1 2005/03/11 17:03:05 gummi Exp $
 * Created on 9.3.2005
 *
 * Copyright (C) 2005 Idega Software hf. All Rights Reserved.
 *
 * This software is the proprietary information of Idega hf.
 * Use is subject to license terms.
 */
package com.idega.content.presentation;

import javax.faces.component.html.HtmlOutputLink;
import javax.faces.context.FacesContext;
import javax.faces.el.ValueBinding;


/**
 * 
 *  Last modified: $Date: 2005/03/11 17:03:05 $ by $Author: gummi $
 * 
 * @author <a href="mailto:gummi@idega.com">Gudmundur Agust Saemundsson</a>
 * @version $Revision: 1.1 $
 */
public class ContentItemToolbarButton extends HtmlOutputLink {

	
	private String resourcePath;
	private String action;
	private Boolean rendered;
	
	/**
	 * 
	 */
	public ContentItemToolbarButton() {
		super();
	}
	
	
	
	public boolean isRendered(){
		if (rendered != null) return rendered.booleanValue();
        ValueBinding vb = getValueBinding("rendered");
        Boolean v = vb != null ? (Boolean)vb.getValue(getFacesContext()) : null;
        if(v==null){
	        	/*try {
				IWContext iwc = IWContext.getInstance();
				IWSlideSession session = (IWSlideSession)IBOLookup.getSessionInstance(iwc,IWSlideSession.class);
				return session.hasPermission(getResourcePath(),IWSlideConstants.PRIVILEGE_WRITE);  //Test
			}
			catch (IBOLookupException e) {
				e.printStackTrace();
			}
			catch (UnavailableIWContext e) {
				e.printStackTrace();
			}
			catch (RemoteException e) {
				e.printStackTrace();
			}*/
			return true;
        } else {
        		return v.booleanValue();
        }
        //return false; //true when testing, then probably false
	}
	
	public void setRendered(boolean value){
		rendered = Boolean.valueOf(value);
	}
	
	/**
	 * @return Returns the action.
	 */
	public String getAction() {
		return action;
	}
	/**
	 * @param action The action to set.
	 */
	public void setAction(String action) {
		this.action= action;
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
	
	
	/**
	 * @see javax.faces.component.UIComponentBase#saveState(javax.faces.context.FacesContext)
	 */
	public Object saveState(FacesContext ctx) {
		Object values[] = new Object[9];
		values[0] = super.saveState(ctx);
		values[1] = resourcePath;
		values[2] = action;
		return values;
	}
	
	/**
	 * @see javax.faces.component.UIComponentBase#restoreState(javax.faces.context.FacesContext, java.lang.Object)
	 */
	public void restoreState(FacesContext ctx, Object state) {
		Object values[] = (Object[])state;
		super.restoreState(ctx, values[0]);
		resourcePath = (String)values[1];
		action = (String)values[2];
	}
	
}
