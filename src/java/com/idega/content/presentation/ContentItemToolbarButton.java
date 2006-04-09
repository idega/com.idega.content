/*
 * $Id: ContentItemToolbarButton.java,v 1.3 2006/04/09 12:01:54 laddi Exp $
 * Created on 9.3.2005
 *
 * Copyright (C) 2005 Idega Software hf. All Rights Reserved.
 *
 * This software is the proprietary information of Idega hf.
 * Use is subject to license terms.
 */
package com.idega.content.presentation;

import java.io.IOException;
import javax.faces.component.html.HtmlOutputLink;
import javax.faces.context.FacesContext;
import javax.faces.el.ValueBinding;


/**
 * 
 *  Last modified: $Date: 2006/04/09 12:01:54 $ by $Author: laddi $
 * 
 * @author <a href="mailto:gummi@idega.com">Gudmundur Agust Saemundsson</a>
 * @version $Revision: 1.3 $
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
		if (this.rendered != null) {
			return this.rendered.booleanValue();
		}
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
	
	/* (non-Javadoc)
	 * @see javax.faces.component.UIOutput#setValue(java.lang.Object)
	 */
	public void setValue(Object value) {
		// TODO Auto-generated method stub
		//super.setValue(value);
		String url = value.toString();
		//String javascriptUrl = "javascript:openContentEditor('"+url+"')";
		super.setValue(url);
	}
	
    public Object getValue()
    {
        /*if (_value != null) return _value;
        ValueBinding vb = getValueBinding("value");
        return vb != null ? (Object)vb.getValue(getFacesContext()) : null;*/
    		return super.getValue();
    }

	public void setRendered(boolean value){
		this.rendered = Boolean.valueOf(value);
	}
	
	/**
	 * @return Returns the action.
	 */
	public String getAction() {
		return this.action;
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
		return this.resourcePath;
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
		values[1] = this.resourcePath;
		values[2] = this.action;
		return values;
	}
	
	/**
	 * @see javax.faces.component.UIComponentBase#restoreState(javax.faces.context.FacesContext, java.lang.Object)
	 */
	public void restoreState(FacesContext ctx, Object state) {
		Object values[] = (Object[])state;
		super.restoreState(ctx, values[0]);
		this.resourcePath = (String)values[1];
		this.action = (String)values[2];
	}



	/* (non-Javadoc)
	 * @see javax.faces.component.UIComponentBase#encodeBegin(javax.faces.context.FacesContext)
	 */
	public void encodeBegin(FacesContext context) throws IOException {
		// TODO Auto-generated method stub
		super.encodeBegin(context);
	}



	/* (non-Javadoc)
	 * @see javax.faces.component.UIComponentBase#encodeChildren(javax.faces.context.FacesContext)
	 */
	public void encodeChildren(FacesContext context) throws IOException {
		// TODO Auto-generated method stub
		super.encodeChildren(context);
	}



	/* (non-Javadoc)
	 * @see javax.faces.component.UIComponentBase#encodeEnd(javax.faces.context.FacesContext)
	 */
	public void encodeEnd(FacesContext context) throws IOException {
		// TODO Auto-generated method stub
		super.encodeEnd(context);
	}



	/* (non-Javadoc)
	 * @see javax.faces.component.UIComponentBase#getRendererType()
	 */
	public String getRendererType() {
		// TODO Auto-generated method stub
		//return super.getRendererType();
		return "content_item_toolbar_button";
	}
	

	/* (non-Javadoc)
	 * @see javax.faces.component.UIOutput#getFamily()
	 */
	public String getFamily() {
		// TODO Auto-generated method stub
		return super.getFamily();
	}
	
}
