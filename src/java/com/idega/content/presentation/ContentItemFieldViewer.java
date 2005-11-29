/*
 * $Id: ContentItemFieldViewer.java,v 1.3 2005/11/29 15:30:27 laddi Exp $
 * Created on 3.2.2005
 *
 * Copyright (C) 2005 Idega Software hf. All Rights Reserved.
 *
 * This software is the proprietary information of Idega hf.
 * Use is subject to license terms.
 */
package com.idega.content.presentation;

import java.io.IOException;
import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.el.ValueBinding;
import com.idega.util.RenderUtils;
import com.idega.webface.WFContainer;

/**
 * 
 *  Last modified: $Date: 2005/11/29 15:30:27 $ by $Author: laddi $
 * 
 * @author <a href="mailto:gummi@idega.com">Gudmundur Agust Saemundsson</a>
 * @version $Revision: 1.3 $
 */

public class ContentItemFieldViewer extends WFContainer {
		
		private static final String FACET_PREFIX = "value_prefix";
		private static final String FACET_VALUE = "value";
		private static final String FACET_SUFFIX = "value_suffix";
		private static final String FACET_CONTAINED_COMPONENT = "wf_field_container_component";
		
		private String _suffixClass;
		private String _prefixClass;
		
		
		public ContentItemFieldViewer(){
			super();
		}
		
		
		public void setSuffixClass(String suffixClasses) {
	        _suffixClass = suffixClasses;
	        WFContainer container = (WFContainer)getFacet(FACET_SUFFIX);
	        if(container != null){
	        		container.setStyleClass(suffixClasses);
	        }
	    }

	    public String getSuffixClass()
	    {
	        if (_suffixClass != null) return _suffixClass;
	        ValueBinding vb = getValueBinding("suffixClasses");
	        return vb != null ? (String)vb.getValue(getFacesContext()) : null;
	    }
	    
	    public void setPrefixClass(String prefixClasses) {
	        _prefixClass = prefixClasses;
	        WFContainer container = (WFContainer)getFacet(FACET_PREFIX);
	        if(container != null){
	        		container.setStyleClass(prefixClasses);
	        }
	    }

	    public String getPrefixClass() {
	        if (_prefixClass != null) return _prefixClass;
	        ValueBinding vb = getValueBinding("prefixClasses");
	        return vb != null ? (String)vb.getValue(getFacesContext()) : null;
	    }
		
		public UIComponent getPrefixComponent(){
			WFContainer c = (WFContainer)getFacet(FACET_PREFIX);
			if(c!=null){
				return c.getFacet(FACET_CONTAINED_COMPONENT);
			} else {
				return null;
			}
		}
		
		public UIComponent getMainComponent(){
			return getFacet(FACET_VALUE);
		}
		
		public UIComponent getSuffixComponent(){
			WFContainer c = (WFContainer)getFacet(FACET_SUFFIX);
			if(c!=null){
				return c.getFacet(FACET_CONTAINED_COMPONENT);
			} else {
				return null;
			}
		}
		
		public void setPrefixComponent(UIComponent component) {
			WFContainer c = new WFContainer();
			c.setStyleClass(getPrefixClass());
			c.add(component);
			getFacets().put(FACET_CONTAINED_COMPONENT, c);
		}
		
		public void setValueComponent(UIComponent component) {
			getFacets().put(FACET_VALUE, component);
		}
		
		public void setSuffixComponent(UIComponent component) {
			WFContainer c = new WFContainer();
			c.setStyleClass(getSuffixClass());
			c.add(component);
			getFacets().put(FACET_CONTAINED_COMPONENT, c);
		}
		
		/* (non-Javadoc)
		 * @see javax.faces.component.UIComponent#encodeChildren(javax.faces.context.FacesContext)
		 */
		public void encodeChildren(FacesContext context) throws IOException {
			super.encodeChildren(context);
			UIComponent prefix = getFacet(FACET_PREFIX);
			renderContainer(context,prefix);
			RenderUtils.renderFacet(context,this,FACET_VALUE);
			UIComponent suffix = getFacet(FACET_SUFFIX);
			renderContainer(context,suffix);
		}
		
		public void renderContainer(FacesContext context, UIComponent component) throws IOException{
			if(component != null) {
				component.encodeBegin(context);
//				component.encodeChildren(context);
				RenderUtils.renderFacet(context,component,FACET_CONTAINED_COMPONENT);
				component.encodeEnd(context);
			}
		}
		
		
		/**
		 * @see javax.faces.component.StateHolder#saveState(javax.faces.context.FacesContext)
		 */
		public Object saveState(FacesContext ctx) {
			Object values[] = new Object[3];
			values[0] = super.saveState(ctx);
			values[1] = _suffixClass;
			values[2] = _prefixClass;
			return values;
		}
		
		/**
		 * @see javax.faces.component.StatHolder#restoreState(javax.faces.context.FacesContext, java.lang.Object)
		 */
		public void restoreState(FacesContext ctx, Object state) {
			Object values[] = (Object[])state;
			super.restoreState(ctx, values[0]);
			this._suffixClass = (String) values[1];
			this._prefixClass = (String) values[2];
			
		}
		
	}