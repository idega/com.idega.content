/*
 * Copyright 2005 The Apache Software Foundation.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.idega.content.tree;

import org.apache.myfaces.shared_tomahawk.taglib.UIComponentTagBase;
import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.el.ValueBinding;

/**
 * @author Sean Schofield
 * @version $Revision: 1.1 $ $Date: 2006/09/22 12:35:00 $
 */

public class TreeTag extends UIComponentTagBase //UIComponentBodyTagBase
{
    private String _value;
    private String _var;
    private String _varNodeToggler;
    private String _showLines;
    private String _showNav;
    private String _clientSideToggle;
    private String _showRootNode;
    private String _preserveToggle;
    private String _javascriptLocation;
    private String _imageLocation;
  
    
    public void release()
    {
        super.release();

        _value=null;
        _var=null;
        _varNodeToggler=null;
        _showLines = null;
        _showNav = null;
        _clientSideToggle = null;
        _showRootNode = null;
        _preserveToggle = null;
        _javascriptLocation = null;
        _imageLocation = null;
    }

    public String getComponentType()
    {
//        return "org.apache.myfaces.HtmlTree2";
//    	return "com.idega.content.tree.HtmlTree2";
//    	return "com.idega.content.tree.HtmlTree";
    	return "tree";
    }

    public String getRendererType()
    {
        return "org.apache.myfaces.HtmlTree2";
//    	return "com.idega.content.tree.HtmlTree2";    	
//    	return "lala";
    }

    public void setValue(String value)
    {
        _value = value;
    }

    /**
     * @param var The var to set.
     */
    public void setVar(String var)
    {
        _var = var;
    }

    public void setVarNodeToggler(String varNodeToggler)
    {
        _varNodeToggler = varNodeToggler;
    }

    public void setShowLines(String showLines)
    {
        _showLines = showLines;
    }

    public void setShowNav(String showNav)
    {
        _showNav = showNav;
    }

    public void setClientSideToggle(String clientSideToggle)
    {
        _clientSideToggle = clientSideToggle;
    }

    public void setShowRootNode(String showRootNode)
    {
        _showRootNode = showRootNode;
    }

    public void setPreserveToggle(String preserveToggle)
    {
        _preserveToggle = preserveToggle;
    }

    /**
     * Overrides the super class method since we cannot store these properties in the component's attribute
     * map and still have things work with facelets.
     *
     * @param javascriptLocation
     */
    public void setJavascriptLocation(String javascriptLocation)
    {
        _javascriptLocation = javascriptLocation;
    }

    /**
     * Overrides the super class method since we cannot store these properties in the component's attribute
     * map and still have things work with facelets.
     *
     * @param imageLocation
     */
    public void setImageLocation(String imageLocation)
    {
        _imageLocation = imageLocation;
    }

    protected void setProperties(UIComponent component)
    {
        super.setProperties(component);

        FacesContext context = getFacesContext();

        if (_value != null)
        {
            ValueBinding vb = context.getApplication().createValueBinding(_value);
            component.setValueBinding("value", vb);
        }

        if (_var != null)
        {
            ((HtmlTree)component).setVar(_var);
        }

        if (_varNodeToggler != null)
        {
            ((HtmlTree)component).setVarNodeToggler(_varNodeToggler);
        }

        if (_showNav != null)
        {
        	setBooleanProperty(component, "showNav", _showNav);
        }

        if (_showLines != null)
        {
        	setBooleanProperty(component, "showLines", _showLines);
        }

        if (_clientSideToggle != null)
        {
        	setBooleanProperty(component, "clientSideToggle", _clientSideToggle);
        }

        if (_showRootNode != null)
        {
        	setBooleanProperty(component, "showRootNode", _showRootNode);
        }

        if (_preserveToggle != null)
        {
        	setBooleanProperty(component, "preserveToggle", _preserveToggle);
        }

        if (_javascriptLocation != null)
        {
            ((HtmlTree)component).setJavascriptLocation(_javascriptLocation);
        }

        if (_imageLocation != null)
        {
            ((HtmlTree)component).setImageLocation(_imageLocation);
        }
    }
}
