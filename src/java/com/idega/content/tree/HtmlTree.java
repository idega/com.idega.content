/*
 * Copyright 2004 The Apache Software Foundation.
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

import javax.faces.component.UICommand;
import javax.faces.component.html.HtmlCommandLink;
import javax.faces.context.FacesContext;
import javax.faces.el.MethodBinding;
import javax.faces.el.ValueBinding;
import java.util.Map;

/**
 * Represents "tree data" in an HTML format.  Also provides a mechanism for maintaining expand/collapse
 * state of the nodes in the tree.
 *
 * @author Sean Schofield
 */
public class HtmlTree extends UITreeData
{
//    public static final String COMPONENT_TYPE = "org.apache.myfaces.HtmlTree2";
//	public static final String COMPONENT_TYPE = "com.idega.content.tree.HTMLTree";
	public static final String COMPONENT_TYPE = "lala";
	
//    private static final String DEFAULT_RENDERER_TYPE = "org.apache.myfaces.HtmlTree2";
//    private static final String DEFAULT_RENDERER_TYPE = "com.idega.content.tree.HTMLTreeRenderer";
    private static final String DEFAULT_RENDERER_TYPE = "lala";
	
	private UICommand _expandControl;
    private String _varNodeToggler;
    private Boolean _showNav;
    private Boolean _showLines;
    private Boolean _clientSideToggle;
    private Boolean _showRootNode;
    private Boolean _preserveToggle;
    private String _javascriptLocation;
    private String _imageLocation;

    /**
     * Constructor
     */
    public HtmlTree()
    {
        setRendererType(DEFAULT_RENDERER_TYPE);
        _expandControl = new HtmlCommandLink();
        _expandControl.setParent(this);
        _clientSideToggle = Boolean.TRUE;
        _preserveToggle = Boolean.TRUE;
        _showRootNode = Boolean.TRUE;
        _showNav = Boolean.TRUE;
        _showLines = Boolean.TRUE;
    }

    // see superclass for documentation
    public Object saveState(FacesContext context)
    {
        Object values[] = new Object[9];
        values[0] = super.saveState(context);
        values[1] = _varNodeToggler;
        values[2] = _showLines;
        values[3] = _showNav;
        values[4] = _clientSideToggle;
        values[5] = _showRootNode;
        values[6] = _preserveToggle;
        values[7] = _javascriptLocation;
        values[8] = _imageLocation;

        return (Object)values;
    }

    // see superclass for documentation
    public void restoreState(FacesContext context, Object state)
    {
        Object values[] = (Object[])state;
        super.restoreState(context, values[0]);
        setVarNodeToggler((String)values[1]);
        setShowLines(((Boolean)values[2]).booleanValue());
        setShowNav(((Boolean)values[3]).booleanValue());
        setClientSideToggle(((Boolean)values[4]).booleanValue());
        setShowRootNode(((Boolean)values[5]).booleanValue());
        setPreserveToggle(((Boolean)values[6]).booleanValue());
        setJavascriptLocation((String)values[7]);
        setImageLocation((String)values[8]);
    }

    // see superclass for documentation
    public void setNodeId(String nodeId)
    {
        super.setNodeId(nodeId);

        if (_varNodeToggler != null)
        {
            Map requestMap = getFacesContext().getExternalContext().getRequestMap();
            requestMap.put(_varNodeToggler, this);
        }
    }

    /**
     * Gets the expand/collapse control that can be used to handle expand/collapse nodes.  This is only used in server-side
     * mode.  It allows the nagivation controls (if any) to be clickable as well as any commandLinks the user has set up in
     * their JSP.
     *
     * @return UICommand
     */
    public UICommand getExpandControl()
    {
        return _expandControl;
    }

    public void setVarNodeToggler(String varNodeToggler)
    {
        _varNodeToggler = varNodeToggler;

        // create a method binding for the expand control
        String bindingString = "#{" + varNodeToggler + ".toggleExpanded}";
        MethodBinding actionBinding = FacesContext.getCurrentInstance().getApplication().createMethodBinding(bindingString, null);
        _expandControl.setAction(actionBinding);
    }

    public void setShowNav(boolean showNav)
    {
        _showNav = Boolean.valueOf(showNav);
    }

    public boolean isShowNav()
    {
        if (_showNav != null) return _showNav.booleanValue();
        ValueBinding vb = getValueBinding("showNav");
        Boolean v = vb != null ? (Boolean)vb.getValue(getFacesContext()) : null;
        return v == null ||  v.booleanValue();
    }

    public void setShowLines(boolean showLines)
    {
        _showLines = Boolean.valueOf(showLines);
    }

    public boolean isShowLines()
    {
        if (_showLines != null) return _showLines.booleanValue();
        ValueBinding vb = getValueBinding("showLines");
        Boolean v = vb != null ? (Boolean)vb.getValue(getFacesContext()) : null;
        return v == null || v.booleanValue();
    }

    public void setClientSideToggle(boolean clientSideToggle)
    {
        _clientSideToggle = Boolean.valueOf(clientSideToggle);
    }

    public boolean isClientSideToggle()
    {
        if (_clientSideToggle != null) return _clientSideToggle.booleanValue();
        ValueBinding vb = getValueBinding("clientSideToggle");
        Boolean v = vb != null ? (Boolean)vb.getValue(getFacesContext()) : null;
        return v == null || v.booleanValue();
    }

    public void setShowRootNode(boolean showRootNode)
    {
        _showRootNode = Boolean.valueOf(showRootNode);
    }

    public boolean isShowRootNode()
    {
        if (_showRootNode != null) return _showRootNode.booleanValue();
        ValueBinding vb = getValueBinding("showRootNode");
        Boolean v = vb != null ? (Boolean)vb.getValue(getFacesContext()) : null;
        return v == null || v.booleanValue();
    }

    public void setPreserveToggle(boolean preserveToggle)
    {
        _preserveToggle = Boolean.valueOf(preserveToggle);
    }

    public boolean isPreserveToggle()
    {
        if (_preserveToggle != null) return _preserveToggle.booleanValue();
        ValueBinding vb = getValueBinding("preserveToggle");
        Boolean v = vb != null ? (Boolean)vb.getValue(getFacesContext()) : null;
        return v == null || v.booleanValue();
    }

    public void setJavascriptLocation(String javascriptLocation)
    {
        _javascriptLocation = javascriptLocation;
    }

    public String getJavascriptLocation()
    {
        if (_javascriptLocation != null) return _javascriptLocation;

        ValueBinding vb = getValueBinding("javascriptLocation");
        if (vb == null)
        {
            return null;
        }
        else
        {
            return (String)vb.getValue(getFacesContext());
        }
    }

    public void setImageLocation(String imageLocation)
    {
        _imageLocation = imageLocation;
    }

    public String getImageLocation()
    {
        if (_imageLocation != null) return _imageLocation;

        ValueBinding vb = getValueBinding("imageLocation");
        if (vb == null)
        {
            return null;
        }
        else
        {
            return (String)vb.getValue(getFacesContext());
        }
    }
}
