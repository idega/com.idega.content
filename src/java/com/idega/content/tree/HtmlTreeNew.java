package com.idega.content.tree;

import javax.faces.component.UICommand;
import javax.faces.component.html.HtmlCommandLink;
import javax.faces.context.FacesContext;
import javax.faces.el.MethodBinding;
import javax.faces.el.ValueBinding;

import org.apache.myfaces.custom.tree2.HtmlTree;
import org.apache.myfaces.custom.tree2.HtmlTreeRenderer;

import java.util.Map;

/**
 * Represents "tree data" in an HTML format.  Also provides a mechanism for maintaining expand/collapse
 * state of the nodes in the tree.
 *
 * @author Sean Schofield
 */
public class HtmlTreeNew extends HtmlTree
{
    public static final String COMPONENT_TYPE = "tree";
    private static final String DEFAULT_RENDERER_TYPE = "tree";
	
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
    public HtmlTreeNew()
    {
    	super();
        setRendererType(DEFAULT_RENDERER_TYPE);
 //       System.out.println("HtmlTree");
//    	try {
//    		throw new Exception();
//    	} catch (Exception e) {
//    		e.printStackTrace();
//    		System.out.println("xxxxxxxxxxxxxxxxxxxxx-");
//    	}        
    }
//    
    @Override
    public String getFamily() {
    	System.out.println("getFamily");
    	System.out.println("xxxxxxxxxxxxxxxxxxxxx");
    	
//    	try {
//    		throw new Exception();
//    	} catch (Exception e) {
//    		e.printStackTrace();
//    		System.out.println("xxxxxxxxxxxxxxxxxxxxx-");
//    	}
//    	return "iw_content";
    	return "org.apache.myfaces.HtmlTree2";
    }
}
