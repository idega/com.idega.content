package com.idega.content.tree;

import org.apache.myfaces.custom.tree2.HtmlTree;
//import com.idega.content.tree.HtmlTreeNew;
import org.apache.myfaces.custom.tree2.HtmlTreeRenderer;
import org.apache.myfaces.custom.tree2.TreeState;
//import com.idega.content.tree.TreeState;
import org.apache.myfaces.custom.tree2.TreeWalker;
//import com.idega.content.tree.TreeWalker;
import org.apache.myfaces.custom.tree2.TreeNode;
//import com.idega.content.tree.TreeNode;
import org.apache.myfaces.renderkit.html.util.AddResource;
import org.apache.myfaces.renderkit.html.util.AddResourceFactory;
import org.apache.myfaces.shared_tomahawk.renderkit.html.HTML;
import org.apache.myfaces.shared_tomahawk.renderkit.html.HtmlRendererUtils;
import org.apache.myfaces.shared_tomahawk.renderkit.RendererUtils;

//import org.apache.myfaces.custom.tree2.*;

import javax.faces.component.NamingContainer;
import javax.faces.component.UIComponent;
import javax.faces.component.UICommand;
import javax.faces.component.UIGraphic;
import javax.faces.component.UIViewRoot;
import javax.faces.component.UIParameter;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;
import javax.faces.render.Renderer;

import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.util.Map;
import java.util.Iterator;
import java.net.URLDecoder;
import javax.servlet.http.Cookie;
import java.util.HashMap;

import java.util.List;

/**
 * @author Sean Schofield
 * @author Chris Barlow
 * @author Hans Bergsten (Some code taken from an example in his O'Reilly JavaServer Faces book. Copied with permission)
 * @version $Revision: 1.1 $ $Date: 2006/09/22 12:35:00 $
 */
public class HtmlTreeRendererNew extends HtmlTreeRenderer
{
	
    private static final String NAV_COMMAND = "org.apache.myfaces.tree.NAV_COMMAND";
    private static final String ENCODING = "UTF-8";
    private static final String ATTRIB_DELIM = ";";
    private static final String ATTRIB_KEYVAL = "=";
    private static final String NODE_STATE_EXPANDED = "x";
    private static final String NODE_STATE_CLOSED = "c";
    private static final String SEPARATOR = String.valueOf(NamingContainer.SEPARATOR_CHAR);
    private static final String IMAGE_PREFIX = "t2";
    private static final String TOGGLE_ID = "t2g";

    private static final int NOTHING = 0;
    private static final int CHILDREN = 1;
    private static final int EXPANDED = 2;
    private static final int LINES = 4;
    private static final int LAST = 8;
	
    private static int node = 0;
    private NodeID IDPairs;
    
    public String getNode(){
    	node++;
    	return "node"+node;
    }
    
    public HtmlTreeRendererNew(){
    	IDPairs = new NodeID();
//    	super();
//    	System.out.println("medzio konstruktorius");
    }
    
protected void beforeNodeEncode(FacesContext context, ResponseWriter out, HtmlTree tree)
    throws IOException {
//    System.out.println("before node encode");
    out.startElement(HTML.LI_ELEM, tree);
//    out.writeAttribute(HTML.ID_ATTR,getNode(),null);
    out.writeAttribute(HTML.ID_ATTR,tree.getNodeId(),null);
}

protected void afterNodeEncode(FacesContext context, ResponseWriter out)
    throws IOException {
//    out.endElement(HTML.TR_ELEM);
//    out.endElement(HTML.TABLE_ELEM);
//	System.out.println();
//	System.out.println("after node encode");  //sita breika pagauna
//	System.out.println();
	out.endElement(HTML.LI_ELEM);
}

public void encodeChildren(FacesContext context, UIComponent component) throws IOException
{

    HtmlTree tree = (HtmlTree)component;
    
//	HtmlTree tree = (HtmlTree)component;
    
    if (!component.isRendered()) return;
    if (tree.getValue() == null) return;

    ResponseWriter out = context.getResponseWriter();
    String clientId = null;

    if (component.getId() != null && !component.getId().startsWith(UIViewRoot.UNIQUE_ID_PREFIX))
    {
        clientId = component.getClientId(context);
    }

    boolean isOuterSpanUsed = false;

    if (clientId != null)
    {
        isOuterSpanUsed = true;
        out.startElement("span", component);
        out.writeAttribute("id", clientId, "id");
    }

    boolean clientSideToggle = tree.isClientSideToggle();
    boolean showRootNode = tree.isShowRootNode();

//   TreeState state = tree.getDataModel().getTreeState();

    TreeState state = tree.getDataModel().getTreeState();
    
//    TreeWalker walker = tree.getDataModel().getTreeWalker();

    TreeWalker walker = tree.getDataModel().getTreeWalker();
    
    walker.reset();
    walker.setTree(tree);

    walker.setCheckState(!clientSideToggle); // walk all nodes in client mode

    out.startElement(HTML.UL_ELEM, tree);
    out.writeAttribute(HTML.ID_ATTR,"page_tree_div",null);
    out.writeAttribute(HTML.CLASS_ATTR,"tree_drag_drop",null);
    
    if (showRootNode)
    {
        // encode the tree (starting with the root node)
// temp source b
System.out.println("tree ID = " + tree.getId());
 //   	TreeNode n = tree.
System.out.println("tree child = " + tree.getChildCount());    

System.out.println("node id = " + tree.getNodeId());
// temp source e

        if (walker.next())
        {
        	System.out.println("****************************");
//        	System.out.println("old ID = " + tree.getNodeId());
        	List children = tree.getNode().getChildren();
        	for (int j = 0; j < tree.getNode().getChildCount(); j++){
        		TreeNode tnode = (TreeNode)children.get(j);
        		System.out.println("old node = " + tnode.getIdentifier() + "new node = " + tree.getNodeId()+":"+j);
        		IDPairs.addID(tnode.getIdentifier(), tree.getNodeId()+":"+j);
        	}
        	System.out.println("****************************");          	
//        	 temp source b
//System.out.println("tree ID = " + tree.getId());
//        	 //   	TreeNode n = tree.
//System.out.println("tree child = " + tree.getChildCount());    	
//System.out.println("node id = " + tree.getNodeId());
//System.out.println("node child = " + tree.getNode().getChildCount());        	
//List children = tree.getNode().getChildren();
//TreeNode tnode = (TreeNode)children.get(0);
//System.out.println("children.get(0).getIdentifier() = " + tnode.getIdentifier());
////System.out.println("children.get(0).getIdentifier() = " + tree.get
////        	 temp source e
        	encodeRoot(context, out, tree, walker);
        }        
    }
    else
    {
        // skip the root node
        walker.next();
//        TreeNode rootNode = tree.getNode();
        TreeNode rootNode = tree.getNode();

        // now mark the root as expanded (so we don't stop there)
        String rootNodeId = tree.getNodeId();
        if(!state.isNodeExpanded(rootNodeId))
        {
            state.toggleExpanded(rootNodeId);
        }

        // now encode each of the nodes in the level immediately below the root
        for (int i=0; i < rootNode.getChildCount(); i++)
        {
            if (walker.next()){
            	encodeRoot(context, out, tree, walker);
            }
        }
    }
    
    System.out.println("PRINTING NODES FROM ARRAY");
    for (int j = 0; j < IDPairs.countPairs(); j++){
    	System.out.println("ID in DB = "+IDPairs.getIDinDB(j)+" ID in tree = "+IDPairs.getIDinTree(j));
    }
    
    
    out.endElement(HTML.UL_ELEM);

    // reset the current node id once we're done encoding everything
    tree.setNodeId(null);

    if (isOuterSpanUsed)
    {
        out.endElement("span");
    }
}

//@Override
//protected void encodeTree(FacesContext arg0, ResponseWriter arg1, HtmlTree arg2, TreeWalker arg3) throws IOException {
//protected void encodeTree(FacesContext context, ResponseWriter out, HtmlTree tree, TreeWalker walker) throws IOException {
protected void encodeTree(FacesContext context, ResponseWriter out, HtmlTree tree, TreeWalker walker) throws IOException {
//    beforeNodeEncode(arg0, arg1, arg2);
//    encodeCurrentNode(arg0, arg1, arg2);
//    afterNodeEncode(arg0, arg1);	
////	super.encodeTree(arg0, arg1, arg2, arg3);
    boolean clientSideToggle = tree.isClientSideToggle();
    boolean expanded = false;
    String IDbefore, IDafter;
//	System.out.println("<ul>");
//	out.startElement(HTML.UL_ELEM, tree);
    // encode the current node
    HtmlRendererUtils.writePrettyLineSeparator(context);
    beforeNodeEncode(context, out, tree);
    encodeCurrentNode(context, out, tree);
    
//    afterNodeEncode(context, out);

    // if client side toggling is on, add a span to be used for displaying/hiding children
    if (clientSideToggle)
    {	
System.out.println("CLIENT SIDE");
    	
        String spanId = TOGGLE_SPAN + ":" + tree.getId() + ":" + tree.getNodeId();

//		String spanId = TOGGLE_SPAN + ":" + tree.getId() + ":" + node;

        out.startElement(HTML.SPAN_ELEM, tree);
        out.writeAttribute(HTML.ID_ATTR, spanId, null);

        if (tree.isNodeExpanded())
        {
            out.writeAttribute(HTML.STYLE_ATTR, "display:block", null);
        }
        else
        {
            out.writeAttribute(HTML.STYLE_ATTR, "display:none", null);
        }
    }
    else 
System.out.println("NOT CLIENT SIDE");

    TreeNode node = tree.getNode(); 
//    tree.
    if (tree.isNodeExpanded() || node.getChildCount() > 0){    
System.out.println("node is expanded");
//    	System.out.println("<ul>");
    	out.startElement(HTML.UL_ELEM, tree);
    	expanded = true;
    }
    else
System.out.println("node is not expanded");    
    
    if (node.isLeaf()){
        if (clientSideToggle)
        {
//        	System.out.println("</span>");
            out.endElement(HTML.SPAN_ELEM);
        }    	
    	out.endElement(HTML.LI_ELEM);
    }
//	System.out.println("***BEFORE CYCLE. CHILDS = "+node.getChildCount() +" *** EXPANDED = "+ expanded);
	
    for (int i=0; i < node.getChildCount(); i++)
    {
    	
//    	System.out.println("==> getNodeIDbefore = "+ tree.getNodeId());
    	IDbefore = tree.getNodeId();
    	
//System.out.println("children.get(0).getIdentifier() = " + tnode.toString());


        if (walker.next()) {    											//WALKER.NEXT
        	System.out.println("****************************");
//        	System.out.println("old ID = " + tree.getNodeId());
        	List children = tree.getNode().getChildren();
        	for (int j = 0; j < tree.getNode().getChildCount(); j++){
        		TreeNode tnode = (TreeNode)children.get(j);
        		System.out.println("old node = " + tnode.getIdentifier() + "new node = " + tree.getNodeId()+":"+j);
        		IDPairs.addID(tnode.getIdentifier(), tree.getNodeId()+":"+j);
        	}
        	System.out.println("****************************");        	
        	
//        	System.out.println("==> getNodeIDafter = "+ tree.getNodeId());
        	IDafter = tree.getNodeId();
        	
//        	if (!tree.isNodeExpanded())
//        		out.endElement(HTML.LI_ELEM);
        		
        	if (IDafter.length() < IDbefore.length()){
//        		System.out.println("SKIRTUMAS = " + (IDbefore.length()-IDafter.length()));
        		for (int j = 0; j < ((IDbefore.length()-IDafter.length())/2); j++ ){
        			out.endElement(HTML.UL_ELEM);
        			
        		    if (clientSideToggle)
        		    {
//        		    	System.out.println("</span>");
        		        out.endElement(HTML.SPAN_ELEM);
        		    }       			
        			
        			out.endElement(HTML.LI_ELEM);
//        			System.out.println("UL CIKLE");
        		}
        	}
//        	    out.endElement(HTML.LI_ELEM);
//        	}
//        	else 
////        	    out.endElement(HTML.LI_ELEM);
//        	System.out.println("WALKER.NEXT = TRUE nodeID = " + walker.getRootNodeId());
        	
        	encodeTree(context, out, tree, walker);
        }
 //       else {
//        	System.out.println("WALKER.NEXT = FALSE");
//        	out.endElement(HTML.LI_ELEM);
//        }
    }
    
//	System.out.println("***AFTER CYCLE***");
    
//    if (clientSideToggle)
//    {
////    	System.out.println("</span>");
//        out.endElement(HTML.SPAN_ELEM);
//    }
   
    if (expanded){    
 //       out.endElement(HTML.UL_ELEM);
//        System.out.println("right </ul>");  
    }
    else {
//    	System.out.println("partly </ul>");
    }
//    out.endElement(HTML.LI_ELEM);
}
//    }

protected void encodeRoot(FacesContext context, ResponseWriter out, HtmlTree tree, TreeWalker walker) throws IOException {

  boolean clientSideToggle = tree.isClientSideToggle();
  boolean expanded = false;
  String IDbefore, IDafter;
  
  HtmlRendererUtils.writePrettyLineSeparator(context);
 
  out.startElement(HTML.LI_ELEM, tree);
//  out.writeAttribute(HTML.ID_ATTR,getNode(),null);
  
  out.writeAttribute(HTML.ID_ATTR, tree.getNodeId(), null);
  
  out.writeAttribute("noDrag","true",null);
  
  encodeRootNode(context, out, tree);

  if (clientSideToggle){
      String spanId = TOGGLE_SPAN + ":" + tree.getId() + ":" + tree.getNodeId();

      out.startElement(HTML.SPAN_ELEM, tree);
      out.writeAttribute(HTML.ID_ATTR, spanId, null);

      if (tree.isNodeExpanded()){
          out.writeAttribute(HTML.STYLE_ATTR, "display:block", null);
      }
      else {
          out.writeAttribute(HTML.STYLE_ATTR, "display:none", null);
      }
  }

  TreeNode node = tree.getNode(); 

System.out.println("==> getRootNodeIDbefore = "+ tree.getNodeId());
  if (tree.isNodeExpanded()){    
System.out.println("<ul> root");
  	out.startElement(HTML.UL_ELEM, tree);
  	expanded = true;
  }
System.out.println("root node isn't expanded. child count = "+ node.getChildCount());  
  
  for (int i=0; i < node.getChildCount(); i++)
  {
	  
System.out.println("==> getRootNodeIDbefore = "+ tree.getNodeId());
System.out.println("==> WALKER CHECK STATE = "+ walker.isCheckState());

	  IDbefore = tree.getNodeId();
System.out.println("old ID = " + tree.getNodeId());
      if (walker.next()){
    	  
      	System.out.println("****************************");
//    	System.out.println("old ID = " + tree.getNodeId());
    	List children = tree.getNode().getChildren();
    	for (int j = 0; j < tree.getNode().getChildCount(); j++){
    		TreeNode tnode = (TreeNode)children.get(j);
    		System.out.println("old node = " + tnode.getIdentifier() + "new node = " + tree.getNodeId()+":"+j);
    		IDPairs.addID(tnode.getIdentifier(), tree.getNodeId()+":"+j);
    	}
    	System.out.println("****************************");      	  
    	  
//System.out.println("==> getRootNodeIDafter = "+ tree.getNodeId());
    	  IDafter = tree.getNodeId();    	  
//    	  out.endElement(HTML.LI_ELEM);
    	  for (int j = 0; j < ((IDbefore.length()-IDafter.length())/2); j++ ){
			out.endElement(HTML.UL_ELEM);
			out.endElement(HTML.LI_ELEM);
//			System.out.println("UL CIKLE");			
    	  }
//      {
//      	System.out.println("**********************");    	  tai
//    	System.out.println("encoding root children");
//    	System.out.println("**********************");    
    	
      	encodeTree(context, out, tree, walker);
      }
  }

  if (clientSideToggle)
  {
      out.endElement(HTML.SPAN_ELEM);
  }
  if (expanded){    
//	  System.out.println("</ul> root"); 
      out.endElement(HTML.UL_ELEM);  
  }
  out.endElement(HTML.LI_ELEM);
}


/**
 * Handles the encoding related to the navigation functionality.
 *
 * @param context FacesContext
 * @param out ResponseWriter
 * @param tree HtmlTree
 * @return The additional navigation image to display inside the node (if any).  Only used with client-side toggle.
 * @throws IOException
 */

private String getImageSrc(FacesContext context, UIComponent component, String imageName, boolean withContextPath)
{
    String imageLocation = ((HtmlTree)component).getImageLocation();
    AddResource addResource = AddResourceFactory.getInstance(context);
    if (imageLocation == null)
    {
        return addResource.getResourceUri(context, HtmlTreeRenderer.class,
                                          "images/" + imageName, withContextPath);
    }
    else
    {
        return addResource.getResourceUri(context, imageLocation + "/" + imageName, withContextPath);
    }
}

protected void encodeCurrentNode(FacesContext context, ResponseWriter out, HtmlTree tree)
throws IOException
{
//TreeNode node = tree.getNode();

	TreeNode node = tree.getNode();
// set configurable values
boolean showRootNode = tree.isShowRootNode();
boolean showNav = tree.isShowNav();
boolean showLines = tree.isShowLines();
boolean clientSideToggle = tree.isClientSideToggle();

if (clientSideToggle)
{
    // we must show the nav icons if client side toggle is enabled (regardless of what user says)
    showNav = true;
}

UIComponent nodeTypeFacet = tree.getFacet(node.getType());
UIComponent nodeImgFacet = null;

if (nodeTypeFacet == null)
{
    throw new IllegalArgumentException("Unable to locate facet with the name: " + node.getType());
}

// render node padding
String[] pathInfo = tree.getPathInformation(tree.getNodeId());
int paddingLevel = pathInfo.length - 1;

for (int i = (showRootNode ? 0 : 1); i < paddingLevel; i++)
{
    boolean lastChild = tree.isLastChild((String)pathInfo[i]);
    String lineSrc = (!lastChild && showLines)
                     ? getImageSrc(context, tree, "line-trunk.gif", true)
                     : getImageSrc(context, tree, "spacer.gif", true);
                     
                     out.startElement(HTML.IMG_ELEM, tree);
                     out.writeURIAttribute(HTML.SRC_ATTR, lineSrc, null);

                     out.endElement(HTML.IMG_ELEM);
                     
//                     out.endElement(HTML.LI_ELEM);                   
}

if (showNav){
    nodeImgFacet = encodeNavigation(context, out, tree);
}

// render node
//out.startElement(HTML.TD_ELEM, tree);
if (nodeImgFacet != null)
{
    RendererUtils.renderChild(context, nodeImgFacet);
}

RendererUtils.renderChild(context, nodeTypeFacet);
//out.endElement(HTML.TD_ELEM);
}

protected void encodeRootNode(FacesContext context, ResponseWriter out, HtmlTree tree)
throws IOException
{
//TreeNode node = tree.getNode();

	TreeNode node = tree.getNode();
//System.out.println("encode current node");	

//TreeState state =  tree.getDataModel().getTreeState();
//String id = tree.getNodeId();
//state.toggleExpanded(id);

// set configurable values
boolean showRootNode = tree.isShowRootNode();
boolean showNav = tree.isShowNav();
boolean showLines = tree.isShowLines();
boolean clientSideToggle = tree.isClientSideToggle();

if (clientSideToggle)
{
    // we must show the nav icons if client side toggle is enabled (regardless of what user says)
    showNav = true;
}

UIComponent nodeTypeFacet = tree.getFacet(node.getType());
UIComponent nodeImgFacet = null;

if (nodeTypeFacet == null)
{
    throw new IllegalArgumentException("Unable to locate facet with the name: " + node.getType());
}

// render node padding
String[] pathInfo = tree.getPathInformation(tree.getNodeId());
int paddingLevel = pathInfo.length - 1;

for (int i = (showRootNode ? 0 : 1); i < paddingLevel; i++)
{
    boolean lastChild = tree.isLastChild((String)pathInfo[i]);
    String lineSrc = (!lastChild && showLines)
                     ? getImageSrc(context, tree, "line-trunk.gif", true)
                     : getImageSrc(context, tree, "spacer.gif", true);
                     
                     out.startElement(HTML.IMG_ELEM, tree);
                     out.writeURIAttribute(HTML.SRC_ATTR, lineSrc, null);

                     out.endElement(HTML.IMG_ELEM);                                        
}

if (showNav){
    nodeImgFacet = encodeNavigation(context, out, tree);
}

// render node
//out.startElement(HTML.TD_ELEM, tree);
if (nodeImgFacet != null)
{
    RendererUtils.renderChild(context, nodeImgFacet);
}
RendererUtils.renderChild(context, nodeTypeFacet);
//out.endElement(HTML.TD_ELEM);
}

private UIComponent encodeNavigation(FacesContext context, ResponseWriter out, HtmlTree tree)
throws IOException {
//TreeNode node = tree.getNode();

//TreeNode node = tree.getNode();
TreeNode node = tree.getNode();
String nodeId = tree.getNodeId();
String spanId = TOGGLE_SPAN + ":" + tree.getId() + ":" + nodeId;//TOGGLE_SPAN + nodeId;
boolean showLines = tree.isShowLines();
boolean clientSideToggle = tree.isClientSideToggle();
UIComponent nodeTypeFacet = tree.getFacet(node.getType());
String navSrc = null;
String altSrc = null;
UIComponent nodeImgFacet = null;

int bitMask = NOTHING;
bitMask += (node.isLeaf()) ? NOTHING : CHILDREN;
if (bitMask == CHILDREN) // if there are no children, ignore expand state -> more flexible with dynamic tree-structures
    bitMask += (tree.isNodeExpanded()) ? EXPANDED : NOTHING;
bitMask += (tree.isLastChild(tree.getNodeId())) ? LAST : NOTHING;
bitMask += (showLines) ? LINES : NOTHING;

switch (bitMask)
{
    case (NOTHING):

    case (LAST):
        navSrc = "spacer.gif";
        break;

    case (LINES):
        navSrc = "line-middle.gif";
        break;

    case (LINES + LAST):
        navSrc = "line-last.gif";
        break;

    case (CHILDREN):

    case (CHILDREN + LAST):
        navSrc = "nav-plus.gif";
        altSrc = "nav-minus.gif";
        break;

    case (CHILDREN + LINES):

        navSrc = "nav-plus-line-middle.gif";
        altSrc = "nav-minus-line-middle.gif";
        break;

    case (CHILDREN + LINES + LAST):

        navSrc = "nav-plus-line-last.gif";
        altSrc = "nav-minus-line-last.gif";
        break;

    case (CHILDREN + EXPANDED):

    case (CHILDREN + EXPANDED + LAST):
        navSrc = "nav-minus.gif";
        altSrc = "nav-plus.gif";
        break;

    case (CHILDREN + EXPANDED + LINES):
        navSrc = "nav-minus-line-middle.gif";
        altSrc = "nav-plus-line-middle.gif";
        break;

    case (CHILDREN + EXPANDED + LINES + LAST):
        navSrc = "nav-minus-line-last.gif";
        altSrc = "nav-plus-line-last.gif";
        break;

    // unacceptable bitmask combinations

    case (EXPANDED + LINES):
    case (EXPANDED + LINES + LAST):
    case (EXPANDED):
    case (EXPANDED + LAST):

        throw new IllegalStateException("Encountered a node ["+ nodeId + "] + with an illogical state.  " +
                                        "Node is expanded but it is also considered a leaf (a leaf cannot be considered expanded.");

    default:
        // catch all for any other combinations
        throw new IllegalArgumentException("Invalid bit mask of " + bitMask);
}

// adjust navSrc and altSrc so that the images can be retrieved using the extensions filter
String navSrcUrl = getImageSrc(context, tree, navSrc, false);
navSrc = getImageSrc(context, tree, navSrc, true);
altSrc = getImageSrc(context, tree, altSrc, true);

// render nav cell
//out.startElement(HTML.TD_ELEM, tree);
//out.writeAttribute(HTML.WIDTH_ATTR, "19", null);
//out.writeAttribute(HTML.HEIGHT_ATTR, "100%", null);
//out.writeAttribute("valign", "top", null);

if ((bitMask & LINES)!=0 && (bitMask & LAST)==0)
{
//    out.writeURIAttribute("background", getImageSrc(context, tree, "line-trunk.gif", true), null);
}

//add the appropriate image for the nav control
UIGraphic image = new UIGraphic();
image.setId(IMAGE_PREFIX);
image.setUrl(navSrcUrl);
Map imageAttrs = image.getAttributes();
//imageAttrs.put(HTML.WIDTH_ATTR, "19");
//imageAttrs.put(HTML.HEIGHT_ATTR, "18");
//imageAttrs.put(HTML.BORDER_ATTR, "0");

if (clientSideToggle)
{
    /**
     * With client side toggle, user has the option to specify open/closed images for the node (in addition to
     * the navigtion ones provided by the component.)
     */
    String expandImgSrc = "";
    String collapseImgSrc = "";
    String nodeImageId = "";

    UIComponent expandFacet = nodeTypeFacet.getFacet("expand");
    if (expandFacet != null)
    {     	
        UIGraphic expandImg = (UIGraphic)expandFacet;
        expandImgSrc = context.getApplication().getViewHandler().getResourceURL(context, expandImg.getUrl());
        if (expandImg.isRendered())
        {
            expandImg.setId(IMAGE_PREFIX + NODE_STATE_EXPANDED);
            expandImg.setParent(tree);
            nodeImageId = expandImg.getClientId(context);
            nodeImgFacet = expandFacet;
        }
    }

    UIComponent collapseFacet = nodeTypeFacet.getFacet("collapse");
    if (collapseFacet != null) 
    {
    	UIGraphic collapseImg = (UIGraphic)collapseFacet;
        collapseImgSrc = context.getApplication().getViewHandler().getResourceURL(context, collapseImg.getUrl());
        if (collapseImg.isRendered())
        {
            collapseImg.setId(IMAGE_PREFIX + NODE_STATE_CLOSED);
            collapseImg.setParent(tree);
            nodeImageId = collapseImg.getClientId(context);
            nodeImgFacet = collapseFacet;
        }
    }

    image.setParent(tree);
    if (node.getChildCount() > 0)
    {
        String onClick = new StringBuffer()
            .append("treeNavClick('")
            .append(spanId)
            .append("', '")
            .append(image.getClientId(context))
            .append("', '")
            .append(navSrc)
            .append("', '")
            .append(altSrc)
            .append("', '")
            .append(nodeImageId)
            .append("', '")
            .append(expandImgSrc)
            .append("', '")
            .append(collapseImgSrc)
            .append("', '")
            .append(tree.getId())
            .append("', '")
            .append(nodeId)
            .append("');")
            .toString();

        imageAttrs.put(HTML.ONCLICK_ATTR, onClick);
        imageAttrs.put(HTML.STYLE_ATTR, "cursor:hand;cursor:pointer");
    }
    RendererUtils.renderChild(context, image);

}
else
{
//	System.out.println("not client side toggle");
	
    if (node.getChildCount() > 0)
    {  	
        // set up the expand control and remove whatever children (if any) this control had previously
        UICommand expandControl = tree.getExpandControl();
        expandControl.getChildren().clear();
        expandControl.setId(TOGGLE_ID);

        UIParameter param = new UIParameter();
        param.setName(tree.getId() + NamingContainer.SEPARATOR_CHAR + NAV_COMMAND);
        param.setValue(tree.getNodeId());
        expandControl.getChildren().add(param);
        expandControl.getChildren().add(image);

        RendererUtils.renderChild(context, expandControl);
    }
    else
    {
    	RendererUtils.renderChild(context, image);
    }
}
//out.endElement(HTML.TD_ELEM);

return nodeImgFacet;
}

}