package com.idega.content.tree;

import java.io.IOException;
import java.util.List;

import javax.faces.component.UIComponent;
import javax.faces.component.UIViewRoot;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;

import org.apache.myfaces.custom.tree2.HtmlTree;
import org.apache.myfaces.custom.tree2.HtmlTreeRenderer;
import org.apache.myfaces.custom.tree2.TreeNode;
import org.apache.myfaces.custom.tree2.TreeState;
import org.apache.myfaces.custom.tree2.TreeWalker;
import org.apache.myfaces.renderkit.html.util.AddResource;
import org.apache.myfaces.renderkit.html.util.AddResourceFactory;
import org.apache.myfaces.shared_tomahawk.renderkit.RendererUtils;
import org.apache.myfaces.shared_tomahawk.renderkit.html.HTML;
import org.apache.myfaces.shared_tomahawk.renderkit.html.HtmlRendererUtils;

/**
 * @author Sean Schofield
 * @author Chris Barlow
 * @author Hans Bergsten (Some code taken from an example in his O'Reilly
 *         JavaServer Faces book. Copied with permission)
 * @version $Revision: 1.6 $ $Date: 2007/10/17 15:05:13 $
 */
public class HtmlTreeRendererNew extends HtmlTreeRenderer {

	private static int node = 0;

	// private String currentNodeId;

	public String getNode() {
		node++;
		return "node" + node;
	}

	public HtmlTreeRendererNew() {
	}

	protected void beforeNodeEncode(FacesContext context, ResponseWriter out, HtmlTree tree) throws IOException {
		out.startElement(HTML.LI_ELEM, tree);
		out.writeAttribute(HTML.ID_ATTR, tree.getDataModel().getNodeById(tree.getNodeId()).getIdentifier(), null);
		/* currentNodeId = */tree.getDataModel().getNodeById(tree.getNodeId()).getIdentifier();
	}

	protected void afterNodeEncode(FacesContext context, ResponseWriter out) throws IOException {
		out.endElement(HTML.LI_ELEM);
	}

	public void encodeChildren(FacesContext context, UIComponent component) throws IOException {

		// component.restoreState(context, component);

		HtmlTree tree = (HtmlTree) component;

		tree.setShowLines(false);

		if (!component.isRendered())
			return;
		if (tree.getValue() == null)
			return;

		ResponseWriter out = context.getResponseWriter();
		String clientId = null;

		if (component.getId() != null && !component.getId().startsWith(UIViewRoot.UNIQUE_ID_PREFIX)) {
			clientId = component.getClientId(context);
		}

		boolean isOuterSpanUsed = false;

		if (clientId != null) {
			isOuterSpanUsed = true;
			out.startElement("span", component);
			// out.writeAttribute("id", clientId, "id");

			out.writeAttribute("id", component.getId(), "id");
			System.out.println("span Id = " + component.getId());

			out.startElement("div", component);
			out.writeAttribute("id", "div_id_" + component.getId(), "id");

		}

		boolean clientSideToggle = tree.isClientSideToggle();
		boolean showRootNode = tree.isShowRootNode();

		TreeState state = tree.getDataModel().getTreeState();

		TreeWalker walker = tree.getDataModel().getTreeWalker();

		walker.reset();
		walker.setTree(tree);

		walker.setCheckState(!clientSideToggle); // walk all nodes in client mode

		out.startElement(HTML.UL_ELEM, tree);

		out.writeAttribute(HTML.ID_ATTR, "page_tree_div", null);
		out.writeAttribute(HTML.CLASS_ATTR, "tree_drag_drop", null);

		if (showRootNode) {
			// encode the tree (starting with the root node)

			if (walker.next()) {
				encodeRoot(context, out, tree, walker);
			}
		}
		else {

			TreeNode rootNode = tree.getNode();
			String rootNodeId = tree.getNodeId();
			if (!state.isNodeExpanded(rootNodeId)) {
				state.toggleExpanded(rootNodeId);
			}

			// now encode each of the nodes in the level immediately below the root
			for (int i = 0; i < rootNode.getChildCount(); i++) {
				if (walker.next()) {
					encodeRoot(context, out, tree, walker);
				}
			}
		}

		out.endElement(HTML.UL_ELEM);

		// reset the current node id once we're done encoding everything
		tree.setNodeId(null);

		if (isOuterSpanUsed) {
			out.endElement("div");
			out.endElement("span");
		}
	}

	protected void encodeTree(FacesContext context, ResponseWriter out, HtmlTree tree, TreeWalker walker, TreeNode node) throws IOException {

		boolean clientSideToggle = tree.isClientSideToggle();

		HtmlRendererUtils.writePrettyLineSeparator(context);
		beforeNodeEncode(context, out, tree);
		encodeCurrentNode(context, out, tree, node);

		if (clientSideToggle) {

			// String spanId = TOGGLE_SPAN + ":" + tree.getId() + ":" +
			// tree.getNodeId();
			out.startElement(HTML.SPAN_ELEM, tree);
			// out.writeAttribute(HTML.ID_ATTR, spanId, null);
			out.writeAttribute(HTML.ID_ATTR, tree.getDataModel().getNodeById(tree.getNodeId()).getIdentifier(), null);

			// if (tree.isNodeExpanded()) {
			if (!tree.getDataModel().getNodeById(tree.getNodeId()).isLeaf()) {
				out.writeAttribute(HTML.STYLE_ATTR, "display:block", null);
			}
			else {
				out.writeAttribute(HTML.STYLE_ATTR, "display:none", null);
			}
		}

		if (tree.getDataModel().getNodeById(tree.getNodeId()).getChildCount() > 0) {
			out.startElement(HTML.UL_ELEM, tree);
		}

		if (tree.getDataModel().getNodeById(tree.getNodeId()).isLeaf()) {
			if (clientSideToggle) {
				out.endElement(HTML.SPAN_ELEM);
			}
			out.endElement(HTML.LI_ELEM);
			return;
		}
		else {
			int child_count = tree.getDataModel().getNodeById(tree.getNodeId()).getChildCount();
			List children;
			if (child_count > 0) {
				children = tree.getDataModel().getNodeById(tree.getNodeId()).getChildren();
				for (int i = 0; i < child_count; i++) {
					node = (TreeNode) children.get(i);
					walker.next();
					encodeTree(context, out, tree, walker, node);
				}
			}
			out.endElement(HTML.UL_ELEM);
			if (clientSideToggle) {
				out.endElement(HTML.SPAN_ELEM);
			}
			out.endElement(HTML.LI_ELEM);
		}
	}

	protected void encodeRoot(FacesContext context, ResponseWriter out, HtmlTree tree, TreeWalker walker) throws IOException {

		boolean clientSideToggle = tree.isClientSideToggle();
		boolean expanded = false;

		HtmlRendererUtils.writePrettyLineSeparator(context);

		out.startElement(HTML.LI_ELEM, tree);
		out.writeAttribute(HTML.ID_ATTR, tree.getDataModel().getNodeById(tree.getNodeId()).getIdentifier(), null);
		out.writeAttribute("noDrag", "true", null);

		encodeRootNode(context, out, tree);

		if (clientSideToggle) {
			// String spanId = TOGGLE_SPAN + ":" + tree.getId() + ":" +
			// tree.getNodeId();
			// String spanId = "spanId2" +
			// tree.getDataModel().getNodeById(tree.getNodeId()).getIdentifier();

			out.startElement(HTML.SPAN_ELEM, tree);
			// out.writeAttribute(HTML.ID_ATTR, spanId, null);
			out.writeAttribute(HTML.ID_ATTR, tree.getDataModel().getNodeById(tree.getNodeId()).getIdentifier(), null);
			// if (tree.isNodeExpanded()) {
			if (!tree.getDataModel().getNodeById(tree.getNodeId()).isLeaf()) {
				out.writeAttribute(HTML.STYLE_ATTR, "display:block", null);
			}
			else {
				out.writeAttribute(HTML.STYLE_ATTR, "display:none", null);
			}
		}

		TreeNode node = tree.getNode();

		out.startElement(HTML.UL_ELEM, tree);
		expanded = true;

		int child_count = node.getChildCount();

		for (int i = 0; i < child_count; i++) {
			if (walker.next()) {
				encodeTree(context, out, tree, walker, node);
			}
		}

		if (clientSideToggle) {
			out.endElement(HTML.SPAN_ELEM);
		}
		if (expanded) {
			out.endElement(HTML.UL_ELEM);
		}
		out.endElement(HTML.LI_ELEM);
	}

	/**
	 * Handles the encoding related to the navigation functionality.
	 * 
	 * @param context
	 *          FacesContext
	 * @param out
	 *          ResponseWriter
	 * @param tree
	 *          HtmlTree
	 * @return The additional navigation image to display inside the node (if
	 *         any). Only used with client-side toggle.
	 * @throws IOException
	 */

	private String getImageSrc(FacesContext context, UIComponent component, String imageName, boolean withContextPath) {
		String imageLocation = ((HtmlTree) component).getImageLocation();
		AddResource addResource = AddResourceFactory.getInstance(context);

		// ((HtmlTree)component).setImageLocation("/idegaweb/bundles/com.idega.content.bundle/resources/images/");

		if (imageLocation == null) {
			return addResource.getResourceUri(context, HtmlTreeRenderer.class, "images2/" + imageName, withContextPath);
		}
		else {
			return addResource.getResourceUri(context, imageLocation + "/" + imageName, withContextPath);
		}
	}

	protected void encodeCurrentNode(FacesContext context, ResponseWriter out, HtmlTree tree, TreeNode node) throws IOException {
		UIComponent nodeTypeFacet = tree.getFacet(node.getType());

		if (nodeTypeFacet == null) {
			throw new IllegalArgumentException("Unable to locate facet with the name: " + node.getType());
		}

		// render node padding
		/*String[] pathInfo =*/ tree.getPathInformation(tree.getNodeId());

		RendererUtils.renderChild(context, nodeTypeFacet);
	}

	protected void encodeRootNode(FacesContext context, ResponseWriter out, HtmlTree tree) throws IOException {
		TreeNode node = tree.getNode();

		boolean showRootNode = tree.isShowRootNode();
		boolean showLines = tree.isShowLines();

		UIComponent nodeTypeFacet = tree.getFacet(node.getType());

		if (nodeTypeFacet == null) {
			throw new IllegalArgumentException("Unable to locate facet with the name: " + node.getType());
		}

		String[] pathInfo = tree.getPathInformation(tree.getNodeId());
		int paddingLevel = pathInfo.length - 1;

		for (int i = (showRootNode ? 0 : 1); i < paddingLevel; i++) {
			boolean lastChild = tree.isLastChild(pathInfo[i]);
			String lineSrc = (!lastChild && showLines) ? getImageSrc(context, tree, "line-trunk.gif", true) : getImageSrc(context, tree, "spacer.gif", true);

			out.startElement(HTML.IMG_ELEM, tree);
			out.writeURIAttribute(HTML.SRC_ATTR, lineSrc, null);

			out.endElement(HTML.IMG_ELEM);
		}

		RendererUtils.renderChild(context, nodeTypeFacet);
	}

	/*private UIComponent encodeNavigation(FacesContext context, ResponseWriter out, HtmlTree tree) throws IOException {

		TreeNode node = tree.getNode();
		String nodeId = tree.getNodeId();
		String spanId = TOGGLE_SPAN + ":" + tree.getId() + ":" + nodeId;// TOGGLE_SPAN
																																		// + nodeId;
		boolean showLines = tree.isShowLines();
		boolean clientSideToggle = tree.isClientSideToggle();
		UIComponent nodeTypeFacet = tree.getFacet(node.getType());
		String navSrc = null;
		String altSrc = null;
		UIComponent nodeImgFacet = null;

		int bitMask = NOTHING;
		// bitMask += (node.isLeaf()) ? NOTHING : CHILDREN;
		// if (bitMask == CHILDREN) // if there are no children, ignore expand state
		// -> more flexible with dynamic tree-structures
		// bitMask += (tree.isNodeExpanded()) ? EXPANDED : NOTHING;
		// bitMask += (tree.isLastChild(tree.getNodeId())) ? LAST : NOTHING;
		// bitMask += (showLines) ? LINES : NOTHING;

		switch (bitMask) {
			case (NOTHING):

			case (LAST):
				navSrc = "spacer.gif"; // space is here
				// navSrc = "line-middle.gif";
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

			case (CHILDREN + EXPANDED + LAST): // is cia ima minusa
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

				throw new IllegalStateException("Encountered a node [" + nodeId + "] + with an illogical state.  " + "Node is expanded but it is also considered a leaf (a leaf cannot be considered expanded.");

			default:
				// catch all for any other combinations
				throw new IllegalArgumentException("Invalid bit mask of " + bitMask);
		}

		// adjust navSrc and altSrc so that the images can be retrieved using the
		// extensions filter
		String navSrcUrl = getImageSrc(context, tree, navSrc, false);
		navSrc = getImageSrc(context, tree, navSrc, true);
		altSrc = getImageSrc(context, tree, altSrc, true);

		// render nav cell
		// out.startElement(HTML.TD_ELEM, tree);
		// out.writeAttribute(HTML.WIDTH_ATTR, "19", null);
		// out.writeAttribute(HTML.HEIGHT_ATTR, "100%", null);
		// out.writeAttribute("valign", "top", null);

		if ((bitMask & LINES) != 0 && (bitMask & LAST) == 0) {
			// out.writeURIAttribute("background", getImageSrc(context, tree,
			// "line-trunk.gif", true), null);
		}

		// add the appropriate image for the nav control

		// UIGraphic image = new UIGraphic();
		// image.setId(IMAGE_PREFIX);
		// image.setUrl(navSrcUrl);
		// Map imageAttrs = image.getAttributes();

		// imageAttrs.put(HTML.WIDTH_ATTR, "19");
		// imageAttrs.put(HTML.HEIGHT_ATTR, "18");
		// imageAttrs.put(HTML.BORDER_ATTR, "0");

		if (clientSideToggle) {
			/**
			 * With client side toggle, user has the option to specify open/closed
			 * images for the node (in addition to the navigtion ones provided by the
			 * component.)
			 */
			/*String expandImgSrc = "";
			String collapseImgSrc = "";
			String nodeImageId = "";

			UIComponent expandFacet = nodeTypeFacet.getFacet("expand");
			if (expandFacet != null) {
				UIGraphic expandImg = (UIGraphic) expandFacet;
				expandImgSrc = context.getApplication().getViewHandler().getResourceURL(context, expandImg.getUrl());
				if (expandImg.isRendered()) {
					expandImg.setId(IMAGE_PREFIX + NODE_STATE_EXPANDED);
					expandImg.setParent(tree);
					nodeImageId = expandImg.getClientId(context);
					nodeImgFacet = expandFacet;
				}
			}

			UIComponent collapseFacet = nodeTypeFacet.getFacet("collapse");
			if (collapseFacet != null) {
				UIGraphic collapseImg = (UIGraphic) collapseFacet;
				collapseImgSrc = context.getApplication().getViewHandler().getResourceURL(context, collapseImg.getUrl());
				if (collapseImg.isRendered()) {
					collapseImg.setId(IMAGE_PREFIX + NODE_STATE_CLOSED);
					collapseImg.setParent(tree);
					nodeImageId = collapseImg.getClientId(context);
					nodeImgFacet = collapseFacet;
				}
			}

		}

		return nodeImgFacet;
	}*/

}