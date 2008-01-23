/*
 * $Id: ContentItemViewerRenderer.java,v 1.6 2008/01/23 12:11:59 valdas Exp $
 * Created on 16.2.2005
 *
 * Copyright (C) 2005 Idega Software hf. All Rights Reserved.
 *
 * This software is the proprietary information of Idega hf.
 * Use is subject to license terms.
 */
package com.idega.content.renderkit;

import java.io.IOException;
import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import com.idega.content.presentation.ContentItemViewer;
import com.idega.util.RenderUtils;
import com.idega.webface.renderkit.ContainerRenderer;


/**
 * 
 *  Last modified: $Date: 2008/01/23 12:11:59 $ by $Author: valdas $
 * 
 * @author <a href="mailto:gummi@idega.com">Gudmundur Agust Saemundsson</a>
 * @version $Revision: 1.6 $
 */
public class ContentItemViewerRenderer extends ContainerRenderer {
	
	/*
	 * (non-Javadoc)
	 * 
	 * @see javax.faces.render.Renderer#encodeChildren(javax.faces.context.FacesContext,
	 *      javax.faces.component.UIComponent)
	 */
	public void encodeChildren(FacesContext ctx, UIComponent comp) throws IOException {
		if (!comp.isRendered()) {
			return;
		}
		
		ContentItemViewer viewer = (ContentItemViewer)comp;
		
		renderHeader(ctx,viewer);
		
		renderJavaScriptForComments(ctx, viewer);

		boolean renderToolbarAbove = false;
		if(renderToolbarAbove){
			renderToolbar(ctx,viewer);
		}
		
		renderFields(ctx, viewer);
		
		renderDetailsCommand(ctx,viewer);
		
		if(!renderToolbarAbove){
			renderToolbar(ctx,viewer);
		}
		
		renderFooter(ctx,viewer);
		
		renderComments(ctx, viewer);
		
		renderJavaScriptForFeed(ctx, viewer);
		
		renderJavaScript(ctx, viewer);
	}
	
	public void renderFields(FacesContext context, ContentItemViewer viewer) throws IOException {
		String attr[] = viewer.getViewerFieldNames();
		for (int i = 0; i < attr.length; i++) {
			RenderUtils.renderFacet(context,viewer,viewer.getFacetName(attr[i]));
		}
	}

	/**
	 * @param ctx
	 * @param viewer
	 * @throws IOException
	 */
	public void renderFooter(FacesContext ctx, ContentItemViewer viewer) throws IOException {
		RenderUtils.renderChild(ctx,viewer.getViewerFacetWrapper(ContentItemViewer.FACET_ITEM_FOOTER));
	}

	/**
	 * @param ctx
	 * @param viewer
	 * @throws IOException
	 */
	public void renderHeader(FacesContext ctx, ContentItemViewer viewer) throws IOException {
		RenderUtils.renderChild(ctx,viewer.getViewerFacetWrapper(ContentItemViewer.FACET_ITEM_HEADER));
	}

	/**
	 * @param ctx
	 * @param comp
	 * @throws IOException
	 */
	public void renderDetailsCommand(FacesContext ctx, ContentItemViewer viewer) throws IOException {
		RenderUtils.renderChild(ctx,viewer.getViewerFacetWrapper(ContentItemViewer.FACET_ITEM_DETAILS_COMMAND));
	}

	/**
	 * @param ctx
	 * @param comp
	 * @throws IOException
	 */
	public void renderToolbar(FacesContext ctx, ContentItemViewer viewer) throws IOException {
		UIComponent toolbar = (UIComponent) viewer.getFacets().get(ContentItemViewer.FACET_TOOLBAR);
		RenderUtils.renderChild(ctx,toolbar);
	}
	
	public void renderComments(FacesContext ctx, ContentItemViewer viewer) throws IOException {
		UIComponent comments = (UIComponent) viewer.getFacets().get(ContentItemViewer.FACET_ITEM_COMMENTS);
		if (comments == null) {
			return;
		}
		RenderUtils.renderChild(ctx, comments);
	}
	
	public void renderJavaScriptForComments(FacesContext ctx,ContentItemViewer viewer) throws IOException {
		UIComponent script = (UIComponent) viewer.getFacets().get(ContentItemViewer.FACET_COMMENTS_SCRIPTS);
		if (script == null) {
			return;
		}
		RenderUtils.renderChild(ctx, script);
	}

	public void renderJavaScriptForFeed(FacesContext ctx,ContentItemViewer viewer) throws IOException {
		UIComponent script = (UIComponent) viewer.getFacets().get(ContentItemViewer.FACET_FEED_SCRIPT);
		if (script == null) {
			return;
		}
		RenderUtils.renderChild(ctx, script);
	}
	
	private void renderJavaScript(FacesContext ctx, ContentItemViewer viewer) throws IOException {
		Object o = viewer.getFacets().get(ContentItemViewer.FACET_JAVA_SCRIPT);
		if (o instanceof UIComponent) {
			RenderUtils.renderChild(ctx, (UIComponent) o);
		}
		return;
	}
	
}
