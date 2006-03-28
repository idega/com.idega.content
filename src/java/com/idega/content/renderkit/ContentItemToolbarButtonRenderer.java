/*
 * $Id: ContentItemToolbarButtonRenderer.java,v 1.1 2006/03/28 10:11:51 tryggvil Exp $
 * Created on 9.3.2005
 *
 * Copyright (C) 2006 Idega Software hf. All Rights Reserved.
 *
 * This software is the proprietary information of Idega hf.
 * Use is subject to license terms.
 */
package com.idega.content.renderkit;

import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;
import java.util.Iterator;
import javax.faces.component.UIComponent;
import javax.faces.component.UIOutput;
import javax.faces.component.UIParameter;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;
import javax.faces.convert.ConverterException;
import org.apache.myfaces.renderkit.RendererUtils;
import org.apache.myfaces.renderkit.html.HTML;
import org.apache.myfaces.renderkit.html.HtmlLinkRendererBase;
import org.apache.myfaces.renderkit.html.HtmlRendererUtils;


/**
 * <p>
 * Renderer for the ContentItemToolbarButton
 * </p>
 * Last modified: $Date: 2006/03/28 10:11:51 $ by $Author: tryggvil $
 * 
 * @author <a href="mailto:tryggvi@idega.com">Tryggvi Larusson</a>
 * @version $Revision: 1.1 $
 */
public class ContentItemToolbarButtonRenderer extends HtmlLinkRendererBase {

	/* (non-Javadoc)
	 * @see org.apache.myfaces.renderkit.html.HtmlLinkRendererBase#getStyle(javax.faces.context.FacesContext, javax.faces.component.UIComponent)
	 */
	protected String getStyle(FacesContext facesContext, UIComponent link) {
		// TODO Auto-generated method stub
		return super.getStyle(facesContext, link);
	}

	/* (non-Javadoc)
	 * @see org.apache.myfaces.renderkit.html.HtmlLinkRendererBase#getStyleClass(javax.faces.context.FacesContext, javax.faces.component.UIComponent)
	 */
	protected String getStyleClass(FacesContext facesContext, UIComponent link) {
		// TODO Auto-generated method stub
		return super.getStyleClass(facesContext, link);
	}

	/* (non-Javadoc)
	 * @see org.apache.myfaces.renderkit.html.HtmlLinkRendererBase#renderCommandLinkStart(javax.faces.context.FacesContext, javax.faces.component.UIComponent, java.lang.String, java.lang.Object, java.lang.String, java.lang.String)
	 */
	protected void renderCommandLinkStart(FacesContext facesContext, UIComponent component, String clientId, Object value, String style, String styleClass) throws IOException {
		// TODO Auto-generated method stub
		super.renderCommandLinkStart(facesContext, component, clientId, value, style, styleClass);
	}

	/* (non-Javadoc)
	 * @see org.apache.myfaces.renderkit.html.HtmlLinkRendererBase#renderJavaScriptAnchorStart(javax.faces.context.FacesContext, javax.faces.context.ResponseWriter, javax.faces.component.UIComponent, java.lang.String)
	 */
	protected void renderJavaScriptAnchorStart(FacesContext facesContext, ResponseWriter writer, UIComponent component, String clientId) throws IOException {
		// TODO Auto-generated method stub
		super.renderJavaScriptAnchorStart(facesContext, writer, component, clientId);
	}

	/* (non-Javadoc)
	 * @see org.apache.myfaces.renderkit.html.HtmlLinkRendererBase#renderLinkEnd(javax.faces.context.FacesContext, javax.faces.component.UIComponent)
	 */
	protected void renderLinkEnd(FacesContext facesContext, UIComponent component) throws IOException {
		// TODO Auto-generated method stub
		super.renderLinkEnd(facesContext, component);
	}

	/* (non-Javadoc)
	 * @see org.apache.myfaces.renderkit.html.HtmlLinkRendererBase#renderNonJavaScriptAnchorStart(javax.faces.context.FacesContext, javax.faces.context.ResponseWriter, javax.faces.component.UIComponent, java.lang.String)
	 */
	protected void renderNonJavaScriptAnchorStart(FacesContext facesContext, ResponseWriter writer, UIComponent component, String clientId) throws IOException {
		// TODO Auto-generated method stub
		super.renderNonJavaScriptAnchorStart(facesContext, writer, component, clientId);
	}

	/* (non-Javadoc)
	 * @see org.apache.myfaces.renderkit.html.HtmlLinkRendererBase#renderOutputLinkStart(javax.faces.context.FacesContext, javax.faces.component.UIOutput)
	 */
	protected void renderOutputLinkStart(FacesContext facesContext, UIOutput output) throws IOException {
		//super.renderOutputLinkStart(facesContext, output);
        ResponseWriter writer = facesContext.getResponseWriter();

        //calculate href
        String href = RendererUtils.getStringValue(facesContext, output);
        if (output.getChildCount() > 0)
        {
            StringBuffer hrefBuf = new StringBuffer(href);
            addChildParametersToHref(output, hrefBuf,
                                     (href.indexOf('?') == -1), //first url parameter?
                                     writer.getCharacterEncoding());
            href = hrefBuf.toString();
        }
        href = facesContext.getExternalContext().encodeResourceURL(href);    //TODO: or encodeActionURL ?

        //write anchor
        writer.startElement(HTML.ANCHOR_ELEM, output);
        writer.writeAttribute(HTML.ID_ATTR, output.getClientId(facesContext), null);
       // writer.writeURIAttribute(HTML.HREF_ATTR, href, null);
        writeUriAttribute(facesContext,output,writer,href);
        HtmlRendererUtils.renderHTMLAttributes(writer, output, HTML.ANCHOR_PASSTHROUGH_ATTRIBUTES);
        writer.flush();
	}
	
	protected void writeUriAttribute(FacesContext facesContext, UIOutput output,ResponseWriter writer,String href) throws IOException {
		String javascriptUrl = "javascript:openContentEditor('"+href+"')";
		writer.writeURIAttribute(HTML.HREF_ATTR, javascriptUrl, null);
	}

    private void addChildParametersToHref(UIComponent linkComponent,
            StringBuffer hrefBuf,
            boolean firstParameter,
            String charEncoding)
	throws IOException
	{
		for (Iterator it = linkComponent.getChildren().iterator(); it.hasNext(); )
		{
		UIComponent child = (UIComponent)it.next();
		if (child instanceof UIParameter)
		{
		String name = ((UIParameter)child).getName();
		Object value = ((UIParameter)child).getValue();
		
		addParameterToHref(name, value, hrefBuf, firstParameter, charEncoding);
		firstParameter = false;
		}
		}
	}
    
    private static void addParameterToHref(String name, Object value, StringBuffer hrefBuf, boolean firstParameter, String charEncoding)
    throws UnsupportedEncodingException
	{
		if (name == null)
		{
		    throw new IllegalArgumentException("Unnamed parameter value not allowed within command link.");
		}
		
		hrefBuf.append(firstParameter ? '?' : '&');
		hrefBuf.append(URLEncoder.encode(name, charEncoding));
		hrefBuf.append('=');
		if (value != null)
		{
		    //UIParameter is no ConvertibleValueHolder, so no conversion possible
		    hrefBuf.append(URLEncoder.encode(value.toString(), charEncoding));
		}
	}
	
	/* (non-Javadoc)
	 * @see javax.faces.render.Renderer#convertClientId(javax.faces.context.FacesContext, java.lang.String)
	 */
	public String convertClientId(FacesContext context, String clientId) {
		// TODO Auto-generated method stub
		return super.convertClientId(context, clientId);
	}

	/* (non-Javadoc)
	 * @see javax.faces.render.Renderer#decode(javax.faces.context.FacesContext, javax.faces.component.UIComponent)
	 */
	public void decode(FacesContext context, UIComponent component) {
		// TODO Auto-generated method stub
		super.decode(context, component);
	}

	/* (non-Javadoc)
	 * @see javax.faces.render.Renderer#encodeBegin(javax.faces.context.FacesContext, javax.faces.component.UIComponent)
	 */
	public void encodeBegin(FacesContext context, UIComponent component) throws IOException {
		// TODO Auto-generated method stub
		super.encodeBegin(context, component);
	}

	/* (non-Javadoc)
	 * @see javax.faces.render.Renderer#encodeChildren(javax.faces.context.FacesContext, javax.faces.component.UIComponent)
	 */
	public void encodeChildren(FacesContext context, UIComponent component) throws IOException {
		// TODO Auto-generated method stub
		super.encodeChildren(context, component);
	}

	/* (non-Javadoc)
	 * @see javax.faces.render.Renderer#encodeEnd(javax.faces.context.FacesContext, javax.faces.component.UIComponent)
	 */
	public void encodeEnd(FacesContext context, UIComponent component) throws IOException {
		// TODO Auto-generated method stub
		super.encodeEnd(context, component);
	}

	/* (non-Javadoc)
	 * @see javax.faces.render.Renderer#getConvertedValue(javax.faces.context.FacesContext, javax.faces.component.UIComponent, java.lang.Object)
	 */
	public Object getConvertedValue(FacesContext context, UIComponent component, Object submittedValue) throws ConverterException {
		// TODO Auto-generated method stub
		return super.getConvertedValue(context, component, submittedValue);
	}

	/* (non-Javadoc)
	 * @see javax.faces.render.Renderer#getRendersChildren()
	 */
	public boolean getRendersChildren() {
		// TODO Auto-generated method stub
		return super.getRendersChildren();
	}

	
	
}
