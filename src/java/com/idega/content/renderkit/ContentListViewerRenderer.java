/*
 * $Id: ContentListViewerRenderer.java,v 1.10 2006/05/11 16:09:47 eiki Exp $ Created on
 * 27.1.2005
 * 
 * Copyright (C) 2005 Idega Software hf. All Rights Reserved.
 * 
 * This software is the proprietary information of Idega hf. Use is subject to
 * license terms.
 */
package com.idega.content.renderkit;

import java.io.IOException;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;
import javax.faces.component.UIColumn;
import javax.faces.component.UIComponent;
import javax.faces.component.UIData;
import javax.faces.context.FacesContext;
import javax.faces.context.ResponseWriter;
import org.apache.myfaces.shared_tomahawk.renderkit.JSFAttr;
import org.apache.myfaces.shared_tomahawk.renderkit.RendererUtils;
import org.apache.myfaces.shared_tomahawk.renderkit.html.HTML;
import org.apache.myfaces.shared_tomahawk.renderkit.html.HtmlRendererUtils;
import org.apache.myfaces.shared_tomahawk.util.ArrayUtils;
import org.apache.myfaces.shared_tomahawk.util.StringUtils;
import com.idega.content.presentation.ContentItemListViewer;
import com.idega.util.RenderUtils;
import com.idega.webface.WFContainer;
import com.idega.webface.renderkit.BaseRenderer;

/**
 * 
 * Last modified: $Date: 2006/05/11 16:09:47 $ by $Author: eiki $
 * 
 * @author <a href="mailto:gummi@idega.com">Gudmundur Agust Saemundsson </a>
 * @version $Revision: 1.10 $
 */
public class ContentListViewerRenderer extends BaseRenderer {
	
	public static final String DIV_ELEM = "div";

	/**
	 * Gets the default Logger. By default it uses the package and the class
	 * name to get the logger. <br>
	 * This behaviour can be overridden in subclasses.
	 * 
	 * @return the default Logger
	 */
	protected Logger getLogger() {
		return Logger.getLogger(this.getClass().getName());
	}

	protected void logError(String message) {
		getLogger().log(Level.WARNING, message);
	}

	public boolean getRendersChildren() {
		return true;
	}

	public void encodeBegin(FacesContext facesContext, UIComponent uiComponent) throws IOException {
		RendererUtils.checkParamValidity(facesContext, uiComponent, UIData.class);
		ResponseWriter writer = facesContext.getResponseWriter();
		beforeViewerList(facesContext, (UIData) uiComponent);
		HtmlRendererUtils.writePrettyLineSeparator(facesContext);
		writer.startElement(DIV_ELEM, uiComponent);
		writer.writeAttribute(HTML.ID_ATTR, uiComponent.getClientId(facesContext), null);
		HtmlRendererUtils.renderHTMLAttributes(writer, uiComponent, HTML.COMMON_PASSTROUGH_ATTRIBUTES);
//		renderFacet(facesContext, writer, (UIData) uiComponent, true);
		renderHeader(facesContext,(UIData)uiComponent);
	}

	/**
	 * @param facesContext
	 * @param data
	 * @throws IOException
	 */
	public void renderHeader(FacesContext facesContext, UIData data) throws IOException {
		UIComponent comp = data.getHeader();
		RenderUtils.renderChild(facesContext,comp);
	}
	/**
	 * @param facesContext
	 * @param data
	 * @throws IOException
	 */
	public void renderFooter(FacesContext facesContext, UIData data) throws IOException {
		UIComponent comp = data.getFooter();
		RenderUtils.renderChild(facesContext,comp);
	}

	public void encodeChildren(FacesContext facesContext, UIComponent component) throws IOException {		
		RendererUtils.checkParamValidity(facesContext, component, UIData.class);
		UIData uiData = (UIData) component;
		ResponseWriter writer = facesContext.getResponseWriter();
		
		String savedArticleItemStyleClass = null;
		String firstArticleItemStyleClass = null;
		
		String rowClasses;
		String columnClasses;
		if (component instanceof ContentItemListViewer) {
			rowClasses = ((ContentItemListViewer) component).getRowClasses();
			columnClasses = ((ContentItemListViewer) component).getColumnClasses();
			firstArticleItemStyleClass = ((ContentItemListViewer) component).getFirstArticleItemStyleClass();
		}
		else {
			rowClasses = (String) component.getAttributes().get(JSFAttr.ROW_CLASSES_ATTR);
			columnClasses = (String) component.getAttributes().get(JSFAttr.COLUMN_CLASSES_ATTR);
		}
		
		Styles styles = new Styles(rowClasses, columnClasses);
		int first = uiData.getFirst();
		int rows = uiData.getRows();
		int rowCount = uiData.getRowCount();
		if (rows <= 0) {
			rows = rowCount - first;
		}
		int last = first + rows;
		if (last > rowCount) {
			last = rowCount;
		}
		for (int i = first; i < last; i++) {
			uiData.setRowIndex(i);   
			
			if (!uiData.isRowAvailable()) {
				logError("Row is not available. Rowindex = " + i);
				return;
			}
			beforeViewer(facesContext, uiData);
			
			if (styles.hasRowStyle()) {
				HtmlRendererUtils.writePrettyLineSeparator(facesContext);
				writer.startElement(DIV_ELEM, component);
				String rowStyle = styles.getRowStyle(i);
				writer.writeAttribute(HTML.CLASS_ATTR, rowStyle, null);
			}
			List children = component.getChildren();
			for (int j = 0, size = component.getChildCount(); j < size; j++) {
				UIComponent child = (UIComponent) children.get(j);				
				
				if (child instanceof UIColumn && ((UIColumn) child).isRendered()) {
					if (styles.hasColumnStyle()) {
						HtmlRendererUtils.writePrettyLineSeparator(facesContext);
						writer.startElement(DIV_ELEM, component);
						String columnStyle = styles.getColumnStyle(j);
						writer.writeAttribute(HTML.CLASS_ATTR, columnStyle, null);
					}
					HtmlRendererUtils.writePrettyLineSeparator(facesContext);
					
					//different style classes to article items are assigned;
                    //that will help to style articles 
                    List childrenOfAChild = child.getChildren(); // the same as getting first child...
                    for (int k = 0; k < child.getChildCount(); k++) { // because there is only one child allways 
                        WFContainer article = (WFContainer) childrenOfAChild.get(k);
                        if (i == first) {
                            savedArticleItemStyleClass = article.getStyleClass();                        
                        }
                        StringBuffer buf = new StringBuffer();
                        
                        if (!(firstArticleItemStyleClass == null)) {
                            if (i == first) { // this is first atricle item                                
                                buf.append(firstArticleItemStyleClass);  
                            } else {
                                buf.append(savedArticleItemStyleClass);
                            }
                        } else {
                            buf.append(savedArticleItemStyleClass);
                        }
                        
                        if (i == first) {                            
                            buf.append(" ").append(savedArticleItemStyleClass).append("_first");
                        }
                        if (i == last - 1) { //last                            
                            buf.append(" ").append(savedArticleItemStyleClass).append("_last");
                        }
                        
                        if ((i % 2) == 0) {
                            buf.append(" ").append(savedArticleItemStyleClass).append("_odd");
                        } else {
                            buf.append(" ").append(savedArticleItemStyleClass).append("_even");
                        }
                        
                        article.setStyleClass(buf.toString());
                        
                    }
					
					RendererUtils.renderChild(facesContext, child);
					if (styles.hasColumnStyle()) {
						writer.endElement(DIV_ELEM);
					}
				}
			}
			if (styles.hasRowStyle()) {
				writer.endElement(DIV_ELEM);
			}
			afterViewer(facesContext, uiData);
		}
	}

	/**
	 * Convenient method for derived table renderers.
	 */
	protected void beforeViewerList(FacesContext facesContext, UIData uiData) throws IOException {
	}

	/**
	 * Convenient method for derived table renderers.
	 */
	protected void beforeViewer(FacesContext facesContext, UIData uiData) throws IOException {
	}

	/**
	 * Convenient method for derived table renderers.
	 */
	protected void afterViewer(FacesContext facesContext, UIData uiData) throws IOException {
	}

	/**
	 * Convenient method for derived table renderers.
	 */
	protected void afterViewerList(FacesContext facesContext, UIData uiData) throws IOException {
	}

	public void encodeEnd(FacesContext facesContext, UIComponent uiComponent) throws IOException {
		RendererUtils.checkParamValidity(facesContext, uiComponent, UIData.class);
		ResponseWriter writer = facesContext.getResponseWriter();
		renderFooter(facesContext,(UIData)uiComponent);
//		renderFacet(facesContext, writer, (UIData) uiComponent, false);
		HtmlRendererUtils.writePrettyLineSeparator(facesContext);
		writer.endElement(DIV_ELEM);
		HtmlRendererUtils.writePrettyLineSeparator(facesContext);
		afterViewerList(facesContext, (UIData) uiComponent);
	}

	protected void renderViewerListHeaderRow(FacesContext facesContext, ResponseWriter writer, UIData uiData,
			UIComponent headerFacet, String headerStyleClass) throws IOException {
		renderViewerListHeaderOrFooterRow(facesContext, writer, uiData, headerFacet, headerStyleClass, DIV_ELEM);
	}

	protected void renderViewerListFooterRow(FacesContext facesContext, ResponseWriter writer, UIData uiData,
			UIComponent footerFacet, String footerStyleClass) throws IOException {
		renderViewerListHeaderOrFooterRow(facesContext, writer, uiData, footerFacet, footerStyleClass, DIV_ELEM);
	}

	protected void renderColumnHeaderRow(FacesContext facesContext, ResponseWriter writer, UIData uiData,
			String headerStyleClass) throws IOException {
		renderColumnHeaderOrFooterRow(facesContext, writer, uiData, headerStyleClass, true);
	}

	protected void renderColumnFooterRow(FacesContext facesContext, ResponseWriter writer, UIData uiData,
			String footerStyleClass) throws IOException {
		renderColumnHeaderOrFooterRow(facesContext, writer, uiData, footerStyleClass, false);
	}

	private void renderViewerListHeaderOrFooterRow(FacesContext facesContext, ResponseWriter writer, UIData uiData,
			UIComponent facet, String styleClass, String colElementName) throws IOException {
//		HtmlRendererUtils.writePrettyLineSeparator(facesContext);
//		writer.startElement(HTML.TR_ELEM, uiData);
//		writer.startElement(colElementName, uiData);
//		if (colElementName.equals(HTML.TH_ELEM)) {
//			writer.writeAttribute(HTML.SCOPE_ATTR, HTML.SCOPE_COLGROUP_VALUE, null);
//		}
//		writer.writeAttribute(HTML.COLSPAN_ATTR, new Integer(colspan), null);
//		if (styleClass != null) {
//			writer.writeAttribute(HTML.CLASS_ATTR, styleClass, null);
//		}
//		if (facet != null) {
//			RendererUtils.renderChild(facesContext, facet);
//		}
//		writer.endElement(colElementName);
//		writer.endElement(HTML.TR_ELEM);
	}

	private void renderColumnHeaderOrFooterRow(FacesContext facesContext, ResponseWriter writer, UIData uiData,
			String styleClass, boolean header) throws IOException {
//		HtmlRendererUtils.writePrettyLineSeparator(facesContext);
//		writer.startElement(HTML.TR_ELEM, uiData);
//		for (Iterator it = uiData.getChildren().iterator(); it.hasNext();) {
//			UIComponent uiComponent = (UIComponent) it.next();
//			if (uiComponent instanceof UIColumn && ((UIColumn) uiComponent).isRendered()) {
//				if (header) {
//					renderColumnHeaderCell(facesContext, writer, (UIColumn) uiComponent, styleClass);
//				}
//				else {
//					renderColumnFooterCell(facesContext, writer, (UIColumn) uiComponent, styleClass);
//				}
//			}
//		}
//		writer.endElement(HTML.TR_ELEM);
	}

	protected void renderColumnHeaderCell(FacesContext facesContext, ResponseWriter writer, UIColumn uiColumn,
			String headerStyleClass) throws IOException {
//		writer.startElement(HTML.TH_ELEM, uiColumn);
//		if (headerStyleClass != null) {
//			writer.writeAttribute(HTML.CLASS_ATTR, headerStyleClass, null);
//		}
//		UIComponent facet = uiColumn.getHeader();
//		if (facet != null) {
//			RendererUtils.renderChild(facesContext, facet);
//		}
//		writer.endElement(HTML.TH_ELEM);
	}

	protected void renderColumnFooterCell(FacesContext facesContext, ResponseWriter writer, UIColumn uiColumn,
			String footerStyleClass) throws IOException {
//		writer.startElement(HTML.TD_ELEM, uiColumn);
//		if (footerStyleClass != null) {
//			writer.writeAttribute(HTML.CLASS_ATTR, footerStyleClass, null);
//		}
//		UIComponent facet = uiColumn.getFooter();
//		if (facet != null) {
//			RendererUtils.renderChild(facesContext, facet);
//		}
//		writer.endElement(HTML.TD_ELEM);
	}

	//-------------------------------------------------------------
	// Helper class Styles
	//-------------------------------------------------------------
	private static class Styles {

		//~ Instance fields
		// ------------------------------------------------------------------------
		private String[] _columnStyle;

		private String[] _rowStyle;

		//~ Constructors
		// ---------------------------------------------------------------------------
		Styles(String rowStyles, String columnStyles) {
			this._rowStyle = (rowStyles == null) ? ArrayUtils.EMPTY_STRING_ARRAY
					: StringUtils.trim(StringUtils.splitShortString(rowStyles, ','));
			this._columnStyle = (columnStyles == null) ? ArrayUtils.EMPTY_STRING_ARRAY
					: StringUtils.trim(StringUtils.splitShortString(columnStyles, ','));
		}

		public String getRowStyle(int idx) {
			if (!hasRowStyle()) {
				return null;
			}
			return this._rowStyle[idx % this._rowStyle.length];
		}

		public String getColumnStyle(int idx) {
			if (!hasColumnStyle()) {
				return null;
			}
			return this._columnStyle[idx % this._columnStyle.length];
		}

		public boolean hasRowStyle() {
			return this._rowStyle.length > 0;
		}

		public boolean hasColumnStyle() {
			return this._columnStyle.length > 0;
		}
	}
}