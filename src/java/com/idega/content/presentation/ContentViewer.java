package com.idega.content.presentation;

import java.io.IOException;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.el.ValueBinding;
import javax.faces.event.AbortProcessingException;
import javax.faces.event.ActionEvent;

import com.idega.webface.WFBlock;
import com.idega.webface.WFToolbar;
import com.idega.webface.WFToolbarButton;
import com.idega.webface.WFUtil;

/**
 * @author gimmi
 */
public class ContentViewer extends WFBlock {

	private static String PARAMETER_ACTION = "prm_action";
	private static String BAR = "cv_f_bar";
	private static String LIST = "ac_list";
	private static String FILE_DETAILS = "ac_file_details";
	private static String PREVIEW = "ac_preview";
	
	private String currentAction = null;
	
	private boolean renderListLink = true;
	private boolean renderDetailsLink = false;
	private boolean renderPreviewLink = false;
	
	private Boolean renderWebDAVList;
	private Boolean renderWebDAVFileDetails;
	private Boolean renderWebDAVFilePreview;
	
	public ContentViewer() {
		super();
	}
	
	public void initializeContent() {
		renderWebDAVList = new Boolean(true);
		renderWebDAVFileDetails = new Boolean(false);
		
//		setToolbarEmbeddedInTitlebar(false);
		
//		setToolbar();
		setToolbarEmbeddedInTitlebar(false);
		
		WebDAVList list = new WebDAVList();
		list.setId(getId()+"_list");
		list.setRendered(renderWebDAVList.booleanValue());
		
		WebDAVFileDetails details = new WebDAVFileDetails();
		details.setRendered(renderWebDAVFileDetails.booleanValue());
		details.setId(getId()+"_details");
		
//		getFacets().put(BAR, bar);
//		getChildren().add(bar);
//		super.setToolbar(bar);
		
		getFacets().put(LIST, list);
		getFacets().put(FILE_DETAILS, details);
//		getChildren().add(list);
//		getChildren().add(details);
	}
	
	public Boolean getRenderWebDAVList() {
    if (renderWebDAVList != null) return renderWebDAVList;
    ValueBinding vb = getValueBinding("renderWebDAVList");
    Boolean v = vb != null ? (Boolean)vb.getValue(getFacesContext()) : null;
    return v != null ? v : null;
	}
	
	public void setRenderWebDAVList(Boolean render) {
		renderWebDAVList = render;
	}
	
	public Boolean getRenderWebDAVFileDetails() {
    if (renderWebDAVFileDetails != null) return renderWebDAVFileDetails;
    ValueBinding vb = getValueBinding("renderWebDAVFileDetails");
    Boolean v = vb != null ? (Boolean)vb.getValue(getFacesContext()) : null;
    return v != null ? v : null;
	}
	
	public void setRenderWebDAVFileDetails(Boolean render) {
		renderWebDAVFileDetails = render;
	}
	
	public void encodeBegin(FacesContext context) throws IOException {
		WFUtil.createValueBinding("#{WebDAVListBean.isClickedFile}");
		Boolean fileSelected = (Boolean) WFUtil.invoke("WebDAVListBean", "getIsClickedFile");
		
		if (LIST.equals(currentAction)) {
			
			renderListLink = true;
			renderDetailsLink = false;
			renderPreviewLink = false;
			
			super.setToolbar(getToolbar());
		}
		else {
			if (fileSelected.booleanValue()) {
				
				renderListLink = true;
				renderDetailsLink = true;
				renderPreviewLink = true;
				
				if (currentAction == null) {
					setRenderFlags(FILE_DETAILS);
				}
				
				super.setToolbar(getToolbar());
			}
		}
		super.encodeBegin(context);
	}
	
	public void encodeChildren(FacesContext context) throws IOException {
		super.encodeChildren(context);

//		UIComponent bar = getFacet(BAR);
//		if (bar != null) {
//			renderChild(context, bar);
//		}

		UIComponent list = getFacet(LIST);
		if (list != null) {
			list.setRendered(renderWebDAVList.booleanValue());
			renderChild(context, list);
		}
		
		UIComponent details = getFacet(FILE_DETAILS);
		if (details != null) {
			details.setRendered(renderWebDAVFileDetails.booleanValue());
			renderChild(context, details);
		}
		
		
	}
		
	public WFToolbar getToolbar() {
		WFToolbar bar = new WFToolbar();
		
		WFToolbarButton list = new WFToolbarButton("list.jpg");
		list.getAttributes().put(PARAMETER_ACTION, LIST);
		list.setId(getId()+"_btnList");
		list.setToolTip("Document List");
		list.setActionListener(WFUtil.createMethodBinding("#{contentviewerbean.processAction}", new Class[]{ActionEvent.class}));
//		list.setValueBinding("rendered", WFUtil.createValueBinding("#{contentviewerbean.renderListLink}"));
		list.setRendered(renderListLink);

		WFToolbarButton details = new WFToolbarButton("details.jpg");
		details.getAttributes().put(PARAMETER_ACTION, FILE_DETAILS);
		details.setId(getId()+"_btnDetails");
		details.setToolTip("Document Details");
		details.setActionListener(WFUtil.createMethodBinding("#{contentviewerbean.processAction}", new Class[]{ActionEvent.class}));
//		details.setValueBinding("rendered", WFUtil.createValueBinding("#{contentviewerbean.renderDetailsLink}"));
		details.setRendered(renderDetailsLink);

		WFToolbarButton preview = new WFToolbarButton("preview.jpg");
		preview.getAttributes().put(PARAMETER_ACTION, PREVIEW);
		preview.setId(getId()+"_btnPreview");
		preview.setToolTip("Document Preview");
		preview.setActionListener(WFUtil.createMethodBinding("#{contentviewerbean.processAction}", new Class[]{ActionEvent.class}));
//		preview.setValueBinding("rendered", WFUtil.createValueBinding("#{contentviewerbean.renderPreviewLink}"));
		preview.setRendered(renderPreviewLink);

		bar.addButton(list);
		bar.addButton(details);
		bar.addButton(preview);

//		super.setToolbar(bar);
//		getChildren().add(bar);
		return bar;
	}

	public void processAction(ActionEvent actionEvent) throws AbortProcessingException {
		Object source = actionEvent.getSource();
		if (source instanceof WFToolbarButton) {
			WFToolbarButton bSource = (WFToolbarButton) source;
			String action = (String) bSource.getAttributes().get(PARAMETER_ACTION);
			if (action != null) {
				currentAction = action;
				setRenderFlags(action);
			}
		}
	}

	private void setRenderFlags(String action) {
		System.out.println("[ContentViewer] action = "+action);
		if (LIST.equals(action)) {
			setRenderWebDAVList( new Boolean(true));
			setRenderWebDAVFileDetails(new Boolean(false));
			renderWebDAVFilePreview = new Boolean(false);
		} else if (FILE_DETAILS.equals(action)) {
			setRenderWebDAVList( new Boolean(false));
			setRenderWebDAVFileDetails(new Boolean(true));
			renderWebDAVFilePreview = new Boolean(false);
		} else if (PREVIEW.equals(action)) {
			setRenderWebDAVList( new Boolean(false));
			setRenderWebDAVFileDetails(new Boolean(false));
			renderWebDAVFilePreview = new Boolean(true);
		}
	}
	
	public Object saveState(FacesContext ctx) {
		Object values[] = new Object[10];
		values[0] = super.saveState(ctx);
		values[1] = renderWebDAVList;
		values[2] = renderWebDAVFileDetails;
		values[3] = renderWebDAVFilePreview;
		values[4] = new Boolean(renderListLink);
		values[5] = new Boolean(renderDetailsLink);
		values[6] = new Boolean(renderPreviewLink);
		return values;
	}

	public void restoreState(FacesContext ctx, Object state) {
		Object values[] = (Object[]) state;
		super.restoreState(ctx, values[0]);
		renderWebDAVList = (Boolean) values[1];
		renderWebDAVFileDetails = (Boolean) values[2];
		renderWebDAVFilePreview = (Boolean) values[3];
		renderListLink = ((Boolean) values[4]).booleanValue();
		renderDetailsLink = ((Boolean) values[5]).booleanValue();
		renderPreviewLink = ((Boolean) values[6]).booleanValue();
	}
}
