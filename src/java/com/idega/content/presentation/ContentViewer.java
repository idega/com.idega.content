package com.idega.content.presentation;

import java.io.IOException;
import java.util.Iterator;
import java.util.List;
import javax.faces.component.UIComponent;
import javax.faces.component.UIParameter;
import javax.faces.component.html.HtmlCommandLink;
import javax.faces.context.FacesContext;
import javax.faces.event.AbortProcessingException;
import javax.faces.event.ActionEvent;
import javax.faces.event.ActionListener;
import com.idega.business.IBOLookup;
import com.idega.content.business.WebDAVUpload;
import com.idega.idegaweb.IWBundle;
import com.idega.idegaweb.IWUserContext;
import com.idega.presentation.IWContext;
import com.idega.slide.business.IWSlideSession;
import com.idega.webface.WFBlock;
import com.idega.webface.WFTitlebar;
import com.idega.webface.WFToolbar;
import com.idega.webface.WFToolbarButton;
import com.idega.webface.WFUtil;

/**
 * @author gimmi
 */
public class ContentViewer extends WFBlock implements ActionListener{

	public static final String PARAMETER_ROOT_FOLDER = "cv_prt";
	
	static String PARAMETER_ACTION = "prm_action";
	private static String BAR = "cv_f_bar";
	private static String LIST = "ac_list";
	private static String FILE_DETAILS = "ac_file_details";
	private static String PREVIEW = "ac_preview";
	private static String NEW_FOLDER = "ac_folder";
	private static String UPLOAD = "ac_upload";
	static String DELETE = "ac_delete";
	static String PATH_TO_DELETE = "ac_path2del";
	
	private String currentAction = null;
	
	private boolean renderListLink = true;
	private boolean renderDetailsLink = false;
	private boolean renderPreviewLink = false;
	private boolean renderNewFolderLink = true;
	private boolean renderUploadLink = false;
	
	private boolean renderWebDAVList = true;
	private boolean renderWebDAVFileDetails = false;
	private boolean renderWebDAVFilePreview = false;
	private boolean renderWebDAVNewFolder = false;
	private boolean renderWebDAVUpload = false;
	private boolean renderWebDAVDeleter = false;
	
	private String rootFolder = null;
	
	public ContentViewer() {
		super();
	}
	
	protected IWBundle getBundle() {
		return ContentBlock.getBundle();
	}
			
	public void initializeContent() {	

//		String path = (String) WFUtil.invoke("WebDAVListBean", "getWebDAVPath");
//		if (path == null || "".equals(path)) {
//			path = "/"
//		}
		WFTitlebar tb = new WFTitlebar();
		tb.setValueRefTitle(true);
		tb.setTitleText("WebDAVListBean.webDAVPath");
		this.setTitlebar(tb);
		

		setToolbarEmbeddedInTitlebar(false);
		String startFolder = (String) this.getAttributes().get("startFolder");
		if (rootFolder == null) {
			rootFolder = (String) this.getAttributes().get("rootFolder");
		}
		
		WebDAVList list = new WebDAVList();
		list.setId(getId()+"_list");
		list.setRendered(renderWebDAVList);
		list.setStartFolder(startFolder);
		list.setRootFolder(rootFolder);
		
		WebDAVFileDetails details = new WebDAVFileDetails();
		details.setRendered(renderWebDAVFileDetails);
		details.setId(getId()+"_details");
		
		WebDAVFilePreview preview = new WebDAVFilePreview();
		preview.setRendered(renderWebDAVFilePreview);
		preview.setId(getId()+"_preview");
		
		WebDAVFolderCreation folder = new WebDAVFolderCreation();
		folder.setRendered(renderWebDAVNewFolder);
		folder.setId(getId()+"_folder");
		
		WebDAVUpload upload = new WebDAVUpload();
		upload.setRendered(renderWebDAVUpload);
		upload.setId(getId()+"_upload");
		
		WebDAVDocumentDeleter deleter = new WebDAVDocumentDeleter();
		deleter.setRendered(renderWebDAVDeleter);
		deleter.setId(getId()+"_deleter");
		
//		getFacets().put(BAR, bar);
//		getChildren().add(bar);
//		super.setToolbar(bar);
		
		getFacets().put(LIST, list);
		getFacets().put(FILE_DETAILS, details);
		getFacets().put(PREVIEW, preview);
		getFacets().put(NEW_FOLDER, folder);
		getFacets().put(UPLOAD, upload);
		getFacets().put(DELETE, deleter);
//		getChildren().add(list);
//		getChildren().add(details);
	}
	
	public void encodeBegin(FacesContext context) throws IOException {
		Boolean fileSelected = (Boolean) WFUtil.invoke("WebDAVListBean", "getIsClickedFile");

		String tmp = (String) context.getExternalContext().getRequestParameterMap().get(PARAMETER_ROOT_FOLDER);
		if (tmp != null) {
			IWUserContext iwuc = IWContext.getInstance();			
			IWSlideSession ss = (IWSlideSession) IBOLookup.getSessionInstance(iwuc, IWSlideSession.class);
			String webDAVServerURI = ss.getWebdavServerURI();
			tmp = tmp.replaceFirst(webDAVServerURI, "");
			WFUtil.invoke("WebDAVListBean", "setWebDAVPath", tmp);
			rootFolder = tmp;
		}
		if (LIST.equals(currentAction)) {
			
			renderListLink = true;
			renderDetailsLink = false;
			renderPreviewLink = false;
			renderUploadLink = false;
			
			super.setToolbar(getToolbar());
		}
		else {
			if (fileSelected.booleanValue()) {
				
				renderListLink = true;
				renderDetailsLink = true;
				renderPreviewLink = true;
				renderNewFolderLink = false;
				renderUploadLink = false;
				
				if (currentAction == null) {
					setRenderFlags(FILE_DETAILS);
				}
				else if(PREVIEW.equals(currentAction)){
					setRenderFlags(PREVIEW);
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
			list.setRendered(renderWebDAVList);
			renderChild(context, list);
		}
		
		UIComponent details = getFacet(FILE_DETAILS);
		if (details != null) {
			details.setRendered(renderWebDAVFileDetails);
			renderChild(context, details);
		}
		
		UIComponent preview = getFacet(PREVIEW);
		if (preview != null) {
			preview.setRendered(renderWebDAVFilePreview);
			renderChild(context, preview);
		}
		
		UIComponent folder = getFacet(NEW_FOLDER);
		if (folder != null) {
			folder.setRendered(renderWebDAVNewFolder);
			renderChild(context, folder);
		}
		
		UIComponent upload = getFacet(UPLOAD);
		if (upload != null) {
			upload.setRendered(renderWebDAVUpload);
			renderChild(context, upload);
		}

		UIComponent deleter = getFacet(DELETE);
		if (deleter != null) {
			deleter.setRendered(renderWebDAVDeleter);
			renderChild(context, deleter);
		}
		
	}
		
	public WFToolbar getToolbar() {
		WFToolbar bar = new WFToolbar();
		
		WFToolbarButton list = new WFToolbarButton("/images/list.jpg",getBundle());
		list.getAttributes().put(PARAMETER_ACTION, LIST);
		list.setId(getId()+"_btnList");
		list.setToolTip("Document List");
		list.setActionListener(WFUtil.createMethodBinding("#{contentviewerbean.processAction}", new Class[]{ActionEvent.class}));
//		list.setValueBinding("rendered", WFUtil.createValueBinding("#{contentviewerbean.renderListLink}"));
		list.setRendered(renderListLink);

		WFToolbarButton details = new WFToolbarButton("/images/details.jpg",getBundle());
		details.getAttributes().put(PARAMETER_ACTION, FILE_DETAILS);
		details.setId(getId()+"_btnDetails");
		details.setToolTip("Document Details");
		details.setActionListener(WFUtil.createMethodBinding("#{contentviewerbean.processAction}", new Class[]{ActionEvent.class}));
		details.setRendered(renderDetailsLink);

		WFToolbarButton preview = new WFToolbarButton("/images/preview.jpg",getBundle());
		preview.getAttributes().put(PARAMETER_ACTION, PREVIEW);
		preview.setId(getId()+"_btnPreview");
		preview.setToolTip("Document Preview");
		preview.setActionListener(WFUtil.createMethodBinding("#{contentviewerbean.processAction}", new Class[]{ActionEvent.class}));
		preview.setRendered(renderPreviewLink);
		
		WFToolbarButton newFolder = new WFToolbarButton("/images/newfolder.gif",getBundle());
		newFolder.getAttributes().put(PARAMETER_ACTION, NEW_FOLDER);
		newFolder.setId(getId()+"_btnNewFolder");
		newFolder.setToolTip("New Folder");
		newFolder.setActionListener(WFUtil.createMethodBinding("#{contentviewerbean.processAction}", new Class[]{ActionEvent.class}));
		newFolder.setRendered(renderNewFolderLink);
		
		WFToolbarButton upload = new WFToolbarButton("/images/upload.jpg",getBundle());
		upload.getAttributes().put(PARAMETER_ACTION, UPLOAD);
		upload.setId(getId()+"_btnUpload");
		upload.setToolTip("Upload");
		upload.setActionListener(WFUtil.createMethodBinding("#{contentviewerbean.processAction}", new Class[]{ActionEvent.class}));
		upload.setRendered(renderUploadLink);

		bar.addButton(newFolder);
		bar.addButton(upload);
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
		} else if (source instanceof HtmlCommandLink){
			String action = (String) ((HtmlCommandLink)source).getAttributes().get(PARAMETER_ACTION);
			if (DELETE.equals(action)) {
				List children = ((HtmlCommandLink)source).getChildren();
				Iterator iter = children.iterator();
				String path = "unknown";
				while (iter.hasNext()) {
					Object obj = iter.next();
					if ((obj instanceof UIParameter) && PATH_TO_DELETE.equals(((UIParameter) obj).getName()) ) {
						path = (String) ((UIParameter) obj).getValue();
						WFUtil.invoke("WebDAVListBean", "setClickedFilePath", path);
//						path = (String) WFUtil.createMethodBinding("#{WebDAVListBean.getClickedFilePath}", null).invoke(context,null);
					}
				}
//				String path = (String) ((HtmlCommandLink)source).getAttributes().get(PATH_TO_DELETE);
				System.out.println("repps = "+path);
				currentAction = action;
				setRenderFlags(action);
			}
			
		}
	}

	private void setRenderFlags(String action) {
		//System.out.println("[ContentViewer] action = "+action);
		if (LIST.equals(action)) {
			renderWebDAVList = true;
			renderWebDAVFileDetails = false;
			renderWebDAVFilePreview = false;
			renderWebDAVNewFolder = false;
			renderWebDAVUpload = false;
			renderWebDAVDeleter = false;
			WFUtil.invoke("WebDAVListBean","setClickedFilePath", null, String.class);
		} else if (FILE_DETAILS.equals(action)) {
			renderWebDAVList = false;
			renderWebDAVFileDetails = true;
			renderWebDAVFilePreview = false;
			renderWebDAVNewFolder = false;
			renderWebDAVUpload = false;
			renderWebDAVDeleter = false;
		} else if (PREVIEW.equals(action)) {
			renderWebDAVList = false;
			renderWebDAVFileDetails = false;
			renderWebDAVFilePreview = true;
			renderWebDAVNewFolder = false;
			renderWebDAVUpload = false;
			renderWebDAVDeleter = false;
		}else if (NEW_FOLDER.equals(action)) {
			renderWebDAVList = true;
			renderWebDAVFileDetails = false;
			renderWebDAVFilePreview = false;
			renderWebDAVNewFolder = true;
			renderWebDAVUpload = false;
			renderWebDAVDeleter = false;
		}else if (UPLOAD.equals(action)) {
			renderWebDAVList = true;
			renderWebDAVFileDetails = false;
			renderWebDAVFilePreview = false;
			renderWebDAVNewFolder = false;
			renderWebDAVUpload = true;
			renderWebDAVDeleter = false;
		}else if (DELETE.equals(action)) {
			renderWebDAVList = false;
			renderWebDAVFileDetails = false;
			renderWebDAVFilePreview = false;
			renderWebDAVNewFolder = false;
			renderWebDAVUpload = false;
			renderWebDAVDeleter = true;
		}
	}
	
	public Object saveState(FacesContext ctx) {
		Object values[] = new Object[15];
		values[0] = super.saveState(ctx);
		values[1] = new Boolean(renderWebDAVList);
		values[2] = new Boolean(renderWebDAVFileDetails);
		values[3] = new Boolean(renderWebDAVFilePreview);
		values[4] = new Boolean(renderWebDAVNewFolder);
		values[5] = new Boolean(renderListLink);
		values[6] = new Boolean(renderDetailsLink);
		values[7] = new Boolean(renderPreviewLink);
		values[8] = new Boolean(renderNewFolderLink);
		values[9] = rootFolder;
		values[12] = new Boolean(renderWebDAVUpload);
		values[13] = new Boolean(renderUploadLink);
		values[14] = new Boolean(renderWebDAVDeleter);
		return values;
	}

	public void restoreState(FacesContext ctx, Object state) {
		Object values[] = (Object[]) state;
		super.restoreState(ctx, values[0]);
		renderWebDAVList = ((Boolean) values[1]).booleanValue();
		renderWebDAVFileDetails = ((Boolean) values[2]).booleanValue();
		renderWebDAVFilePreview = ((Boolean) values[3]).booleanValue();
		renderWebDAVNewFolder = ((Boolean) values[4]).booleanValue();
		renderListLink = ((Boolean) values[5]).booleanValue();
		renderDetailsLink = ((Boolean) values[6]).booleanValue();
		renderPreviewLink = ((Boolean) values[7]).booleanValue();
		renderPreviewLink = ((Boolean) values[8]).booleanValue();
		rootFolder = (String) values[9];
		renderWebDAVUpload = ((Boolean) values[12]).booleanValue();
		renderUploadLink = ((Boolean) values[13]).booleanValue();
		renderWebDAVDeleter = ((Boolean) values[14]).booleanValue();
	}
}
