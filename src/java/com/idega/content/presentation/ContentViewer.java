package com.idega.content.presentation;

import java.io.IOException;
import java.rmi.RemoteException;
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
public class ContentViewer extends ContentBlock implements ActionListener{

	public static final String PARAMETER_ROOT_FOLDER = "cv_prt";
	
	static String PARAMETER_ACTION = "prm_action";
	private static String BAR = "cv_f_bar";
	public static String LIST = "ac_list";
	private static String FILE_DETAILS = "ac_file_details";
	private static String FILE_DETAILS_LESS = "ac_less_file_details";
	private static String PREVIEW = "ac_preview";
	private static String NEW_FOLDER = "ac_folder";
	private static String PERMISSIONS = "ac_permissions";
	private static String UPLOAD = "ac_upload";
	static String DELETE = "ac_delete";
	static String PATH_TO_DELETE = "ac_path2del";
	
	private String currentAction = null;
	
	private boolean renderListLink = true;
	private boolean renderDetailsLink = false;
	private boolean renderPreviewLink = false;
	private boolean renderNewFolderLink = true;
	private boolean renderPermissionsLink = true;
	
	private boolean renderWebDAVList = true;
	private boolean renderWebDAVFileDetails = false;
	private boolean renderWebDAVFilePreview = false;
	private boolean renderWebDAVNewFolder = false;
	private boolean renderWebDAVFilePermissions = false;
	private boolean renderWebDAVDeleter = false;
	
	private String rootFolder = null;
	
	public ContentViewer() {
		super();
	}
	
	public void initializeContent() {	

		Boolean useUserHomeFolder = (Boolean) this.getAttributes().get("useUserHomeFolder");
		if (useUserHomeFolder != null && useUserHomeFolder.booleanValue()) {
			try {
				rootFolder = super.getIWSlideSession().getUserHomeFolder();
			}
			catch (RemoteException e) {
				e.printStackTrace();
			}
		}
		
		String startFolder = (String) this.getAttributes().get("startFolder");
		if (rootFolder == null) {
			rootFolder = (String) this.getAttributes().get("rootFolder");
		}

		WFBlock listBlock = new WFBlock();
		WFTitlebar tb = new WFTitlebar();
		tb.setValueRefTitle(true);
		tb.setTitleText("WebDAVListBean.webDAVPath");		
		listBlock.setToolbarEmbeddedInTitlebar(true);
		listBlock.setTitlebar(tb);
		listBlock.setToolbar(getToolbar());

		WebDAVList list = new WebDAVList();
		list.setId(getId()+"_list");
		list.setRendered(renderWebDAVList);
		list.setStartFolder(startFolder);
		list.setRootFolder(rootFolder);
		listBlock.add(list);
		
		WFBlock detailsBlock = new WFBlock();
		WFTitlebar detailsBar = new WFTitlebar();
		detailsBar.setTitleText(getBundle().getLocalizedText("document_details"));
		detailsBlock.setTitlebar(detailsBar);
		detailsBlock.setToolbar(getToolbar());
		WebDAVFileDetails details = new WebDAVFileDetails();
		details.setRendered(renderWebDAVFileDetails);
		details.setId(getId()+"_details");
		detailsBlock.add(details);
		
		WFBlock previewBlock = new WFBlock();
		WFTitlebar previewBar = new WFTitlebar();
		previewBar.setTitleText(getBundle().getLocalizedText("document_details"));
		previewBlock.setTitlebar(previewBar);
		previewBlock.setToolbar(getToolbar());
		WebDAVFileDetails details2 = new WebDAVFileDetails();
		details2.setRendered(renderWebDAVFilePreview);
		details2.setId(getId()+"_details");
		details2.setDetailed(false);
		WebDAVFilePreview preview = new WebDAVFilePreview();
		preview.setRendered(renderWebDAVFilePreview);
		preview.setId(getId()+"_preview");
		previewBlock.add(details2);
		
		WFBlock folderBlock = new WFBlock();
		WFTitlebar folderBar = new WFTitlebar();
		folderBar.setTitleText(getBundle().getLocalizedText("create_a_folder"));
		folderBlock.setTitlebar(folderBar);
		folderBlock.setToolbar(new WFToolbar());
		WebDAVFolderCreation folder = new WebDAVFolderCreation();
		folder.setRendered(renderWebDAVNewFolder);
		folder.setId(getId()+"_folder");
		folderBlock.add(folder);
		
		WFBlock uploadBlock = new WFBlock();
		WFTitlebar uploadBar = new WFTitlebar();
		uploadBar.setTitleText(getBundle().getLocalizedText("upload"));
		uploadBlock.setTitlebar(uploadBar);
		uploadBlock.setToolbar(new WFToolbar());
		WebDAVUpload upload = new WebDAVUpload();
//		upload.setRendered(renderWebDAVUpload);
		upload.setId(getId()+"_upload");
		uploadBlock.add(upload);
		
		WFBlock deleteBlock = new WFBlock();
		WFTitlebar deleteBar = new WFTitlebar();
		deleteBar.setTitleText(getBundle().getLocalizedText("delete"));
		deleteBlock.setTitlebar(deleteBar);
		deleteBlock.setToolbar(new WFToolbar());
		
		WebDAVDocumentDeleter deleter = new WebDAVDocumentDeleter();
		deleter.setRendered(renderWebDAVDeleter);
		deleter.setId(getId()+"_deleter");
		deleteBlock.add(deleter);
//		getFacets().put(BAR, bar);
//		getChildren().add(bar);
//		super.setToolbar(bar);
		

		WFBlock permissionsBlock = new WFBlock();
		WFTitlebar permissionsBar = new WFTitlebar();
		permissionsBar.setTitleText(getBundle().getLocalizedText("permissions"));
		permissionsBlock.setTitlebar(permissionsBar);
		permissionsBlock.setToolbar(getToolbar());
		WebDAVFilePermissions permissions = new WebDAVFilePermissions();
		permissions.setRendered(renderWebDAVFilePermissions);
		permissions.setId(getId()+"_permissions");
		permissionsBlock.add(permissions);
		
		getFacets().put(LIST, listBlock);
		getFacets().put(FILE_DETAILS, detailsBlock);
		getFacets().put(FILE_DETAILS_LESS, previewBlock);
		getFacets().put(PREVIEW, preview);
		getFacets().put(PERMISSIONS, permissionsBlock);
		getFacets().put(NEW_FOLDER, folderBlock);
		getFacets().put(UPLOAD, uploadBlock);
		getFacets().put(DELETE, deleteBlock);
		
//		getChildren().add(list);
//		getChildren().add(details);
//		System.out.println("tmp Eiki");
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
			((WebDAVFilePermissions)getFacet(PERMISSIONS)).setResourcePath(tmp);
			rootFolder = tmp;
		}
		if (LIST.equals(currentAction)) { //currentAction == null || 
			
			renderListLink = true;
			renderDetailsLink = false;
			renderPreviewLink = false;
			renderNewFolderLink = true;
			renderPermissionsLink=true;
			
//			super.setToolbar(getToolbar());
		}
		else {
			if (fileSelected.booleanValue()) {
				
				renderListLink = true;
				renderDetailsLink = true;
				renderPreviewLink = true;
				renderNewFolderLink = false;
				renderPermissionsLink=true;
				
				if (currentAction == null) {
					setRenderFlags(FILE_DETAILS);
				}
				else if(PREVIEW.equals(currentAction)){
					setRenderFlags(PREVIEW);
				}
				
//				super.setToolbar(getToolbar());
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
			((WFBlock)list).setToolbar(getToolbar());
		}
		
		UIComponent details = getFacet(FILE_DETAILS);
		if (details != null) {
			details.setRendered(renderWebDAVFileDetails);
			renderChild(context, details);
			((WFBlock)details).setToolbar(getToolbar());
		}

		UIComponent detailsLess = getFacet(FILE_DETAILS_LESS);
		if (detailsLess != null) {
			detailsLess.setRendered(renderWebDAVFilePreview);
			renderChild(context, detailsLess);
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

		UIComponent permissions = getFacet(PERMISSIONS);
		if (permissions != null) {
			permissions.setRendered(renderWebDAVFilePermissions);
			renderChild(context, permissions);
			((WFBlock)permissions).setToolbar(getToolbar());
		}
		
		UIComponent deleter = getFacet(DELETE);
		if (deleter != null) {
			deleter.setRendered(renderWebDAVDeleter);
			renderChild(context, deleter);
		}
		
		UIComponent upload = getFacet(UPLOAD);
		if (upload != null) {
			renderChild(context, upload);
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

		WFToolbarButton permissions = new WFToolbarButton("/images/permissions.gif",getBundle());
		permissions.getAttributes().put(PARAMETER_ACTION, PERMISSIONS);
		permissions.setId(getId()+"_btnPermissions");
		permissions.setToolTip("Permissions");
		permissions.setActionListener(WFUtil.createMethodBinding("#{contentviewerbean.processAction}", new Class[]{ActionEvent.class}));
		permissions.setRendered(renderPermissionsLink);
		
		bar.addButton(newFolder);
		bar.addButton(list);
		bar.addButton(details);
		bar.addButton(preview);
		bar.addButton(permissions);

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
				setRenderFlags(action);
			}
			
		}
	}

	public void setRenderFlags(String action) {
		//System.out.println("[ContentViewer] action = "+action);
		currentAction = action;
		if (LIST.equals(action)) {
			renderWebDAVList = true;
			renderWebDAVFileDetails = false;
			renderWebDAVFilePreview = false;
			renderWebDAVNewFolder = false;
			renderWebDAVFilePermissions = false;
			renderWebDAVDeleter = false;
			WFUtil.invoke("WebDAVListBean","setClickedFilePath", null, String.class);
			
		} else if (FILE_DETAILS.equals(action)) {
			renderWebDAVList = false;
			renderWebDAVFileDetails = true;
			renderWebDAVFilePreview = false;
			renderWebDAVNewFolder = false;
			renderWebDAVFilePermissions = false;
			renderWebDAVDeleter = false;
		} else if (PREVIEW.equals(action)) {
			renderWebDAVList = false;
			renderWebDAVFileDetails = false;
			renderWebDAVFilePreview = true;
			renderWebDAVNewFolder = false;
			renderWebDAVFilePermissions = false;
			renderWebDAVDeleter = false;
		}else if (NEW_FOLDER.equals(action)) {
			renderWebDAVList = true;
			renderWebDAVFileDetails = false;
			renderWebDAVFilePreview = false;
			renderWebDAVNewFolder = true;
			renderWebDAVFilePermissions = false;
		} else if (PERMISSIONS.equals(action)) {
			renderWebDAVList = false;
			renderWebDAVFileDetails = false;
			renderWebDAVFilePreview = false;
			renderWebDAVNewFolder = false;
			renderWebDAVFilePermissions = true;
		}else if (DELETE.equals(action)) {
			renderWebDAVList = true;
			renderWebDAVFileDetails = false;
			renderWebDAVFilePreview = false;
			renderWebDAVNewFolder = false;
			renderWebDAVFilePermissions = false;
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
		values[10] = new Boolean(renderPermissionsLink);
		values[11] = new Boolean(renderWebDAVFilePermissions);
		values[12] = new Boolean(renderWebDAVDeleter);

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
		renderNewFolderLink = ((Boolean) values[8]).booleanValue();
		rootFolder = (String) values[9];
		renderPermissionsLink = ((Boolean) values[10]).booleanValue();
		renderWebDAVFilePermissions = ((Boolean) values[11]).booleanValue();
		renderWebDAVDeleter = ((Boolean) values[12]).booleanValue();
	}
}
