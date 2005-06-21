package com.idega.content.presentation;

import java.io.IOException;
import java.rmi.RemoteException;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Vector;
import javax.faces.component.UIComponent;
import javax.faces.component.UIParameter;
import javax.faces.component.html.HtmlCommandLink;
import javax.faces.context.FacesContext;
import javax.faces.event.AbortProcessingException;
import javax.faces.event.ActionEvent;
import javax.faces.event.ActionListener;
import com.idega.business.IBOLookup;
import com.idega.business.IBOLookupException;
import com.idega.content.business.WebDAVFilePermissionResource;
import com.idega.idegaweb.IWUserContext;
import com.idega.idegaweb.UnavailableIWContext;
import com.idega.presentation.IWContext;
import com.idega.slide.business.IWSlideSession;
import com.idega.slide.util.IWSlideConstants;
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
	
	public static final String PARAMETER_ACTION = "iw_content_action";
	public static final String PARAMETER_CONTENT_RESOURCE = "iw_content_rs_url";
	
	public static final String ACTION_LIST = "ac_list";
	public static final String ACTION_FILE_DETAILS = "ac_file_details";
	public static final String ACTION_FILE_DETAILS_LESS = "ac_less_file_details";
	public static final String ACTION_PREVIEW = "ac_preview";
	public static final String ACTION_PERMISSIONS = "ac_permissions";
	public static final String ACTION_NEW_FOLDER = "ac_folder";
	public static final String ACTION_UPLOAD = "ac_upload";
	public static final String ACTION_DELETE = "ac_delete";
	
	static final String PATH_TO_DELETE = "ac_path2del";
	
	private static String BAR = "cv_f_bar";
	
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
	private boolean renderWebDAVUploadeComponent = true;
	
	private String rootFolder = null;
	private boolean useUserHomeFolder = false;
	private String startFolder = null;
	private String iconTheme = null;
	private boolean showFolders = true;
	private boolean showPublicFolder = true;
	private boolean showDropboxFolder = true;
	private Collection columnsToHide = null;
	private boolean maintainPath = false;
	private boolean useVersionControl = true;
	private boolean showPermissionTab = true;
	private boolean showUploadComponent = true;
	private String onFileClickEvent = null;
	
	private String currentFolderPath = null;
	private String currentFileName = null;
	private String currentResourceName = null;

	public ContentViewer() {
		super();
	}
	
	public void initializeContent() {	

//		String startFolder = (String) this.getAttributes().get("startFolder");
//		String iconTheme = (String) this.getAttributes().get("iconTheme");
//		rootFolder ="/files/shared";

		WFBlock listBlock = new WFBlock();
		WFTitlebar tb = new WFTitlebar();

		tb.addTitleText(getBundle().getLocalizedText("document_list"));
		tb.addTitleText(getCurrentResourceName());
		tb.setToolTip(getCurrentFolderPath());
//		tb.addTitleText("WebDAVListBean.virtualWebDAVPath", true);
//		tb.addTitleText("WebDAVListBean.webDAVPath", true);
//		listBlock.setToolbarEmbeddedInTitlebar(true);
		listBlock.setTitlebar(tb);
		listBlock.setToolbar(getToolbar());

		WebDAVList list = new WebDAVList();
		list.setId(getId()+"_list");
		list.setRendered(renderWebDAVList);
		list.setStartFolder(startFolder);
		list.setRootFolder(rootFolder);
		list.setIconTheme(iconTheme);
		list.setShowFolders(showFolders);
		list.setShowDropboxFolder(showDropboxFolder);
		list.setShowPublicFolder(showPublicFolder);
		list.setColumnsToHide(columnsToHide);
		list.setUseVersionControl(useVersionControl);
		list.setOnFileClickEvent(onFileClickEvent);
		listBlock.add(list);
		
		WFBlock detailsBlock = new WFBlock();
		WFTitlebar detailsBar = new WFTitlebar();
		detailsBar.addTitleText(getBundle().getLocalizedText("document_details"));
		detailsBar.addTitleText(" (");
		detailsBar.addTitleText(getCurrentFileName());
		detailsBar.addTitleText(")");
		detailsBlock.setTitlebar(detailsBar);
		detailsBlock.setToolbar(getToolbar());
		WebDAVFileDetails details = new WebDAVFileDetails();
		details.setRendered(renderWebDAVFileDetails);
		details.setId(getId()+"_details");
		details.setUseVersionControl(useVersionControl);
		detailsBlock.add(details);
		
		WFBlock previewBlock = new WFBlock();
		WFTitlebar previewBar = new WFTitlebar();
		previewBar.addTitleText(getBundle().getLocalizedText("document_details"));
		previewBar.addTitleText(" (");
		previewBar.addTitleText(getCurrentFileName());
		previewBar.addTitleText(")");
		previewBlock.setTitlebar(previewBar);
		previewBlock.setToolbar(getToolbar());
		WebDAVFileDetails details2 = new WebDAVFileDetails();
		details2.setRendered(renderWebDAVFilePreview);
		details2.setId(getId()+"_details");
		details2.setDetailed(false);
		details2.setUseVersionControl(useVersionControl);
		WebDAVFilePreview preview = new WebDAVFilePreview();
		preview.setRendered(renderWebDAVFilePreview);
		preview.setId(getId()+"_preview");
		previewBlock.add(details2);
		
		WFBlock folderBlock = new WFBlock();
		WFTitlebar folderBar = new WFTitlebar();
		folderBar.addTitleText(getBundle().getLocalizedText("create_a_folder"));
		folderBar.addTitleText(getCurrentResourceName());
		folderBlock.setTitlebar(folderBar);
		folderBlock.setToolbar(new WFToolbar());
		WebDAVFolderCreation folder = new WebDAVFolderCreation();
		folder.setRendered(renderWebDAVNewFolder);
		folder.setId(getId()+"_folder");
		folderBlock.add(folder);
		
		WFBlock uploadBlock = new WFBlock();
		WFTitlebar uploadBar = new WFTitlebar();
		uploadBar.addTitleText(getBundle().getLocalizedText("upload"));
		uploadBar.addTitleText(getCurrentResourceName());
		uploadBlock.setTitlebar(uploadBar);
		//uploadBlock.setToolbar(new WFToolbar());
		WebDAVUpload upload = new WebDAVUpload();
//		upload.setRendered(renderWebDAVUpload);
		upload.setId(getId()+"_upload");
		uploadBlock.add(upload);
		
		WFBlock deleteBlock = new WFBlock();
		WFTitlebar deleteBar = new WFTitlebar();
		deleteBar.addTitleText(getBundle().getLocalizedText("delete"));
		deleteBar.addTitleText(getCurrentResourcePath());
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
		permissionsBar.addTitleText(getBundle().getLocalizedText("permissions"));
		permissionsBar.addTitleText(getCurrentResourceName());
		permissionsBlock.setTitlebar(permissionsBar);
		permissionsBlock.setToolbar(getToolbar());
		WebDAVFilePermissions permissions = new WebDAVFilePermissions();
		permissions.setRendered(renderWebDAVFilePermissions);
		permissions.setId(getId()+"_permissions");
		permissionsBlock.add(permissions);
		
		getFacets().put(ACTION_LIST, listBlock);
		getFacets().put(ACTION_FILE_DETAILS, detailsBlock);
		getFacets().put(ACTION_FILE_DETAILS_LESS, previewBlock);
		getFacets().put(ACTION_PREVIEW, preview);
		getFacets().put(ACTION_PERMISSIONS, permissionsBlock);
		getFacets().put(ACTION_NEW_FOLDER, folderBlock);
		getFacets().put(ACTION_UPLOAD, uploadBlock);
		getFacets().put(ACTION_DELETE, deleteBlock);
		
	}
	
	public void setRootFolder(String rootFolder) {
		this.rootFolder = rootFolder;
	}
	
	public void setUseUserHomeFolder(boolean useUserHomeFolder) {
		this.useUserHomeFolder = useUserHomeFolder;
	}
	
	public void setStartFolder(String startFolder) {
		this.startFolder = startFolder;
	}
	
	public void setIconTheme(String iconTheme) {
		this.iconTheme = iconTheme;
	}
	
	public void setShowFolders(boolean showFolders) {
		this.showFolders = showFolders;
	}
	
	public void setShowPublicFolder(boolean showPublicFolder){
		this.showPublicFolder = showPublicFolder;
	}
	
	public void setShowDropboxFolder(boolean showDropboxFolder){
		this.showDropboxFolder = showDropboxFolder;
	}
	
	public void setColumnsToHide(String columns) {
		if (columns != null) {
			Collection v = new Vector();
			int index = columns.indexOf(",");
			while (index > -1) {
				String tmp = columns.substring(0, index);
				v.add(tmp.trim());
				columns = columns.substring(index+1);
				index = columns.indexOf(",");
			}
			v.add(columns.trim());
			
			this.columnsToHide = v;
		}
	}
	
	
	
	/* (non-Javadoc)
	 * @see javax.faces.component.UIComponent#decode(javax.faces.context.FacesContext)
	 */
	public void decode(FacesContext context) {
		//super.decode(arg0);
		//TODO USE DECODE, DOES NOT WORK BECAUSE IT IS NEVER CALLED!
		
		Map parameters = context.getExternalContext().getRequestMap();
		
		
		String action = (String) parameters.get(PARAMETER_ACTION);
		String resourceURL = (String) parameters.get(PARAMETER_CONTENT_RESOURCE);
		if(resourceURL!=null){
			setCurrentResourcePath(resourceURL);
		}
		
		if(action!=null){
			setRenderFlags(action);
			
			if(ACTION_PERMISSIONS.equals(action)){
				IWContext iwc = IWContext.getInstance();
				try {
					WebDAVFilePermissionResource resource = (WebDAVFilePermissionResource) IBOLookup.getSessionInstance(iwc, WebDAVFilePermissionResource.class);
					resource.clear();
				}
				catch (IBOLookupException e) {
					e.printStackTrace();
				}
				catch (RemoteException e) {
					e.printStackTrace();
				}
			}
			
			maintainPath(true);
		
		}

	}
	
	public boolean doRenderPermissionLink(){
		if(renderPermissionsLink){
			try {
				IWContext iwc = IWContext.getInstance();
				IWSlideSession session = (IWSlideSession)IBOLookup.getSessionInstance(iwc,IWSlideSession.class);
				return session.hasPermission(getCurrentResourcePath(),IWSlideConstants.PRIVILEGE_READ_ACL);
			}
			catch (IBOLookupException e) {
				e.printStackTrace();
			}
			catch (UnavailableIWContext e) {
				e.printStackTrace();
			}
			catch (RemoteException e) {
				e.printStackTrace();
			}        		
		}
		return false;
	}
	
	public boolean doRenderUploadeComponent(){
		if(renderWebDAVUploadeComponent){
			try {
				IWContext iwc = IWContext.getInstance();
				IWSlideSession session = (IWSlideSession)IBOLookup.getSessionInstance(iwc,IWSlideSession.class);
				return session.hasPermission(getCurrentResourcePath(),IWSlideConstants.PRIVILEGE_WRITE);
			}
			catch (IBOLookupException e) {
				e.printStackTrace();
			}
			catch (UnavailableIWContext e) {
				e.printStackTrace();
			}
			catch (RemoteException e) {
				e.printStackTrace();
			}        		
		}
		return false;
	}
	
	
	
	public void encodeBegin(FacesContext context) throws IOException {
//		Boolean useUserHomeFolder = (Boolean) this.getAttributes().get("useUserHomeFolder");
		if (useUserHomeFolder) {
			try {
				rootFolder = super.getIWSlideSession().getUserHomeFolder();
			}
			catch (RemoteException e) {
				e.printStackTrace();
			}
		}
		
		if (rootFolder == null) {
			rootFolder = (String) this.getAttributes().get("rootFolder");
		}
		
		Map parameters = context.getExternalContext().getRequestParameterMap();
		
		String action = (String) parameters.get(PARAMETER_ACTION);
		String resourceURL = (String) parameters.get(PARAMETER_CONTENT_RESOURCE);
		if(resourceURL!=null){
			setCurrentResourcePath(resourceURL);
		}
		
		if(action!=null){
			setRenderFlags(action);
			
			if(ACTION_PERMISSIONS.equals(action)){
				IWContext iwc = IWContext.getInstance();
				try {
					WebDAVFilePermissionResource resource = (WebDAVFilePermissionResource) IBOLookup.getSessionInstance(iwc, WebDAVFilePermissionResource.class);
					resource.clear();
				}
				catch (IBOLookupException e) {
					e.printStackTrace();
				}
				catch (RemoteException e) {
					e.printStackTrace();
				}
			}
			
			maintainPath(true);
		
		}

		if (!maintainPath) {
			WFUtil.invoke("WebDAVListBean", "resetSorter");
			WFUtil.invoke("WebDAVListBean", "setClickedFilePath", null, String.class);
			WFUtil.invoke("WebDAVListBean", "setWebDAVPath", rootFolder, String.class);
			WFUtil.invoke("WebDAVListBean", "setRootFolder", rootFolder, String.class);
		}
		
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
		if (ACTION_LIST.equals(currentAction)) { //currentAction == null || 
			
			renderListLink = true;
			renderDetailsLink = false;
			renderPreviewLink = false;
			renderNewFolderLink = true;
			renderPermissionsLink=true;
			
		}
		else {
			if (fileSelected.booleanValue()) {
				
				renderListLink = true;
				if (ACTION_PREVIEW.equals(currentAction)) {
					renderDetailsLink = true;
					renderPreviewLink = false;
				} else {
					renderDetailsLink = false;
					renderPreviewLink = true;
				}
				renderNewFolderLink = false;
				renderPermissionsLink=true;
				
				if (currentAction == null) {
					setRenderFlags(ACTION_FILE_DETAILS);
				}
				else if(ACTION_PREVIEW.equals(currentAction)){
					setRenderFlags(ACTION_PREVIEW);
				}
				
			}
		}
		
		if(!getShowPermissionTab()){
//			renderWebDAVFilePermissions = false;
			renderPermissionsLink=false;
		}
		
		if(!getShowUploadComponent()){
			renderWebDAVUploadeComponent = false;
		}
		
		super.encodeBegin(context);
	}
	
	public void encodeChildren(FacesContext context) throws IOException {
		super.encodeChildren(context);

//		UIComponent bar = getFacet(BAR);
//		if (bar != null) {
//			renderChild(context, bar);
//		}

		UIComponent list = getFacet(ACTION_LIST);
		if (list != null) {
			list.setRendered(renderWebDAVList);
			renderChild(context, list);
			((WFBlock)list).setToolbar(getToolbar());
		}
		
		UIComponent details = getFacet(ACTION_FILE_DETAILS);
		if (details != null) {
			details.setRendered(renderWebDAVFileDetails);
			renderChild(context, details);
			((WFBlock)details).setToolbar(getToolbar());
		}

		UIComponent detailsLess = getFacet(ACTION_FILE_DETAILS_LESS);
		if (detailsLess != null) {
			detailsLess.setRendered(renderWebDAVFilePreview);
			renderChild(context, detailsLess);
		}
		
		UIComponent preview = getFacet(ACTION_PREVIEW);
		if (preview != null) {
			preview.setRendered(renderWebDAVFilePreview);
			renderChild(context, preview);
		}
		
		UIComponent folder = getFacet(ACTION_NEW_FOLDER);
		if (folder != null) {
			folder.setRendered(renderWebDAVNewFolder);
			renderChild(context, folder);
		}

		UIComponent permissions = getFacet(ACTION_PERMISSIONS);
		if (permissions != null) {
			permissions.setRendered(renderWebDAVFilePermissions);
			renderChild(context, permissions);
			((WFBlock)permissions).setToolbar(getToolbar());
		}
		
		UIComponent deleter = getFacet(ACTION_DELETE);
		if (deleter != null) {
			deleter.setRendered(renderWebDAVDeleter);
			renderChild(context, deleter);
		}
		
		UIComponent upload = getFacet(ACTION_UPLOAD);
		if (upload != null) {
			upload.setRendered(doRenderUploadeComponent());
			renderChild(context, upload);
		}
	}
		
	public WFToolbar getToolbar() {
		WFToolbar bar = new WFToolbar();
		
		WFToolbarButton list = new WFToolbarButton();
//		WFToolbarButton list = new WFToolbarButton("/images/list.jpg",getBundle());
		list.getAttributes().put(PARAMETER_ACTION, ACTION_LIST);
		list.setId(getId()+"_btnList");
		list.setStyleClass("content_viewer_document_list");
		list.setToolTip(getBundle().getLocalizedString("document_list"));
//		list.setToolTip("Document List");
		list.setActionListener(WFUtil.createMethodBinding("#{contentviewerbean.processAction}", new Class[]{ActionEvent.class}));
//		list.setValueBinding("rendered", WFUtil.createValueBinding("#{contentviewerbean.renderListLink}"));
		list.setRendered(renderListLink);

		WFToolbarButton details = new WFToolbarButton();
//		WFToolbarButton details = new WFToolbarButton("/images/details.jpg",getBundle());
		details.getAttributes().put(PARAMETER_ACTION, ACTION_FILE_DETAILS);
		details.setStyleClass("content_viewer_details");
		details.setId(getId()+"_btnDetails");
//		details.setToolTip("Document Details");
		details.setToolTip(getBundle().getLocalizedString("document_details"));
		details.setDisplayText(getBundle().getLocalizedString("document_details"));
		details.setActionListener(WFUtil.createMethodBinding("#{contentviewerbean.processAction}", new Class[]{ActionEvent.class}));
		details.setRendered(renderDetailsLink);

		WFToolbarButton preview = new WFToolbarButton();
//		WFToolbarButton preview = new WFToolbarButton("/images/preview.jpg",getBundle());
		preview.getAttributes().put(PARAMETER_ACTION, ACTION_PREVIEW);
		preview.setId(getId()+"_btnPreview");
		preview.setStyleClass("content_viewer_preview");
		preview.setToolTip(getBundle().getLocalizedString("preview"));
		preview.setDisplayText(getBundle().getLocalizedString("preview"));
//		preview.setToolTip("Preview");
		preview.setActionListener(WFUtil.createMethodBinding("#{contentviewerbean.processAction}", new Class[]{ActionEvent.class}));
		preview.setRendered(renderPreviewLink);
		
		WFToolbarButton newFolder = new WFToolbarButton();
		newFolder.getAttributes().put(PARAMETER_ACTION, ACTION_NEW_FOLDER);
		newFolder.setId(getId()+"_btnNewFolder");
		newFolder.setStyleClass("content_viewer_new_folder");
		newFolder.setToolTip(getBundle().getLocalizedString("create_a_folder"));
		newFolder.setDisplayText(getBundle().getLocalizedString("create_a_folder"));
		newFolder.setActionListener(WFUtil.createMethodBinding("#{contentviewerbean.processAction}", new Class[]{ActionEvent.class}));
		newFolder.setRendered(showFolders && renderNewFolderLink);

		WFToolbarButton permissions = new WFToolbarButton();
//		WFToolbarButton permissions = new WFToolbarButton("/images/permissions.gif",getBundle());
		permissions.getAttributes().put(PARAMETER_ACTION, ACTION_PERMISSIONS);
		permissions.setId(getId()+"_btnPermissions");
		permissions.setStyleClass("content_viewer_permissions");
//		permissions.setToolTip("Permissions");
		permissions.setToolTip(getBundle().getLocalizedString("permissions"));
		permissions.setDisplayText(getBundle().getLocalizedString("permissions"));
		permissions.setActionListener(WFUtil.createMethodBinding("#{contentviewerbean.processAction}", new Class[]{ActionEvent.class}));
		permissions.setRendered(doRenderPermissionLink());
		
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
				if(ACTION_PERMISSIONS.equals(action)){
					IWContext iwc = IWContext.getInstance();
					try {
						WebDAVFilePermissionResource resource = (WebDAVFilePermissionResource) IBOLookup.getSessionInstance(iwc, WebDAVFilePermissionResource.class);
						resource.clear();
					}
					catch (IBOLookupException e) {
						e.printStackTrace();
					}
					catch (RemoteException e) {
						e.printStackTrace();
					}
				}
			}
			maintainPath(true);
		} else if (source instanceof HtmlCommandLink){
			String action = (String) ((HtmlCommandLink)source).getAttributes().get(PARAMETER_ACTION);
			if (ACTION_DELETE.equals(action)) {
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
			maintainPath(true);
		}
	}
	
	protected void maintainPath(boolean maintain) {
		this.maintainPath = maintain;
	}
	
	protected boolean getMaintainPath() {
		return maintainPath;
	}

	public void setRenderFlags(String action) {
		//System.out.println("[ContentViewer] action = "+action);
		currentAction = action;
		if (ACTION_LIST.equals(action)) {
			renderWebDAVList = true;
			renderWebDAVFileDetails = false;
			renderWebDAVFilePreview = false;
			renderWebDAVNewFolder = false;
			renderWebDAVFilePermissions = false;
			renderWebDAVDeleter = false;
			renderWebDAVUploadeComponent = true;
			WFUtil.invoke("WebDAVListBean","setClickedFilePath", null, String.class);
			
		} else if (ACTION_FILE_DETAILS.equals(action)) {
			renderWebDAVList = false;
			renderWebDAVFileDetails = true;
			renderWebDAVFilePreview = false;
			renderWebDAVNewFolder = false;
			renderWebDAVFilePermissions = false;
			renderWebDAVDeleter = false;
			renderWebDAVUploadeComponent = true;
		} else if (ACTION_PREVIEW.equals(action)) {
			renderWebDAVList = false;
			renderWebDAVFileDetails = false;
			renderWebDAVFilePreview = true;
			renderWebDAVNewFolder = false;
			renderWebDAVFilePermissions = false;
			renderWebDAVDeleter = false;
			renderWebDAVUploadeComponent = true;
		}else if (ACTION_NEW_FOLDER.equals(action)) {
			renderWebDAVList = true;
			renderWebDAVFileDetails = false;
			renderWebDAVFilePreview = false;
			renderWebDAVNewFolder = true;
			renderWebDAVFilePermissions = false;
			renderWebDAVUploadeComponent = true;
		} else if (ACTION_PERMISSIONS.equals(action)) {
			renderWebDAVList = false;
			renderWebDAVFileDetails = false;
			renderWebDAVFilePreview = false;
			renderWebDAVNewFolder = false;
			renderWebDAVFilePermissions = true;
			renderWebDAVUploadeComponent = false;
		}else if (ACTION_DELETE.equals(action)) {
			renderWebDAVList = true;
			renderWebDAVFileDetails = false;
			renderWebDAVFilePreview = false;
			renderWebDAVNewFolder = false;
			renderWebDAVFilePermissions = false;
			renderWebDAVDeleter = true;
			renderWebDAVUploadeComponent = true;
		}
	}
	
	public Object saveState(FacesContext ctx) {
		Object values[] = new Object[26];
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
		values[13] = currentFolderPath;
		values[14] = currentFileName;
		values[15] = startFolder;
		values[16] = new Boolean(useUserHomeFolder);
		values[17] = new Boolean(showFolders);
		values[18] = columnsToHide;
		values[19] = new Boolean(useVersionControl);
		values[20] = new Boolean(renderWebDAVUploadeComponent);
		values[21] = new Boolean(showPermissionTab);
		values[22] = new Boolean(showUploadComponent);
		values[23] = onFileClickEvent;
		values[24] = new Boolean(showPublicFolder);
		values[25] = new Boolean(showDropboxFolder);

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
		currentFolderPath = ((String)values[13]);
		currentFileName = ((String)values[14]);
		startFolder = ((String) values[15]);
		useUserHomeFolder = ((Boolean) values[16]).booleanValue();
		showFolders = ((Boolean) values[17]).booleanValue();
		columnsToHide = ((Collection) values[18]);
		useVersionControl = ((Boolean) values[19]).booleanValue();
		renderWebDAVUploadeComponent = ((Boolean) values[20]).booleanValue();
		showPermissionTab = ((Boolean) values[21]).booleanValue();
		showUploadComponent = ((Boolean) values[22]).booleanValue();
		onFileClickEvent = ((String) values[23]);
		showPublicFolder = ((Boolean) values[24]).booleanValue();
		showDropboxFolder = ((Boolean) values[25]).booleanValue();
		maintainPath(true);
	}
	/**
	 * @return Returns the currentFileName.
	 */
	public String getCurrentFileName() {
		return currentFileName;
	}
	/**
	 * @param currentFileName The currentFileName to set.
	 */
	public void setCurrentFileName(String currentFileName) {
		this.currentFileName = currentFileName;
	}
	/**
	 * @return Returns the currentFolderPath.
	 */
	public String getCurrentFolderPath() {
		if(currentFolderPath == null){
			FacesContext context = getFacesContext();
			currentFolderPath = (String) WFUtil.createMethodBinding("#{WebDAVListBean.getWebDAVPath}", null).invoke(context,null);
			if(currentFolderPath == null || "".equals(currentFolderPath)){
				currentFolderPath = "/";
			}
		}
		return currentFolderPath;
	}
	/**
	 * @param currentFolderPath The currentFolderPath to set.
	 */
	public void setCurrentFolderPath(String currentFolderPath) {
		this.currentFolderPath = currentFolderPath;
	}
	
	public void setCurrentResourcePath(String resource) {
		super.setCurrentResourcePath(resource);
		int index = resource.lastIndexOf("/");
		if (index > -1) {
			String path = resource.substring(0, index);
			setCurrentFolderPath(path);
			WFUtil.invoke("WebDAVListBean", "setWebDAVPath", path);
			
			if (!resource.endsWith("/")) {
				WFUtil.invoke("WebDAVListBean", "setClickedFilePath", resource);
				String file = resource.substring(index+1);
				setCurrentFileName(file);
			}
		}
		
	}
	
	/**
	 * @return Returns the current resource path.
	 */
	public String getCurrentResourcePath() {
		if (super.currentResourcePath != null) {
			return super.currentResourcePath;
		}
		else {
			String path = getCurrentFolderPath();
			
			String fileName = getCurrentFileName();
			if(fileName != null){
				return path+(("/".equals(path.substring(path.length()-1)))?"":"/")+fileName;
			} else {
				return path;
			}
		}
	}
	
	public String getCurrentResourceName() {
		if (currentResourceName == null) {
			currentResourceName = "";
//			String tmp = null;
//			if (rootFolder != null) {
//				tmp = currentFolderPath.replaceAll(rootFolder, "");
//			} else {
//				tmp = currentFolderPath;
//			}
			int index = currentFolderPath.lastIndexOf("/");
			try {
				if (index >= 0 && !currentFolderPath.equals("/")) {

					currentResourceName = " ("+currentFolderPath.substring(index+1)+")";
				}
			} catch (ArrayIndexOutOfBoundsException e) {
				currentResourceName = "";
			}
		}
		return currentResourceName;
	}
	
	public void setUseVersionControl(boolean useVersionControl) {
		this.useVersionControl = useVersionControl;
	}
	/**
	 * @return Returns the showPermissionTab.
	 */
	public boolean getShowPermissionTab() {
		return showPermissionTab;
	}
	/**
	 * @param showPermissionTab The showPermissionTab to set.
	 */
	public void setShowPermissionTab(boolean showPermissionTab) {
		this.showPermissionTab = showPermissionTab;
	}	
	
	/**
	 * @return Returns the showUploadComponent.
	 */
	public boolean getShowUploadComponent() {
		return showUploadComponent;
	}
	/**
	 * @param showUploadComponent The showUploadComponent to set.
	 */
	public void setShowUploadComponent(boolean showUploadComponent) {
		this.showUploadComponent = showUploadComponent;
	}
	
	/**
	 * Set the onClick event, for a file click
	 * example .setOnFileClickEvent("event([NAME])"); or event([ID]); or just event()
	 * @param event
	 */
	public void setOnFileClickEvent(String event) {
		this.onFileClickEvent = event;
	}
	
}
