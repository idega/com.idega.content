package com.idega.content.presentation;

import java.io.IOException;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Enumeration;
import java.util.List;
import java.util.Vector;

import javax.faces.component.UIColumn;
import javax.faces.component.UICommand;
import javax.faces.component.UIComponent;
import javax.faces.component.UIParameter;
import javax.faces.component.html.HtmlCommandLink;
import javax.faces.component.html.HtmlGraphicImage;
import javax.faces.component.html.HtmlOutputLink;
import javax.faces.component.html.HtmlOutputText;
import javax.faces.event.AbortProcessingException;
import javax.faces.event.ActionEvent;
import javax.faces.event.ActionListener;
import javax.faces.model.DataModel;

import org.apache.commons.httpclient.HttpException;
import org.apache.webdav.lib.WebdavResources;

import com.idega.business.IBOLookup;
import com.idega.content.business.ContentUtil;
import com.idega.content.business.WebDAVBeanComparator;
import com.idega.content.data.WebDAVBean;
import com.idega.core.search.presentation.SearchResults;
import com.idega.idegaweb.IWMainApplication;
import com.idega.idegaweb.IWUserContext;
import com.idega.presentation.IWContext;
import com.idega.slide.business.IWSlideSession;
import com.idega.slide.util.WebdavExtendedResource;
import com.idega.webface.WFList;
import com.idega.webface.WFUtil;
import com.idega.webface.bean.WFListBean;
import com.idega.webface.convert.WFTimestampConverter;
import com.idega.webface.model.WFDataModel;

/**
 * A managed bean for the WebDAVList component
 * @author gimmi
 */
public class WebDAVListManagedBean extends SearchResults implements ActionListener, WFListBean,Serializable {

	private static final long serialVersionUID = 5991054676990358537L;
	
	private static final String P_ID = "wb_list";
	public static final String PARAMETER_WEB_DAV_URL = "wdurl";
	public static final String PARAMETER_IS_FOLDER = "isf";
	private static final String ACTION_SORT = "wdlmb";
	private static final String SORT_BY_NAME = "name";
	private static final String SORT_BY_SIZE = "size";
	private static final String SORT_BY_MODIFICATION_DATE = "modDate";
	private static final String SORT_BY_NAME_DESC = "name_desc";
	private static final String SORT_BY_SIZE_DESC = "size_desc";
	private static final String SORT_BY_MODIFICATION_DATE_DESC = "modDate_desc";

	public static final String COLUMN_ICON = "icon";
	public static final String COLUMN_NAME = "name";
	public static final String COLUMN_SIZE = "size";
	public static final String COLUMN_VERSION = "version";
	public static final String COLUMN_LOCK = "lock";
	public static final String COLUMN_CHECKOUT = "checkout";
	public static final String COLUMN_LAST_MODIFIED = "last_modified";
	public static final String COLUMN_DELETE = "delete";
	
	private String clickedFilePath;
	private String clickedFileName;
	
	private String webDAVPath = "";
	private String rootPath = null;
	private String startPath = null;
	private String iconTheme = null;
	private boolean showFolders = true;
	private boolean showPublicFolder = true;
	private boolean showDropboxFolder = true;
	private Collection<String> columnsToHide = null;
	private boolean useVersionControl = true;
	private String onFileClickEventName = null;
	private int startPage = -1;
	private int rows = -1;
	private String sorter = SORT_BY_NAME;
	private boolean useStartPathIfAvailable = true;

	public WebDAVListManagedBean() {
		List<String> hideColumns = new ArrayList<String>();
		hideColumns.add(COLUMN_CHECKOUT);
		hideColumns.add(COLUMN_LOCK);
		setColumnsToHide(hideColumns);
	}
	
	public void resetSorter() {
		this.sorter = SORT_BY_NAME;
	}
	
	public void setWebDAVPath(String path) {
		this.webDAVPath = path;
	}
	
	public String getWebDAVPath() {
		return this.webDAVPath;
	}
	
	public boolean getIsClickedFile() {
		return (getClickedFilePath() != null && !("".equals(getClickedFilePath()))  );
	}
	
	public void setClickedFilePath(String path) {
		this.clickedFilePath = path;
	}
	
	public String getClickedFilePath() {
		return this.clickedFilePath;
	}
	
	public String getClickedFileName() {
		return this.clickedFileName;
	}

	public void setStartFolder(String start) {
		if (start != null && "".equals(start)) {
			start = null;
		}
		this.startPath = start;
	}
	
	public void setRootFolder(String root) {
		if (root != null && "".equals(root)) {
			root = null;
		}
		this.rootPath = root;
	}
	
	public void setShowFolders(Boolean show) {
		this.showFolders = show.booleanValue();
	}
	
	public void setShowPublicFolder(Boolean show){
		this.showPublicFolder = show.booleanValue();
	}
	
	public void setShowDropboxFolder(Boolean show){
		this.showDropboxFolder = show.booleanValue();
	}
	
	public void setIconTheme(String theme) {
		if (theme != null && "".equals(theme)) {
			theme = null;
		}
		this.iconTheme = theme;
	}
	
	public Collection<String> getColumnsToHide(){
		if(this.columnsToHide==null){
			this.columnsToHide=new ArrayList<String>();
		}
		return this.columnsToHide;
	}
	
	public void setColumnsToHide(Collection<String> columns) {
		this.columnsToHide = columns;
	}
	
	public void addColumnsToHide(Collection<String> coll){
		if(this.columnsToHide==null){
			setColumnsToHide(coll);
		}
		else{
			this.columnsToHide.addAll(coll);
		}
	}
	
	private boolean showColumn(String columnName) {
		return (this.columnsToHide == null || !this.columnsToHide.contains(columnName));
	}
	
	public void setUseVersionControl(Boolean useVersionControl) {
		this.useVersionControl = useVersionControl.booleanValue();
		if (!this.useVersionControl) {
			
			getColumnsToHide().add(COLUMN_VERSION);
			getColumnsToHide().add(COLUMN_LOCK);
			getColumnsToHide().add(COLUMN_CHECKOUT);
		}
	}
	
	public void refresh(UIComponent comp) {
		ContentBlock block = null;
		while (comp != null && block == null) {
			if (comp instanceof ContentBlock) {
				block = (ContentBlock) comp;
				ContentViewer cv = block.getContentViewer();
				if (cv != null) { 
					cv.maintainPath(true);
				}
			} else {
				comp = comp.getParent();
			}
		}

		updateDataModel(new Integer(this.startPage), new Integer(this.rows));
	}
	
	public void processAction(ActionEvent actionEvent) throws AbortProcessingException {
		UIComponent comp = actionEvent.getComponent();
		
		ContentViewer v = null;
		UIComponent tmp = comp.getParent();
		while ( tmp != null && v == null) {
			if (tmp instanceof ContentViewer) {
				v = (ContentViewer) tmp;
			}
			else {
				tmp = tmp.getParent();
			}
		}
		
		if (v != null) {
			v.maintainPath(true);
		
			if (comp.getAttributes().get(ContentViewer.PARAMETER_ACTION) != null) {
				v.setRenderFlags((String) comp.getAttributes().get(ContentViewer.PARAMETER_ACTION) ); 
			}
		}
		
		boolean isFolder = true;
		String sortBy = (String) comp.getAttributes().get(ACTION_SORT);
		if (sortBy != null) {
			this.sorter = sortBy;
		}
		else {
			if (comp instanceof UICommand) {
				List<UIComponent> children = comp.getChildren();
				UIParameter par;
				for (UIComponent child: children) {
					if (child instanceof UIParameter) {
						par = (UIParameter) child;
						if (PARAMETER_WEB_DAV_URL.equals(par.getName()) ) {
							useStartPathIfAvailable = false;
							this.webDAVPath = (String) par.getValue();
						} else if (PARAMETER_IS_FOLDER.equals(par.getName())) {
							isFolder = ((Boolean) par.getValue()).booleanValue();
						}
					}
				}
	
			}
			
			WFList parent = getWFListParent(comp);
			
			if (this.webDAVPath != null && parent != null) {
				if (isFolder) {
					this.setClickedFilePath(null);
					this.clickedFileName = null;
				} else {
					this.setClickedFilePath(this.webDAVPath);
					int index = this.webDAVPath.lastIndexOf("/");
					if (index > -1) {
						this.clickedFileName = this.webDAVPath.substring(index+1);
						this.webDAVPath = this.webDAVPath.substring(0, index);
					}
				}
			}
			
			if (v != null) {
				v.setCurrentFolderPath(this.webDAVPath);
				v.setCurrentFileName(getClickedFileName());
			}
		}
		
	}

	private WFList getWFListParent(UIComponent comp) {
		UIComponent parent = comp.getParent();
		while (parent != null && !(parent instanceof WFList)) {
			parent = parent.getParent();
		}
		if (parent instanceof WFList) {
			return (WFList) parent;
		} else {
			return null;
		}
	}

	@SuppressWarnings("unchecked")
	public UIColumn[] createColumns(String var) {
		Vector<UIColumn> columns = new Vector<UIColumn>();
		String imageSize = "16";
		if (showColumn(COLUMN_ICON)) {
			UIColumn columnIcon = new UIColumn();
			HtmlGraphicImage icon = new HtmlGraphicImage();
			icon.setValueBinding("url", WFUtil.createValueBinding("#{"+var+".iconURL}"));
			icon.setId(P_ID+"_I");
			icon.setHeight("16");// sizes that make sense 16/32/64/128
			HtmlCommandLink iconLink = new HtmlCommandLink();
			iconLink.setId(P_ID+"_L");
			
			WFUtil.addParameterVB(iconLink, PARAMETER_WEB_DAV_URL, var + ".webDavUrl");
			WFUtil.addParameterVB(iconLink, PARAMETER_IS_FOLDER, var + ".isCollection");
			iconLink.setActionListener(WFUtil.createMethodBinding("#{"+WebDAVList.WEB_DAV_LIST_BEAN_ID+".processAction}", new Class[]{ActionEvent.class}));
			iconLink.getChildren().add(icon);
			columnIcon.getChildren().add(iconLink);
			
			columns.add(columnIcon);
		}
		
		if (showColumn(COLUMN_NAME)) {
			UIColumn columnSort = new UIColumn();
			HtmlCommandLink nameSortLink = new HtmlCommandLink();
			HtmlOutputText nameSortText = ContentBlock.getBundle().getLocalizedText("name");
			if (SORT_BY_NAME.equals(this.sorter)) {
				nameSortText.setStyleClass("wf_listheaderlink_clicked");
				nameSortLink.getAttributes().put(ACTION_SORT, SORT_BY_NAME_DESC);
			} else if (SORT_BY_NAME_DESC.equals(this.sorter)) {
				nameSortText.setStyleClass("wf_listheaderlink_clicked_descending");
				nameSortLink.getAttributes().put(ACTION_SORT, SORT_BY_NAME);
			} else {
				nameSortText.setStyleClass("wf_listheaderlink");
				nameSortLink.getAttributes().put(ACTION_SORT, SORT_BY_NAME);
			}
			nameSortLink.getChildren().add(nameSortText);
			nameSortLink.setActionListener(WFUtil.createMethodBinding("#{"+WebDAVList.WEB_DAV_LIST_BEAN_ID+".processAction}", new Class[]{ActionEvent.class}));
			nameSortLink.setId(P_ID+"_sortName");
			columnSort.setHeader(nameSortLink);
			
			//NameLink for file
			HtmlOutputLink nameLink = new HtmlOutputLink();
			
			if (this.onFileClickEventName != null) {
				nameLink.setOnclick(onFileClickEventName);
			}

			nameLink.setValueBinding("value", WFUtil.createValueBinding("#{"+ var + ".encodedURL}"));
			nameLink.setId(P_ID+"_fi");
			nameLink.setStyleClass("wf_listlink");
			nameLink.getChildren().add(WFUtil.getTextVB(var + ".name"));
			nameLink.setValueBinding("rendered", WFUtil.createValueBinding("#{"+var+".isFile}"));
			
			//NameLink for folder
			HtmlCommandLink nameFolderLink = new HtmlCommandLink();
			nameFolderLink.setId(P_ID);
			nameFolderLink.setStyleClass("wf_listlink");
			nameFolderLink.setValueBinding("value", WFUtil.createValueBinding("#{"+var + ".name}"));
			nameFolderLink.setValueBinding("rendered", WFUtil.createValueBinding("#{"+var+".isCollection}"));
			WFUtil.addParameterVB(nameFolderLink, PARAMETER_WEB_DAV_URL, var + ".webDavUrl");
			WFUtil.addParameterVB(nameFolderLink, PARAMETER_IS_FOLDER, var + ".isCollection");
			nameFolderLink.setActionListener(WFUtil.createMethodBinding("#{"+WebDAVList.WEB_DAV_LIST_BEAN_ID+".processAction}", new Class[]{ActionEvent.class}));
			
			// DetailLink
			HtmlCommandLink nameDetailsLink = new HtmlCommandLink();
			nameDetailsLink.setId(P_ID+"_det");
			nameDetailsLink.setStyleClass("content_viewer_file_details");
			nameDetailsLink.getAttributes().put(ContentViewer.PARAMETER_ACTION, ContentViewer.ACTION_FILE_DETAILS);
			nameDetailsLink.setValueBinding("rendered", WFUtil.createValueBinding("#{"+var+".isFile}"));
			nameDetailsLink.setValueBinding("alt", ContentBlock.getBundle().getValueBinding("document_details"));
			nameDetailsLink.setValueBinding("title", ContentBlock.getBundle().getValueBinding("document_details"));
			
			WFUtil.addParameterVB(nameDetailsLink, PARAMETER_WEB_DAV_URL, var + ".webDavUrl");
			WFUtil.addParameterVB(nameDetailsLink, PARAMETER_IS_FOLDER, var + ".isCollection");
			nameDetailsLink.setActionListener(WFUtil.createMethodBinding("#{"+WebDAVList.WEB_DAV_LIST_BEAN_ID+".processAction}", new Class[]{ActionEvent.class}));

			// PreviewLink
			HtmlOutputLink namePreviewLink = new HtmlOutputLink();
			
			namePreviewLink.setId(P_ID+"_pre");
			namePreviewLink.setValueBinding("value", WFUtil.createValueBinding("#{"+ var + ".previewActionURI}"));
			//"document_details"
			namePreviewLink.setStyleClass("content_viewer_file_preview");
			namePreviewLink.setValueBinding("rendered", WFUtil.createValueBinding("#{"+var+".isFile}"));
			namePreviewLink.setValueBinding("alt", ContentBlock.getBundle().getValueBinding("preview"));
			namePreviewLink.setValueBinding("title", ContentBlock.getBundle().getValueBinding("preview"));

			HtmlOutputLink permissionLink = new HtmlOutputLink();
			permissionLink.setId(P_ID+"_per");
			permissionLink.setValueBinding("value", WFUtil.createValueBinding("#{"+ var + ".permissionActionURI}"));
			permissionLink.setStyleClass("content_viewer_file_permissions");
			permissionLink.setValueBinding("rendered", WFUtil.createValueBinding("#{"+var+".renderPermissionLink}"));
			permissionLink.setValueBinding("alt", ContentBlock.getBundle().getValueBinding("permissions"));
			permissionLink.setValueBinding("title", ContentBlock.getBundle().getValueBinding("permissions"));
			
			
			columnSort.getChildren().add(nameLink);
			columnSort.getChildren().add(nameFolderLink);
			
			UIColumn mainColumn = new UIColumn();
			HtmlOutputText emptyText = WFUtil.getText(" ", "wf_listtext");
			emptyText.setValueBinding("rendered", WFUtil.createValueBinding("#{"+var+".isCollection}"));
			mainColumn.getChildren().add(namePreviewLink);
			mainColumn.getChildren().add(nameDetailsLink);
			mainColumn.getChildren().add(permissionLink);
			mainColumn.getChildren().add(emptyText);

			columns.add(columnSort);
			columns.add(mainColumn);
			
		}
		
		if (showColumn(COLUMN_SIZE)) {
			UIColumn columnSize = new UIColumn();
			HtmlCommandLink sizeSortLink = new HtmlCommandLink();
			HtmlOutputText sizeSortText = ContentBlock.getBundle().getLocalizedText("size");
			if (SORT_BY_SIZE.equals(this.sorter)) {
				sizeSortText.setStyleClass("wf_listheaderlink_clicked");
				sizeSortLink.getAttributes().put(ACTION_SORT, SORT_BY_SIZE_DESC);
			} else if (SORT_BY_SIZE_DESC.equals(this.sorter)) {
				sizeSortText.setStyleClass("wf_listheaderlink_clicked_descending");
				sizeSortLink.getAttributes().put(ACTION_SORT, SORT_BY_SIZE);
			} else {
				sizeSortText.setStyleClass("wf_listheaderlink");
				sizeSortLink.getAttributes().put(ACTION_SORT, SORT_BY_SIZE_DESC);
			}
			sizeSortLink.getChildren().add(sizeSortText);
			sizeSortLink.setActionListener(WFUtil.createMethodBinding("#{"+WebDAVList.WEB_DAV_LIST_BEAN_ID+".processAction}", new Class[]{ActionEvent.class}));
			sizeSortLink.setId(P_ID+"_sortSize");
			columnSize.setHeader(sizeSortLink);
			HtmlOutputText size = WFUtil.getTextVB(var + ".length");
			size.setStyleClass("wf_listtext");
			columnSize.getChildren().add(size);
			
			columns.add(columnSize);
		}
		
		if (showColumn(COLUMN_VERSION)) {
			UIColumn columnVersion = new UIColumn();
			columnVersion.setHeader(ContentBlock.getBundle().getLocalizedText("version"));
			HtmlOutputText version = WFUtil.getTextVB(var + ".version");
			version.setStyleClass("wf_listtext");
			columnVersion.getChildren().add(version);
			
			columns.add(columnVersion);
		}
		
		if (showColumn(COLUMN_LOCK)) {
			HtmlGraphicImage lock = new HtmlGraphicImage();
			lock.setValueBinding("rendered", WFUtil.createValueBinding("#{"+var+".isLocked}"));
			lock.setUrl(IWMainApplication.getDefaultIWMainApplication().getURIFromURL(ContentUtil.getBundle().getResourcesVirtualPath())+"/images/locked.gif");
			lock.setId(P_ID+"_lock");
			lock.setHeight(imageSize);// sizes that make sense 16/32/64/128

			HtmlOutputText emptyText = WFUtil.getText(" ", "wf_listtext");
			emptyText.setValueBinding("rendered", WFUtil.createValueBinding("#{"+var+".isUnlocked}"));

			UIColumn columnLock = new UIColumn();
			columnLock.setHeader(ContentBlock.getBundle().getLocalizedText("lock"));
			columnLock.getChildren().add(lock);
			columnLock.getChildren().add(emptyText);
			
			columns.add(columnLock);
		}
		
		if (showColumn(COLUMN_CHECKOUT)) {
			UIColumn checkout = new UIColumn();
			checkout.setHeader(ContentBlock.getBundle().getLocalizedText("checked_out"));
			HtmlOutputText checkedOut = WFUtil.getTextVB(var + ".comment");
			checkedOut.setValueBinding("rendered", WFUtil.createValueBinding("#{"+var+".checkedOut}"));
			checkedOut.setStyleClass("wf_listtext");
			checkout.getChildren().add(checkedOut);
			
			HtmlOutputText emptyText = WFUtil.getText(" ", "wf_listtext");
			checkout.getChildren().add(emptyText);
			
			columns.add(checkout);
		}
		
		if (showColumn(COLUMN_LAST_MODIFIED)) {
			UIColumn lastModified = new UIColumn();
			HtmlCommandLink modSortLink = new HtmlCommandLink();
			HtmlOutputText modSortText = ContentBlock.getBundle().getLocalizedText("last_modified");
			if (SORT_BY_MODIFICATION_DATE.equals(this.sorter)) {
				modSortText.setStyleClass("wf_listheaderlink_clicked");
				modSortLink.getAttributes().put(ACTION_SORT, SORT_BY_MODIFICATION_DATE_DESC);
			} else if (SORT_BY_MODIFICATION_DATE_DESC.equals(this.sorter)) {
				modSortText.setStyleClass("wf_listheaderlink_clicked_descending");
				modSortLink.getAttributes().put(ACTION_SORT, SORT_BY_MODIFICATION_DATE);
			} else {
				modSortText.setStyleClass("wf_listheaderlink");
				modSortLink.getAttributes().put(ACTION_SORT, SORT_BY_MODIFICATION_DATE_DESC);
			}
			modSortLink.getChildren().add(modSortText);
			modSortLink.setActionListener(WFUtil.createMethodBinding("#{"+WebDAVList.WEB_DAV_LIST_BEAN_ID+".processAction}", new Class[]{ActionEvent.class}));
			modSortLink.setId(P_ID+"_sortMod");
			lastModified.setHeader(modSortLink);
			HtmlOutputText modifiedDate = WFUtil.getTextVB(var + ".modifiedDateLong");
			modifiedDate.setStyleClass("wf_listtext");
			modifiedDate.setConverter(new WFTimestampConverter());
			lastModified.getChildren().add(modifiedDate);
			
			columns.add(lastModified);
		}
		
		if (showColumn(COLUMN_DELETE)) {
			UIColumn del = new UIColumn();
			del.setHeader(ContentBlock.getBundle().getLocalizedText("delete"));
			HtmlCommandLink delLink = new HtmlCommandLink();
			delLink.setValueBinding("rendered", WFUtil.createValueBinding("#{"+var+".isReal}"));
			delLink.getAttributes().put(ContentViewer.PARAMETER_ACTION, ContentViewer.ACTION_DELETE);
			WFUtil.addParameterVB(delLink, ContentViewer.PATH_TO_DELETE, var+".webDavUrl");
			delLink.setActionListener(WFUtil.createMethodBinding("#{contentviewerbean.processAction}", new Class[]{ActionEvent.class}));
			delLink.setId(P_ID+"_delLink");
			HtmlGraphicImage delete = new HtmlGraphicImage();
			delete.setUrl(IWMainApplication.getDefaultIWMainApplication().getURIFromURL(ContentUtil.getBundle().getResourcesVirtualPath())+"/images/delete.gif");
			delete.setId(P_ID+"_delete");
			delete.setHeight(imageSize);// sizes that make sense 16/32/64/128
			delLink.getChildren().add(delete);
			
			del.getChildren().add(delLink);
			
			columns.add(del);
		}
		
		return columns.toArray(new UIColumn[]{});
	}
	
	/**
	 * Updates the datamodel, definded by WFList
	 * @param first Number of first element
	 * @param rows Total number of rows
	 */
	public void updateDataModel(Integer start, Integer rows) {
		if (this.dataModel == null) {
			this.dataModel = new WFDataModel();
		}
		
		WebDAVBean[] beans = getDavData();
		
		int availableRows = beans.length;
		
		int nrOfRows = rows.intValue();
		if (nrOfRows == 0) {
			nrOfRows = availableRows;
		}
		int maxRow = Math.min(start.intValue() + nrOfRows,availableRows);
		for (int i = start.intValue(); i < maxRow; i++) {
			this.dataModel.set(beans[i], i);
		}
		
		this.dataModel.setRowCount(availableRows);
	}
	
	
	private WebDAVBean[] getDavData() {
		WebDAVBean[] data;
		try {
			IWUserContext iwuc = IWContext.getInstance();			
			IWSlideSession ss = (IWSlideSession) IBOLookup.getSessionInstance(iwuc, IWSlideSession.class);
			if (this.startPath != null && this.startPath.equals("/")) {
				this.startPath = "";
			}
			if (startPath != null && startPath.endsWith("/")) {
				startPath = startPath.substring(0, startPath.length()-1);
			}
			if (startPath != null && startPath.startsWith(ss.getIWSlideService().getWebdavServerURI())) {
				startPath = startPath.replaceFirst(ss.getIWSlideService().getWebdavServerURI(), "");
			}
			if (this.rootPath != null && this.rootPath.equals("/")) {
				this.rootPath = "";
			}
			if (startPath != null && useStartPathIfAvailable) {
				webDAVPath = startPath;
			}
			if(this.webDAVPath == null){
				this.webDAVPath = "";
			}
			if (this.rootPath != null && this.webDAVPath.indexOf(this.rootPath) == -1) {
				this.webDAVPath = this.rootPath;
			}
			if (ss.getExistence(this.webDAVPath)) {
				data = getDirectoryListing(ss.getResource(this.webDAVPath, false), ss.getWebdavServerURI());
			} else {
				data = new WebDAVBean[] { new WebDAVBean("Resource does not exist") };
			}
		} catch (HttpException ex) {
			System.out.println("[HTTPException]:"+ex.getMessage());
			System.out.println("[HTTPException]:"+ex.getReason());
			System.out.println("[HTTPException]:"+ex.getReasonCode());
			ex.printStackTrace();
			data = new WebDAVBean[] { new WebDAVBean("Caught HttpException") };
		} catch (IOException ex) {
			ex.printStackTrace();
			data = new WebDAVBean[] { new WebDAVBean("Caught IOException") };
		} catch (NullPointerException ex) {
			StackTraceElement[] trace = ex.getStackTrace();
			String traceString = null;
			for (int i = 0; i < trace.length; i++) {
				traceString = traceString + trace[i].toString() + "    \n\r";
			}
			data = new WebDAVBean[] { new WebDAVBean("Nullpointer: " + traceString) };
		}
		return data;
	}
	
	private WebDAVBean[] getDirectoryListing(WebdavExtendedResource headResource, String webDAVServletURL)	throws IOException, HttpException {
		WebdavResources resources = headResource.listWithDeltaV();
		Enumeration enumer = resources.getResources();
		List<WebDAVBean> v = new Vector<WebDAVBean>();
		WebDAVBean bean;
		WebDAVBean upBean = null;
		WebdavExtendedResource resource;
		String url;
		if (this.webDAVPath != null && !"".equals(this.webDAVPath) && !this.webDAVPath.equals(this.rootPath)) {
			upBean = new WebDAVBean();
			upBean.setIconTheme(this.iconTheme);
			int lastIndex = this.webDAVPath.lastIndexOf("/");
			String upTo = ContentBlock.getBundle().getLocalizedString("up_to_parent_folder");
			if (lastIndex > 0) {
				String dotdot = this.webDAVPath.substring(0, lastIndex);
				int lastIndex2 = dotdot.lastIndexOf("/");
				if (lastIndex2 > -1) {
					
					upBean.setName(upTo+" ("+dotdot.substring(lastIndex2+1)+")");
				} else {
					upBean.setName(upTo+" ("+dotdot+")");
				}
				upBean.setWebDavHttpURL(dotdot);
			} else {
				upBean.setName(upTo);
				upBean.setWebDavHttpURL("");
			}
			upBean.setIsReal(false);
			upBean.setIsCollection(true);
		}
		
		while (enumer.hasMoreElements()) {
			resource = (WebdavExtendedResource) enumer.nextElement();
			try{
			if (!resource.getDisplayName().startsWith(".")) {
				if (this.showFolders || (!this.showFolders && !resource.isCollection())) {
					if (resource.getName().equalsIgnoreCase("public") && resource.isCollection() && !this.showPublicFolder) {
						continue;
					}
					if (resource.getName().equalsIgnoreCase("dropbox") && resource.isCollection() && !this.showDropboxFolder) {
						continue;
					}
					try {
						bean = new WebDAVBean(resource);
						url = resource.getPath();
						url = url.replaceFirst(webDAVServletURL, "");
						bean.setWebDavHttpURL(url);
						bean.setIconTheme(this.iconTheme);
						v.add(bean);
					}
					catch (ClassCastException e) {
						//cused by 403 Forbidden
						//Should not stop the list from being shown
						e.printStackTrace();
					}
				}
			}
			}
			catch(Exception e){
				e.printStackTrace();
			}
		}
		
		sortResources(v);
		if (upBean != null) {
			v.add(0,upBean);
		}

		return v.toArray(new WebDAVBean[]{});
	}
	
	private void sortResources(List<WebDAVBean> v) {
		int sortMethod = 1;
		boolean desc = false;
		
		if (SORT_BY_NAME.equals(this.sorter)) {
			sortMethod = WebDAVBeanComparator.SORT_BY_NAME;
			desc = false;
		} else if (SORT_BY_NAME_DESC.equals(this.sorter)) {
			sortMethod = WebDAVBeanComparator.SORT_BY_NAME;
			desc = true;
		} else if (SORT_BY_SIZE.equals(this.sorter)) {
			sortMethod = WebDAVBeanComparator.SORT_BY_SIZE;
			desc = false;
		} else if (SORT_BY_SIZE_DESC.equals(this.sorter)) {
			sortMethod = WebDAVBeanComparator.SORT_BY_SIZE;
			desc = true;
		} else if (SORT_BY_MODIFICATION_DATE.equals(this.sorter)) {
			sortMethod = WebDAVBeanComparator.SORT_BY_MODIFICATION_DATE;
			desc = false;
		} else if (SORT_BY_MODIFICATION_DATE_DESC.equals(this.sorter)) {
			sortMethod = WebDAVBeanComparator.SORT_BY_MODIFICATION_DATE;
			desc = true;
		}
		
		Collections.sort(v, new WebDAVBeanComparator(IWContext.getInstance().getCurrentLocale(), sortMethod, desc));
	}
	
	private WFDataModel dataModel = new WFDataModel();
	
	public DataModel getDataModel() {
		return this.dataModel;
	}
	
	public void setDataModel(DataModel model) {
		this.dataModel = (WFDataModel) model;
	}

	public void setOnFileClickEvent(String event) {
		onFileClickEventName = event;
	}

	public boolean isUseStartPathIfAvailable() {
		return useStartPathIfAvailable;
	}

	public void setUseStartPathIfAvailable(Boolean useStartPathIfAvailable) {
		this.useStartPathIfAvailable = useStartPathIfAvailable == null ? Boolean.TRUE : useStartPathIfAvailable;
	}

}
