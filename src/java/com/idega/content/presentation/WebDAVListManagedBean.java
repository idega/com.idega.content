package com.idega.content.presentation;

import java.io.IOException;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Enumeration;
import java.util.List;
import java.util.Vector;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.el.ELContext;
import javax.faces.component.UIColumn;
import javax.faces.component.UICommand;
import javax.faces.component.UIComponent;
import javax.faces.component.UIParameter;
import javax.faces.component.html.HtmlCommandLink;
import javax.faces.component.html.HtmlGraphicImage;
import javax.faces.component.html.HtmlOutputLink;
import javax.faces.component.html.HtmlOutputText;
import javax.faces.context.FacesContext;
import javax.faces.event.AbortProcessingException;
import javax.faces.event.ActionEvent;
import javax.faces.event.ActionListener;
import javax.faces.model.DataModel;

import org.apache.commons.httpclient.HttpException;
import org.apache.webdav.lib.WebdavResource;
import org.apache.webdav.lib.WebdavResources;
import org.springframework.beans.factory.annotation.Autowired;

import com.idega.block.web2.business.JQuery;
import com.idega.block.web2.business.Web2Business;
import com.idega.business.IBOLookup;
import com.idega.content.business.ContentUtil;
import com.idega.content.business.WebDAVBeanComparator;
import com.idega.content.data.WebDAVBean;
import com.idega.content.repository.download.RepositoryItemDownloader;
import com.idega.core.search.presentation.SearchResults;
import com.idega.idegaweb.IWBundle;
import com.idega.idegaweb.IWMainApplication;
import com.idega.idegaweb.IWResourceBundle;
import com.idega.idegaweb.IWUserContext;
import com.idega.presentation.IWContext;
import com.idega.presentation.text.DownloadLink;
import com.idega.slide.business.IWSlideSession;
import com.idega.slide.util.WebdavExtendedResource;
import com.idega.util.ArrayUtil;
import com.idega.util.CoreConstants;
import com.idega.util.ListUtil;
import com.idega.util.expression.ELUtil;
import com.idega.webface.WFList;
import com.idega.webface.WFUtil;
import com.idega.webface.bean.WFListBean;
import com.idega.webface.convert.WFTimestampConverter;
import com.idega.webface.model.WFDataModel;

/**
 * A managed bean for the WebDAVList component
 * @author gimmi
 */
public class WebDAVListManagedBean extends SearchResults implements ActionListener, WFListBean, Serializable {

	private static final long serialVersionUID = 5991054676990358537L;
	private static final Logger LOGGER = Logger.getLogger(WebDAVListManagedBean.class.getName());
	
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

	private static final String RENDERED = "rendered";
	private static final String VALUE = "value";
	private static final String ACTION_LISTENER_EXPRESSION = "#{".concat(WebDAVList.WEB_DAV_LIST_BEAN_ID).concat(".processAction}");
	
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
	
	private String webDAVPath = CoreConstants.EMPTY;
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

	@Autowired
	private Web2Business web2;
	@Autowired
	private JQuery jQuery;
	
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
		return (getClickedFilePath() != null && !(CoreConstants.EMPTY.equals(getClickedFilePath()))  );
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
		if (start != null && CoreConstants.EMPTY.equals(start)) {
			start = null;
		}
		this.startPath = start;
	}
	
	public void setRootFolder(String root) {
		if (root != null && CoreConstants.EMPTY.equals(root)) {
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
		if (theme != null && CoreConstants.EMPTY.equals(theme)) {
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
		while (tmp != null && v == null) {
			if (tmp instanceof ContentViewer) {
				v = (ContentViewer) tmp;
			} else {
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
		} else {
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
							Object paramValue = par.getValue();
							if (paramValue instanceof Boolean) {
								isFolder = (Boolean) paramValue;
							} else if (paramValue instanceof String) {
								isFolder = Boolean.valueOf((String) paramValue);
							}
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
					int index = this.webDAVPath.lastIndexOf(CoreConstants.SLASH);
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

	public UIColumn[] createColumns(String var) {
		FacesContext fc = FacesContext.getCurrentInstance();
		ELContext elContext = fc.getELContext();
		
		List<UIColumn> columns = new ArrayList<UIColumn>();
		String imageSize = "16";
		if (showColumn(COLUMN_ICON)) {
			UIColumn columnIcon = new UIColumn();
			HtmlGraphicImage icon = new HtmlGraphicImage();
			icon.setValueExpression("url", WFUtil.createValueExpression(elContext, "#{"+var+".iconURL}", String.class));
			icon.setValueExpression("alt", WFUtil.createValueExpression(elContext, "#{"+var + ".name}", String.class));
			icon.setId(P_ID+"_I");
			icon.setHeight("16");// sizes that make sense 16/32/64/128
			HtmlCommandLink iconLink = new HtmlCommandLink();
			iconLink.setId(P_ID+"_L");
			
			WFUtil.addParameterVB(iconLink, PARAMETER_WEB_DAV_URL, var + ".webDavUrl");
			WFUtil.addParameterVB(iconLink, PARAMETER_IS_FOLDER, var + ".isCollection");
			iconLink.addActionListener(WFUtil.getMethodExpressionForActionListener(elContext, ACTION_LISTENER_EXPRESSION));
			iconLink.getChildren().add(icon);
			columnIcon.getChildren().add(iconLink);
			
			columns.add(columnIcon);
		}
		
		IWBundle bundle = ContentBlock.getBundle();
		
		if (showColumn(COLUMN_NAME)) {
			UIColumn columnSort = new UIColumn();
			HtmlCommandLink nameSortLink = new HtmlCommandLink();
			HtmlOutputText nameSortText = bundle.getLocalizedText("name");
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
			nameSortLink.addActionListener(WFUtil.getMethodExpressionForActionListener(elContext, ACTION_LISTENER_EXPRESSION));
			nameSortLink.setId(P_ID+"_sortName");
			columnSort.setHeader(nameSortLink);
			
			//NameLink for file
			HtmlOutputLink nameLink = new HtmlOutputLink();
			
			if (this.onFileClickEventName != null) {
				nameLink.setOnclick(onFileClickEventName);
			}

			nameLink.setValueExpression(VALUE, WFUtil.createValueExpression(elContext, "#{"+ var + ".encodedURL}", String.class));
			nameLink.setId(P_ID+"_fi");
			nameLink.setStyleClass("wf_listlink");
			nameLink.getChildren().add(WFUtil.getTextVB(var + ".name"));
			nameLink.setValueExpression(RENDERED, WFUtil.createValueExpression(elContext, "#{"+var+".isFile}", Boolean.class));
			
			//NameLink for folder
			HtmlCommandLink nameFolderLink = new HtmlCommandLink();
			nameFolderLink.setId(P_ID);
			nameFolderLink.setStyleClass("wf_listlink");
			nameFolderLink.setValueExpression(VALUE, WFUtil.createValueExpression(elContext, "#{"+var + ".name}", String.class));
			nameFolderLink.setValueExpression(RENDERED, WFUtil.createValueExpression(elContext, "#{"+var+".isCollection}", Boolean.class));
			WFUtil.addParameterVB(nameFolderLink, PARAMETER_WEB_DAV_URL, var + ".webDavUrl");
			WFUtil.addParameterVB(nameFolderLink, PARAMETER_IS_FOLDER, var + ".isCollection");
			nameFolderLink.addActionListener(WFUtil.getMethodExpressionForActionListener(elContext, ACTION_LISTENER_EXPRESSION));
			
			// DetailLink
			HtmlCommandLink nameDetailsLink = new HtmlCommandLink();
			nameDetailsLink.setId(P_ID+"_det");
			nameDetailsLink.setStyleClass("content_viewer_file_details");
			nameDetailsLink.getAttributes().put(ContentViewer.PARAMETER_ACTION, ContentViewer.ACTION_FILE_DETAILS);
			nameDetailsLink.setValueExpression(RENDERED, WFUtil.createValueExpression(elContext, "#{"+var+".isFile}", Boolean.class));
			nameDetailsLink.setValueExpression("alt", bundle.getValueExpression("document_details"));
			nameDetailsLink.setValueExpression("title", bundle.getValueExpression("document_details"));
			
			WFUtil.addParameterVB(nameDetailsLink, PARAMETER_WEB_DAV_URL, var + ".webDavUrl");
			WFUtil.addParameterVB(nameDetailsLink, PARAMETER_IS_FOLDER, var + ".isCollection");
			nameDetailsLink.addActionListener(WFUtil.getMethodExpressionForActionListener(elContext, ACTION_LISTENER_EXPRESSION));

			// PreviewLink
			HtmlOutputLink namePreviewLink = new HtmlOutputLink();
			namePreviewLink.setId(P_ID+"_pre");
			namePreviewLink.setValueExpression(VALUE, WFUtil.createValueExpression(elContext, "#{"+ var + ".previewActionURI}", String.class));
			
			//"document_details"
			namePreviewLink.setStyleClass("content_viewer_file_preview");
			namePreviewLink.setValueExpression(RENDERED, WFUtil.createValueExpression(elContext, "#{"+var+".isFile}", Boolean.class));
			namePreviewLink.setValueExpression("alt", bundle.getValueExpression("preview"));
			namePreviewLink.setValueExpression("title", bundle.getValueExpression("preview"));

			HtmlOutputLink permissionLink = new HtmlOutputLink();
			permissionLink.setId(P_ID+"_per");
			permissionLink.setValueExpression(VALUE, WFUtil.createValueExpression(elContext, "#{"+ var + ".permissionActionURI}", String.class));
			permissionLink.setStyleClass("content_viewer_file_permissions");
			permissionLink.setValueExpression(RENDERED, WFUtil.createValueExpression(elContext, "#{"+var+".renderPermissionLink}", Boolean.class));
			permissionLink.setValueExpression("alt", bundle.getValueExpression("permissions"));
			permissionLink.setValueExpression("title", bundle.getValueExpression("permissions"));
			
			columnSort.getChildren().add(nameLink);
			columnSort.getChildren().add(nameFolderLink);
			
			UIColumn mainColumn = new UIColumn();
			HtmlOutputText emptyText = WFUtil.getText(CoreConstants.EMPTY, "wf_listtext");
			emptyText.setValueExpression(RENDERED, WFUtil.createValueExpression(elContext, "#{"+var+".isCollection}", Boolean.class));
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
			HtmlOutputText sizeSortText = bundle.getLocalizedText("size");
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
			sizeSortLink.addActionListener(WFUtil.getMethodExpressionForActionListener(elContext, ACTION_LISTENER_EXPRESSION));
			sizeSortLink.setId(P_ID+"_sortSize");
			columnSize.setHeader(sizeSortLink);
			HtmlOutputText size = WFUtil.getTextVB(var + ".length");
			size.setStyleClass("wf_listtext");
			columnSize.getChildren().add(size);
			
			columns.add(columnSize);
		}
		
		if (showColumn(COLUMN_VERSION)) {
			UIColumn columnVersion = new UIColumn();
			columnVersion.setHeader(bundle.getLocalizedText("version"));
			HtmlOutputText version = WFUtil.getTextVB(var + ".version");
			version.setStyleClass("wf_listtext");
			columnVersion.getChildren().add(version);
			
			columns.add(columnVersion);
		}
		
		if (showColumn(COLUMN_LOCK)) {
			HtmlGraphicImage lock = new HtmlGraphicImage();
			lock.setValueExpression(RENDERED, WFUtil.createValueExpression(elContext, "#{"+var+".isLocked}", Boolean.class));
			lock.setUrl(IWMainApplication.getDefaultIWMainApplication().getURIFromURL(ContentUtil.getBundle().getResourcesVirtualPath())+"/images/locked.gif");
			lock.setId(P_ID+"_lock");
			lock.setHeight(imageSize);// sizes that make sense 16/32/64/128

			HtmlOutputText emptyText = WFUtil.getText(CoreConstants.EMPTY, "wf_listtext");
			emptyText.setValueExpression(RENDERED, WFUtil.createValueExpression(elContext, "#{"+var+".isUnlocked}", Boolean.class));

			UIColumn columnLock = new UIColumn();
			columnLock.setHeader(bundle.getLocalizedText("lock"));
			columnLock.getChildren().add(lock);
			columnLock.getChildren().add(emptyText);
			
			columns.add(columnLock);
		}
		
		if (showColumn(COLUMN_CHECKOUT)) {
			UIColumn checkout = new UIColumn();
			checkout.setHeader(bundle.getLocalizedText("checked_out"));
			HtmlOutputText checkedOut = WFUtil.getTextVB(var + ".comment");
			checkedOut.setValueExpression(RENDERED, WFUtil.createValueExpression(elContext, "#{"+var+".checkedOut}", Boolean.class));
			checkedOut.setStyleClass("wf_listtext");
			checkout.getChildren().add(checkedOut);
			
			HtmlOutputText emptyText = WFUtil.getText(CoreConstants.EMPTY, "wf_listtext");
			checkout.getChildren().add(emptyText);
			
			columns.add(checkout);
		}
		
		if (showColumn(COLUMN_LAST_MODIFIED)) {
			UIColumn lastModified = new UIColumn();
			HtmlCommandLink modSortLink = new HtmlCommandLink();
			HtmlOutputText modSortText = bundle.getLocalizedText("last_modified");
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
			modSortLink.addActionListener(WFUtil.getMethodExpressionForActionListener(elContext, ACTION_LISTENER_EXPRESSION));
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
			del.setHeader(bundle.getLocalizedText("delete"));
			HtmlCommandLink delLink = new HtmlCommandLink();
			delLink.setValueExpression(RENDERED, WFUtil.createValueExpression(elContext, "#{"+var+".isReal}", Boolean.class));
			delLink.getAttributes().put(ContentViewer.PARAMETER_ACTION, ContentViewer.ACTION_DELETE);
			WFUtil.addParameterVB(delLink, ContentViewer.PATH_TO_DELETE, var+".webDavUrl");
			delLink.addActionListener(WFUtil.getMethodExpressionForActionListener(elContext, "#{contentviewerbean.processAction}"));
			delLink.setId(P_ID+"_delLink");
			HtmlGraphicImage delete = new HtmlGraphicImage();
			delete.setUrl(IWMainApplication.getDefaultIWMainApplication().getURIFromURL(ContentUtil.getBundle().getResourcesVirtualPath())+"/images/delete.gif");
			delete.setId(P_ID+"_delete");
			delete.setValueExpression("alt", ContentUtil.getBundle().getValueExpression("delete"));
			delete.setHeight(imageSize);// sizes that make sense 16/32/64/128
			delLink.getChildren().add(delete);
			
			del.getChildren().add(delLink);
			
			columns.add(del);
		}
		
		IWContext iwc = IWContext.getIWContext(fc);
		if (iwc != null && (iwc.isLoggedOn() && iwc.isSuperAdmin())) {
			//	Download
			UIColumn download = new UIColumn();
			download.setHeader(bundle.getLocalizedText("download"));
			
			IWResourceBundle iwrb = bundle.getResourceBundle(iwc);
			
			DownloadLink downloadLink = new DownloadLink(iwrb.getLocalizedString("download", "Download"));
			downloadLink.setValueExpression(RENDERED, WFUtil.createValueExpression(elContext, "#{"+var+".isReal}", Boolean.class));
			downloadLink.setMediaWriterClass(RepositoryItemDownloader.class);
			WFUtil.addParameterVB(downloadLink, PARAMETER_WEB_DAV_URL, var + ".webDavUrl");
			WFUtil.addParameterVB(downloadLink, PARAMETER_IS_FOLDER, var + ".isCollection");
			
			download.getChildren().add(downloadLink);
			columns.add(download);
			
			//	Stream
//			UIColumn stream = new UIColumn();
//			stream.setHeader(bundle.getLocalizedUIComponent("stream", iwc.getApplication().createComponent(HtmlOutputText.COMPONENT_TYPE), "Stream"));
//			
//			PresentationUtil.addStyleSheetToHeader(iwc, getWebBusiness().getBundleURIToFancyBoxStyleFile());
//			PresentationUtil.addJavaScriptSourcesLinesToHeader(iwc, Arrays.asList(
//					getJQuery().getBundleURIToJQueryLib(),
//					bundle.getVirtualPathWithFileNameString("javascript/ContentAdmin.js"),
//					CoreConstants.DWR_ENGINE_SCRIPT,
//					CoreConstants.DWR_UTIL_SCRIPT,
//					"/dwr/interface/" + RepositoryItemStreamer.DWR_OBJECT + ".js"
//			));
//			PresentationUtil.addJavaScriptSourcesLinesToHeader(iwc, getWebBusiness().getBundleURIsToFancyBoxScriptFiles());
//			PresentationUtil.addJavaScriptActionOnLoad(iwc, "jQuery(window).load(function() {\n" +
//																"jQuery.each(jQuery('a.streamerLink'), function() {\n" +
//																	"var link = jQuery(this);\n" +
//																	"link.fancybox({autoScale: false, autoDimensions: false, width: 450, height: 175});\n" +
//																"});\n" +
//															"});"
//			);
//			Link streamerLink = new Link(iwrb.getLocalizedString("stream", "Stream"), RepositoryItemStreamViewer.class);
//			streamerLink.setTitle(iwrb.getLocalizedString("stream_to_other_server", "Stream to other server"));
//			streamerLink.setValueExpression(RENDERED, WFUtil.createValueExpression(elContext, "#{"+var+".isReal}", Boolean.class));
//			streamerLink.setStyleClass("streamerLink");
//			WFUtil.addParameterVB(streamerLink, PARAMETER_WEB_DAV_URL, var + ".webDavUrl");
//			
//			stream.getChildren().add(streamerLink);
//			columns.add(stream);
		}
		
		return ArrayUtil.convertListToArray(columns);
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
	
	@SuppressWarnings("deprecation")
	private WebDAVBean[] getDavData() {
		WebDAVBean[] data;
		try {
			IWUserContext iwuc = IWContext.getInstance();			
			IWSlideSession ss = IBOLookup.getSessionInstance(iwuc, IWSlideSession.class);
			if (this.startPath != null && this.startPath.equals(CoreConstants.SLASH)) {
				this.startPath = CoreConstants.EMPTY;
			}
			if (startPath != null && startPath.endsWith(CoreConstants.SLASH)) {
				startPath = startPath.substring(0, startPath.length()-1);
			}
			if (startPath != null && startPath.startsWith(ss.getIWSlideService().getWebdavServerURI())) {
				startPath = startPath.replaceFirst(ss.getIWSlideService().getWebdavServerURI(), CoreConstants.EMPTY);
			}
			if (this.rootPath != null && this.rootPath.equals(CoreConstants.SLASH)) {
				this.rootPath = CoreConstants.EMPTY;
			}
			if (startPath != null && useStartPathIfAvailable) {
				webDAVPath = startPath;
			}
			if(this.webDAVPath == null){
				this.webDAVPath = CoreConstants.EMPTY;
			}
			if (this.rootPath != null && this.webDAVPath.indexOf(this.rootPath) == -1) {
				this.webDAVPath = this.rootPath;
			}
			if (ss.getExistence(this.webDAVPath)) {
				data = getDirectoryListing(ss.getResource(this.webDAVPath, false), ss.getWebdavServerURI(), ss);
			} else {
				data = new WebDAVBean[] { new WebDAVBean("Resource does not exist") };
			}
		} catch (HttpException ex) {
			LOGGER.warning("[HTTPException]:"+ex.getMessage());
			LOGGER.warning("[HTTPException]:"+ex.getReason());
			LOGGER.warning("[HTTPException]:"+ex.getReasonCode());
			data = new WebDAVBean[] { new WebDAVBean("Caught HttpException") };
			LOGGER.log(Level.WARNING, "Error while trying to list directories and files for: " + this.webDAVPath, ex);
		} catch (IOException ex) {
			data = new WebDAVBean[] { new WebDAVBean("Caught IOException") };
			LOGGER.log(Level.WARNING, "Error while trying to list directories and files for: " + this.webDAVPath, ex);
		} catch (NullPointerException ex) {
			data = new WebDAVBean[] { new WebDAVBean(getExceptionData(ex)) };
			LOGGER.log(Level.WARNING, "Error while trying to list directories and files for: " + this.webDAVPath, ex);
		} catch (Exception ex) {
			data = new WebDAVBean[] { new WebDAVBean(getExceptionData(ex)) };
			LOGGER.log(Level.WARNING, "Error while trying to list directories and files for: " + this.webDAVPath, ex);
		}
		return data;
	}
	
	private String getExceptionData(Throwable t) {
		StackTraceElement[] trace = t.getStackTrace();
		String traceString = t.getClass().getName().concat(":\n\r");
		for (int i = 0; i < trace.length; i++) {
			traceString = traceString.concat(trace[i].toString()).concat("    \n\r");
		}
		return traceString;
	}
	
	@SuppressWarnings("unchecked")
	private WebDAVBean[] getDirectoryListing(WebdavExtendedResource headResource, String webDAVServletURL, IWSlideSession slide)
		throws IOException, HttpException {
		
		WebdavResources resources = headResource.listWithDeltaV();
		Enumeration<WebdavResource> enumer = resources.getResources();
		List<WebdavResource> resourcesInList = Collections.list(enumer);
		if (ListUtil.isEmpty(resourcesInList)) {
			LOGGER.info("No objects found in: " + headResource + ". Will try to use Slide API to fetch objects");
			
			WebdavResource wdr = slide.getWebdavResource(headResource.getPath());
			WebdavResource[] children = wdr.listWebdavResources();
			if (ArrayUtil.isEmpty(children)) {
				LOGGER.info("No objects there found by Slide API too in: " + headResource);
			} else {
				resourcesInList = Arrays.asList(children);
			}
		}
		
		List<WebDAVBean> directories = new Vector<WebDAVBean>();
		WebDAVBean upBean = null;
		if (this.webDAVPath != null && !CoreConstants.EMPTY.equals(this.webDAVPath) && !this.webDAVPath.equals(this.rootPath)) {
			upBean = new WebDAVBean();
			upBean.setIconTheme(this.iconTheme);
			int lastIndex = this.webDAVPath.lastIndexOf(CoreConstants.SLASH);
			String upTo = ContentBlock.getBundle().getLocalizedString("up_to_parent_folder");
			if (lastIndex > 0) {
				String dotdot = this.webDAVPath.substring(0, lastIndex);
				int lastIndex2 = dotdot.lastIndexOf(CoreConstants.SLASH);
				if (lastIndex2 > -1) {
					upBean.setName(upTo+" ("+dotdot.substring(lastIndex2+1)+")");
				} else {
					upBean.setName(upTo+" ("+dotdot+")");
				}
				upBean.setWebDavHttpURL(dotdot);
			} else {
				upBean.setName(upTo);
				upBean.setWebDavHttpURL(CoreConstants.EMPTY);
			}
			upBean.setIsReal(false);
			upBean.setIsCollection(true);
		}
		
		if (resourcesInList != null) {
			String url;
			WebDAVBean bean;
			for (WebdavResource resource: resourcesInList) {
				try {
					if (resource.exists() && !(resource instanceof WebdavExtendedResource) || !resource.getDisplayName().startsWith(CoreConstants.DOT)) {
						if (this.showFolders || (!this.showFolders && !resource.isCollection())) {
							String name = resource.getName();
							
							if (name == null || (name.equalsIgnoreCase("public") && resource.isCollection() && !this.showPublicFolder)) {
								continue;
							}
							if (name.equalsIgnoreCase("dropbox") && resource.isCollection() && !this.showDropboxFolder) {
								continue;
							}
							
							try {
								bean = new WebDAVBean((WebdavExtendedResource) resource);
								url = resource.getPath();
								url = url.replaceFirst(webDAVServletURL, CoreConstants.EMPTY);
								bean.setWebDavHttpURL(url);
								bean.setIconTheme(this.iconTheme);
								directories.add(bean);
							} catch (ClassCastException e) {
								//cused by 403 Forbidden
								//Should not stop the list from being shown
								LOGGER.log(Level.WARNING, "Error while creating WebDAVBean from resource: " + resource, e);
							}
						}
					}
				} catch(Exception e){
					e.printStackTrace();
				}
			}
		}
		
		sortResources(directories);
		if (upBean != null) {
			directories.add(0, upBean);
		}

		return ArrayUtil.convertListToArray(directories);
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

	Web2Business getWebBusiness() {
		if (web2 == null) {
			ELUtil.getInstance().autowire(this);
		}
		return web2;
	}
	
	JQuery getJQuery() {
		if (jQuery == null) {
			ELUtil.getInstance().autowire(this);
		}
		return jQuery;
	}
	
}