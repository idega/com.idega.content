/*
 * $Id: MetadataListManagedBean.java,v 1.1 2005/01/10 10:26:01 joakim Exp $
 *
 * Copyright (C) 2004 Idega. All Rights Reserved.
 *
 * This software is the proprietary information of Idega.
 * Use is subject to license terms.
 *
 */
package com.idega.content.presentation;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.List;
import java.util.Vector;
import javax.faces.component.UIColumn;
import javax.faces.component.UICommand;
import javax.faces.component.UIComponent;
import javax.faces.component.UIParameter;
import javax.faces.component.html.HtmlCommandLink;
import javax.faces.component.html.HtmlGraphicImage;
import javax.faces.component.html.HtmlOutputText;
import javax.faces.event.AbortProcessingException;
import javax.faces.event.ActionEvent;
import javax.faces.event.ActionListener;
import javax.faces.model.DataModel;
import org.apache.commons.httpclient.HttpException;
import org.apache.webdav.lib.PropertyName;
import org.apache.webdav.lib.WebdavResources;
import com.idega.business.IBOLookup;
import com.idega.content.data.MetadataValueBean;
import com.idega.content.data.WebDAVBean;
import com.idega.idegaweb.IWMainApplication;
import com.idega.presentation.IWContext;
import com.idega.slide.business.IWSlideService;
import com.idega.slide.business.IWSlideSession;
import com.idega.slide.util.WebdavExtendedResource;
import com.idega.slide.util.WebdavRootResource;
import com.idega.webface.WFList;
import com.idega.webface.WFUtil;
import com.idega.webface.bean.WFListBean;
import com.idega.webface.model.WFDataModel;


/**
 * 
 * Last modified: $Date: 2005/01/10 10:26:01 $ by $Author: joakim $
 *
 * @author Joakim Johnson
 * @version $Revision: 1.1 $
 */
public class MetadataListManagedBean implements WFListBean, ActionListener {
	
	private static final String P_ID = "wb_list";
	private static final String PARAMETER_WEB_DAV_URL = "wdurl";
	private static final String PARAMETER_IS_FOLDER = "isf";
	
	private String clickedFilePath;
	private String clickedFileName;
	
	private String webDAVPath = "";
	private String rootPath = null;
	private String startPath = null;
	
	private int startPage = -1;
	private int rows = -1;

	public MetadataListManagedBean() {
	}
	
	private WFDataModel dataModel = new WFDataModel();

	public DataModel getDataModel() {
		return dataModel;
	}
	
	public void setDataModel(DataModel model) {
		this.dataModel = (WFDataModel) model;
	}

	public UIColumn[] createColumns(String var) {
		
//		String imageSize = "16";
//		
//		UIColumn col0 = new UIColumn();
//		
//		HtmlGraphicImage icon = new HtmlGraphicImage();
//		icon.setValueBinding("url", WFUtil.createValueBinding("#{"+var+".iconURL}"));
//		icon.setId(P_ID+"_I");
//		icon.setHeight("16");// sizes that make sense 16/32/64/128
//
//		HtmlCommandLink iconLink = new HtmlCommandLink();
//		iconLink.setId(P_ID+"_L");
//		
//		WFUtil.addParameterVB(iconLink, PARAMETER_WEB_DAV_URL, var + ".webDavUrl");
//		WFUtil.addParameterVB(iconLink, PARAMETER_IS_FOLDER, var + ".isCollection");
//		iconLink.setActionListener(WFUtil.createMethodBinding("#{"+WebDAVList.WEB_DAV_LIST_BEAN_ID+".processAction}", new Class[]{ActionEvent.class}));
//		iconLink.getChildren().add(icon);
//		col0.getChildren().add(iconLink);
		
		UIColumn typeCol = new UIColumn();
		typeCol.setHeader(ContentBlock.getBundle().getLocalizedText("type"));
		HtmlOutputText creation = WFUtil.getTextVB(var + ".type");
		creation.setStyleClass("wf_listtext");
		typeCol.getChildren().add(creation);
		
		UIColumn valuesCol = new UIColumn();
		valuesCol.setHeader(ContentBlock.getBundle().getLocalizedText("size"));
		HtmlOutputText size = WFUtil.getTextVB(var + ".length");
		size.setStyleClass("wf_listtext");
		valuesCol.getChildren().add(size);
		
		
		UIColumn buttonCol = new UIColumn();
//		buttonCol.setHeader(ContentBlock.getBundle().getLocalizedText("delete"));
		HtmlCommandLink delLink = new HtmlCommandLink();
		delLink.setValueBinding("rendered", WFUtil.createValueBinding("#{"+var+".isReal}"));
		delLink.getAttributes().put(ContentViewer.PARAMETER_ACTION, ContentViewer.DELETE);
		WFUtil.addParameterVB(delLink, ContentViewer.PATH_TO_DELETE, var+".webDavUrl");
//		delLink.getAttributes().put(ContentViewer.PATH_TO_DELETE, WFUtil.invoke(var, "getWebDavUrl"));
		delLink.setActionListener(WFUtil.createMethodBinding("#{contentviewerbean.processAction}", new Class[]{ActionEvent.class}));
		delLink.setId(P_ID+"_delLink");
		HtmlGraphicImage delete = new HtmlGraphicImage();
		delete.setUrl(IWMainApplication.getDefaultIWMainApplication().getURIFromURL(WFUtil.getContentBundle().getResourcesVirtualPath())+"/images/delete.gif");
		delete.setId(P_ID+"_delete");
//		delete.setHeight(imageSize);// sizes that make sense 16/32/64/128
		delLink.getChildren().add(delete);
		
		buttonCol.getChildren().add(delLink);


		//return new UIColumn[] { col0, col, col2, col3, col4, col5, col6 ,col7};
		return new UIColumn[] { typeCol, valuesCol, buttonCol};
	}

	/**
	 * Updates the datamodel, definded by WFList
	 * @param first Number of first element
	 * @param rows Total number of rows
	 */
	public void updateDataModel(Integer start, Integer rows) {
		if (dataModel == null) {
			dataModel = new WFDataModel();
		}
		
		this.startPage = start.intValue();
		this.rows = rows.intValue();
		
		MetadataValueBean[] beans = getMetadata();
		
		int availableRows = beans.length;
		 
		int nrOfRows = rows.intValue();
		if (nrOfRows == 0) {
			nrOfRows = availableRows;
		}
		int maxRow = Math.min(start.intValue() + nrOfRows,availableRows);
		for (int i = start.intValue(); i < maxRow; i++) {
			dataModel.set(beans[i], i);
		}

		dataModel.setRowCount(availableRows);
	}
	

	private MetadataValueBean[] getMetadata() {
		MetadataValueBean[] data;
		ArrayList arrayList = new ArrayList();
		
		try {

			IWContext iwc = IWContext.getInstance();
			IWSlideSession session = (IWSlideSession)IBOLookup.getSessionInstance(iwc,IWSlideSession.class);
			IWSlideService service = (IWSlideService)IBOLookup.getServiceInstance(iwc,IWSlideService.class);
	
			WebdavRootResource rootResource = session.getWebdavRootResource();
			//TODO (JJ) have to fetch the filepath
			String filePath = "";
			Iterator iter = WebDAVMetadata.metadataType.iterator();
			
			while(iter.hasNext()) {
				String type = (String)iter.next();
				Enumeration enum = rootResource.propfindMethod(filePath,new PropertyName("DAV:",type).toString());
	
				while(enum.hasMoreElements()) {
					MetadataValueBean mvb = new MetadataValueBean(type, enum.nextElement().toString());
					arrayList.add(mvb);
				}
			}
			data = (MetadataValueBean[])arrayList.toArray();
			
			
			
/*			
			IWUserContext iwuc = IWContext.getInstance();			
			IWSlideSession ss = (IWSlideSession) IBOLookup.getSessionInstance(iwuc, IWSlideSession.class);
			if (startPath != null && startPath.equals("/")) {
				startPath = "";
			}
			if (rootPath != null && rootPath.equals("/")) {
				rootPath = "";
			}
			
			
			if (startPath != null) {
				webDAVPath = startPath;
				startPath = null;
			} else if(webDAVPath == null){
				webDAVPath = "";
			}
			
			if (rootPath != null && webDAVPath.indexOf(rootPath) == -1) {
				webDAVPath = rootPath;
			}
			
			if (ss.getExistence(webDAVPath)) {
				data = getDirectoryListing(ss.getWebdavResource(webDAVPath), ss.getWebdavServerURI());
			} else {
				data = new WebDAVBean[] { new WebDAVBean("Resource does not exist") };
			}
*/		} catch (HttpException ex) {
			System.out.println("[HTTPException]:"+ex.getMessage());
			System.out.println("[HTTPException]:"+ex.getReason());
			System.out.println("[HTTPException]:"+ex.getReasonCode());
			ex.printStackTrace();
			data = new MetadataValueBean[] { new MetadataValueBean("Caught HttpException","") };
		} catch (IOException ex) {
			ex.printStackTrace();
			data = new MetadataValueBean[] { new MetadataValueBean("Caught IOException","") };
		} catch (NullPointerException ex) {
			StackTraceElement[] trace = ex.getStackTrace();
			String traceString = null;
			for (int i = 0; i < trace.length; i++) {
				traceString = traceString + trace[i].toString() + "    \n\r";
			}
			data = new MetadataValueBean[] { new MetadataValueBean("Nullpointer: " + traceString,"") };
		}
		return data;
	}

	private WebDAVBean[] getDirectoryListing(WebdavExtendedResource headResource, String webDAVServletURL)	throws IOException, HttpException {
		WebdavResources resources = headResource.listWithDeltaV();//headResource.getChildResources();
		Enumeration enumer = resources.getResources();
		Vector v = new Vector();
		WebDAVBean bean;
		WebdavExtendedResource resource;
		String url;
		if (webDAVPath != null && !"".equals(webDAVPath) && !webDAVPath.equals(rootPath)) {
			bean = new WebDAVBean();
			int lastIndex = webDAVPath.lastIndexOf("/");
			if (lastIndex > 0) {
				String dotdot = webDAVPath.substring(0, lastIndex);
				bean.setName("Up to "+dotdot);
				bean.setWebDavHttpURL(dotdot);
			} else {
				bean.setName("Up to /");
				bean.setWebDavHttpURL("");
			}
			bean.setIsReal(false);
			bean.setIsCollection(true);
			v.add(bean);
		}
		
		while (enumer.hasMoreElements()) {
			resource = (WebdavExtendedResource) enumer.nextElement();
			if (!resource.getDisplayName().startsWith(".")) {
				bean = new WebDAVBean(resource);
				url = resource.getPath();
				url = url.replaceFirst(webDAVServletURL, "");
				bean.setWebDavHttpURL(url);
				v.add(bean);
			}
		}
		return (WebDAVBean[]) v.toArray(new WebDAVBean[]{});
	}
	
	public void setWebDAVPath(String path) {
		this.webDAVPath = path;
	}
	
	public String getWebDAVPath() {
		return webDAVPath;
	}
	
	public boolean getIsClickedFile() {
		return (getClickedFilePath() != null && !("".equals(getClickedFilePath()))  );
	}
	
	public void setClickedFilePath(String path) {
		this.clickedFilePath = path;
	}
	
	public String getClickedFilePath() {
		return clickedFilePath;
	}
	
	public String getClickedFileName() {
		return clickedFileName;
	}

	public void setStartFolder(String start) {
		this.startPath = start;
	}
	
	public void setRootFolder(String root) {
		this.rootPath = root;
	}

	public void refresh() {
		updateDataModel(new Integer(startPage), new Integer(rows));
	}
	
	public void processAction(ActionEvent actionEvent) throws AbortProcessingException {
		UIComponent comp = actionEvent.getComponent();
		
		boolean isFolder = true;
		if (comp instanceof UICommand) {
			List children = comp.getChildren();
			Iterator iter = children.iterator();
			UIComponent child;
			UIParameter par;
			while (iter.hasNext()) {
				child = (UIComponent) iter.next();
				if (child instanceof UIParameter) {
					par = (UIParameter) child;
					if (PARAMETER_WEB_DAV_URL.equals(par.getName()) ) {
						webDAVPath = (String) par.getValue();
					} else if (PARAMETER_IS_FOLDER.equals(par.getName())) {
						isFolder = ((Boolean) par.getValue()).booleanValue();
					}
				}
					
			}

		}
		
		WFList parent = getWFListParent(comp);
		
		if (webDAVPath != null && parent != null) {
			WFList parentList = (WFList) parent;
			if (isFolder) {
				this.setClickedFilePath(null);
				this.updateDataModel(new Integer(parentList.getFirst()), new Integer(parentList.getRows()));
			} else {
				this.setClickedFilePath(webDAVPath);
				int index = webDAVPath.lastIndexOf("/");
				if (index > -1) {
					clickedFileName = webDAVPath.substring(index+1);
					webDAVPath = webDAVPath.substring(0, index);
				}
			}
		}
	}

	private WFList getWFListParent(UIComponent comp) {
		UIComponent parent = (UIComponent) comp.getParent();
		while (parent != null && !(parent instanceof WFList)) {
			parent = parent.getParent();
		}
		if (parent instanceof WFList) {
			return (WFList) parent;
		} else {
			return null;
		}
	}
}
