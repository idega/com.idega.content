/*
 * $Id: MetadataListManagedBean.java,v 1.3 2005/01/17 17:03:42 gimmi Exp $
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
import javax.faces.component.UIColumn;
import javax.faces.component.UIComponent;
import javax.faces.component.html.HtmlCommandButton;
import javax.faces.component.html.HtmlInputText;
import javax.faces.component.html.HtmlOutputText;
import javax.faces.event.AbortProcessingException;
import javax.faces.event.ActionEvent;
import javax.faces.event.ActionListener;
import org.apache.commons.httpclient.HttpException;
import org.apache.webdav.lib.PropertyName;
import com.idega.business.IBOLookup;
import com.idega.content.data.MetadataValueBean;
import com.idega.presentation.IWContext;
import com.idega.slide.business.IWSlideSession;
import com.idega.slide.util.WebdavRootResource;
import com.idega.webface.WFResourceUtil;
import com.idega.webface.WFUtil;
import com.idega.webface.bean.AbstractWFEditableListManagedBean;
import com.idega.webface.bean.WFEditableListDataBean;
import com.idega.webface.bean.WFListBean;


/**
 * 
 * Last modified: $Date: 2005/01/17 17:03:42 $ by $Author: gimmi $
 *
 * @author Joakim Johnson
 * @version $Revision: 1.3 $
 */
public class MetadataListManagedBean extends AbstractWFEditableListManagedBean implements WFListBean, ActionListener {

	protected String[] componentIDToken = new String[] { "type", "Values", "Button" };
	protected String[] localizationKey = new String[] { "type", "values", "" };

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
	private static WFResourceUtil localizer = WFResourceUtil.getResourceUtilContent();

	public MetadataListManagedBean() {
	}
/*
	public UIColumn[] createColumns(String var) {
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
*/
	private MetadataValueBean[] getMetadata() {
		MetadataValueBean[] data;
		ArrayList arrayList = new ArrayList();
		
		try {

			IWContext iwc = IWContext.getInstance();
			IWSlideSession session = (IWSlideSession)IBOLookup.getSessionInstance(iwc,IWSlideSession.class);
//			IWSlideService service = (IWSlideService)IBOLookup.getServiceInstance(iwc,IWSlideService.class);
	
			WebdavRootResource rootResource = session.getWebdavRootResource();
			//TODO (JJ) have to fetch the filepath
			String filePath = "";
			Iterator iter = WebDAVMetadata.metadataType.iterator();
			
			while(iter.hasNext()) {
				String type = (String)iter.next();
				Enumeration enumer = rootResource.propfindMethod(filePath,new PropertyName("DAV:",type).toString());
	
				String value = "";
				while(enumer.hasMoreElements()) {
					value = value+enumer.nextElement();
				}
				MetadataValueBean mvb = new MetadataValueBean(type, value,"delete");
				arrayList.add(mvb);
			}
			data = (MetadataValueBean[])arrayList.toArray(new MetadataValueBean[0]);
			
			//Test
			data = new MetadataValueBean[] {new MetadataValueBean()};
			

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
			data = new MetadataValueBean[] { new MetadataValueBean("Caught HttpException","","") };
		} catch (IOException ex) {
			ex.printStackTrace();
			data = new MetadataValueBean[] { new MetadataValueBean("Caught IOException","","") };
		} catch (NullPointerException ex) {
			StackTraceElement[] trace = ex.getStackTrace();
			String traceString = null;
			for (int i = 0; i < trace.length; i++) {
				traceString = traceString + trace[i].toString() + "    \n\r";
			}
			data = new MetadataValueBean[] { new MetadataValueBean("Nullpointer: " + traceString,"","") };
		}
		return data;
	}
/*
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
*/
	public void setWebDAVPath(String path) {
		this.webDAVPath = path;
	}
	
	public String getWebDAVPath() {
		return webDAVPath;
	}
	
	public void processAction(ActionEvent actionEvent) throws AbortProcessingException {
	}

	/* (non-Javadoc)
	 * @see com.idega.webface.bean.AbstractWFEditableListManagedBean#getData()
	 */
	public WFEditableListDataBean[] getData() {
		return getMetadata();
	}

	/* (non-Javadoc)
	 * @see com.idega.webface.bean.AbstractWFEditableListManagedBean#getNumberOfColumns()
	 */
	public int getNumberOfColumns() {
		return 3;
	}

	/* (non-Javadoc)
	 * @see com.idega.webface.bean.AbstractWFEditableListManagedBean#getUIComponent(java.lang.String, int)
	 */
	public UIComponent getUIComponent(String var, int columnIndex) {
		int index = columnIndex;
		UIComponent component = null;
		switch (index) {
			case 0:
				component = getTypeUIComponent(var);
				break;
			case 1:
				component = getValuesUIComponent(var);
				break;
			default:
				component = getButtonUIComponent(var);
		}
		component.setId(getUIComponentID(var, componentIDToken[index]));
		return component;
	}

	protected String getUIComponentID(String var, String idToken) {
		return String.valueOf(var + "." + componentIDToken + ".id");
	}

	private UIComponent getTypeUIComponent(String var) {
		UIColumn typeCol = new UIColumn();
		typeCol.setHeader(ContentBlock.getBundle().getLocalizedText("type"));
		HtmlOutputText creation = WFUtil.getTextVB(var + ".type");
		creation.setStyleClass("wf_listtext");
		typeCol.getChildren().add(creation);

//		HtmlOutputText t = new HtmlOutputText();
//		t.setStyleClass("wf_listtext");
		return typeCol;
	}
	private UIComponent getValuesUIComponent(String var) {
		UIColumn valuesCol = new UIColumn();
		valuesCol.setHeader(ContentBlock.getBundle().getLocalizedText("values"));
		HtmlInputText t = WFUtil.getInputText("metadatavalues", var + ".metadatavalues");
		t.setStyleClass("wf_listtext");
		valuesCol.getChildren().add(t);
		return valuesCol;
	}
	private UIComponent getButtonUIComponent(String var) {
		//TODO(JJ) Have to change this to create a button instead.
		UIColumn buttonCol = new UIColumn();
		buttonCol.setHeader(ContentBlock.getBundle().getLocalizedText("type"));
//		HtmlOutputText creation = WFUtil.getTextVB(var + ".type");
//		creation.setStyleClass("wf_listtext");
//		typeCol.getChildren().add(creation);

		HtmlCommandButton saveButton = localizer.getButtonVB("delete", "delete", this);
		buttonCol.getChildren().add(saveButton);
		return buttonCol;
	}
	/* (non-Javadoc)
	 * @see com.idega.webface.bean.AbstractWFEditableListManagedBean#getHeader(int)
	 */
	public UIComponent getHeader(int columnIndex) {
		return ContentBlock.getBundle().getLocalizedText(localizationKey[columnIndex]);
	}
}
