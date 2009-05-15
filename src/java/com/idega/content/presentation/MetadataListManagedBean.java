/*
 * $Id: MetadataListManagedBean.java,v 1.15 2009/05/15 07:23:54 valdas Exp $
 *
 * Copyright (C) 2004 Idega. All Rights Reserved.
 *
 * This software is the proprietary information of Idega.
 * Use is subject to license terms.
 *
 */
package com.idega.content.presentation;

import java.io.IOException;
import java.rmi.RemoteException;
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
import com.idega.business.IBOLookupException;
import com.idega.content.business.WebDAVMetadataResource;
import com.idega.content.data.MetadataValueBean;
import com.idega.presentation.IWContext;
import com.idega.slide.business.IWSlideService;
import com.idega.slide.business.IWSlideSession;
import com.idega.slide.util.WebdavRootResource;
import com.idega.webface.WFResourceUtil;
import com.idega.webface.WFUtil;
import com.idega.webface.bean.AbstractWFEditableListManagedBean;
import com.idega.webface.bean.WFEditableListDataBean;
import com.idega.webface.bean.WFListBean;


/**
 * 
 * Last modified: $Date: 2009/05/15 07:23:54 $ by $Author: valdas $
 * Displays all the metadata types and values for the specified resource
 * Typically followed by WebDavMetadata in presentation to enable addeing metadata
 *
 * @author Joakim Johnson
 * @version $Revision: 1.15 $
 */
public class MetadataListManagedBean extends AbstractWFEditableListManagedBean implements WFListBean, ActionListener {

	protected String[] componentIDToken = new String[] { "type", "Values", "Button" };
	protected String[] localizationKey = new String[] { "type", "values", "" };

	private String resourcePath = null;
	
	private static WFResourceUtil localizer = WFResourceUtil.getResourceUtilContent();

	public MetadataListManagedBean() {
	}

	public void setResourcePath(String path){
		this.resourcePath = path;
	}

	/**
	 * Deleting metadata settings
	 */
	public void processAction(ActionEvent actionEvent) throws AbortProcessingException {
//		System.out.println("MetadataList action");
		UIComponent comp = actionEvent.getComponent();
		String var = (String)comp.getAttributes().get("var");
		this.resourcePath = (String)comp.getAttributes().get("resourcePath");
		String type = WFUtil.getStringValue(var,"type");
		
		MetadataValueBean[] ret = new MetadataValueBean[0];

		try {
			IWContext iwc = IWContext.getInstance();
			IWSlideSession session = IBOLookup.getSessionInstance(iwc,IWSlideSession.class);
			IWSlideService service = IBOLookup.getServiceInstance(iwc,IWSlideService.class);
	
			WebdavRootResource rootResource = session.getWebdavRootResource();

			String filePath = this.resourcePath;
			String serverURI = service.getWebdavServerURI();
			if(!this.resourcePath.startsWith(serverURI)) {
				filePath = service.getURI(this.resourcePath);
			}
			rootResource.proppatchMethod(filePath,new PropertyName("DAV:",type),"",true);
			
			WebDAVMetadataResource resource;
			resource = (WebDAVMetadataResource) IBOLookup.getSessionInstance(
					iwc, WebDAVMetadataResource.class);
			ret = resource.getMetadata(this.resourcePath);

			for(int i=0; i<ret.length;i++) {
//				System.out.println("type="+ret[i].getType()+"  val="+ret[i].getValues());
			}
			resource.clear();
			
		} catch (HttpException ex) {
			ex.printStackTrace();
		} catch (IOException ex) {
			ex.printStackTrace();
		} catch (NullPointerException ex) {
			ex.printStackTrace();
		}
	}

	/* (non-Javadoc)
	 * @see com.idega.webface.bean.AbstractWFEditableListManagedBean#getData()
	 */
	@Override
	public WFEditableListDataBean[] getData() {
		MetadataValueBean[] ret = new MetadataValueBean[0];
		if(this.resourcePath!=null) {
			IWContext iwc = IWContext.getInstance();
			WebDAVMetadataResource resource;
			try {
				resource = (WebDAVMetadataResource) IBOLookup.getSessionInstance(
						iwc, WebDAVMetadataResource.class);
				ret = resource.getMetadata(this.resourcePath);
			}
			catch (IBOLookupException e) {
				e.printStackTrace();
			}
			catch (RemoteException e) {
				e.printStackTrace();
			}
			catch (IOException e) {
				e.printStackTrace();
			}
		}
		return ret;
	}

	/* (non-Javadoc)
	 * @see com.idega.webface.bean.AbstractWFEditableListManagedBean#getNumberOfColumns()
	 */
	@Override
	public int getNumberOfColumns() {
		return this.componentIDToken.length;
	}

	/* (non-Javadoc)
	 * @see com.idega.webface.bean.AbstractWFEditableListManagedBean#getUIComponent(java.lang.String, int)
	 * Returns the UIComponent for the specific object (one for each row) and row
	 */
	@Override
	public UIComponent getUIComponent(String var, int columnIndex) {
		int index = columnIndex;
		UIComponent component = null;
		//For this bean it will be and HtmlOutputText, HtmlInputText or HtmlCommandButton depending on collumn
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
		String componentId = getUIComponentID(var, this.componentIDToken[index]);
		component.setId(componentId);
		return component;
	}

	/**
	 * Creates the UIComponent ID based on the object and the idToken provided
	 */
	protected String getUIComponentID(String var, String idToken) {
		return String.valueOf(var + "_" + idToken + "_id");
	}

	/**
	 * Returns the Metadata type as an HtmlOutputText
	 * @param var String used to do the lookup for the data bean
	 * @return UIComponent
	 */
	private UIComponent getTypeUIComponent(String var) {
		UIColumn typeCol = new UIColumn();
		typeCol.setHeader(ContentBlock.getBundle().getLocalizedText("type"));

		HtmlOutputText creation = WFUtil.getTextVB(var + ".localizedType");
		creation.setStyleClass("wf_listtext");
		typeCol.getChildren().add(creation);

		return typeCol;
	}

	/**
	 * Returns the Metadata values as an HtmlInputText
	 * @param var String used to do the lookup for the data bean
	 * @return UIComponent
	 */
	private UIComponent getValuesUIComponent(String var) {
		UIColumn valuesCol = new UIColumn();
		valuesCol.setHeader(ContentBlock.getBundle().getLocalizedText("values"));
		HtmlInputText t = WFUtil.getInputText("metadatavalues", var + ".metadatavalues");
		valuesCol.getChildren().add(t);
		return valuesCol;
	}

	/**
	 * Returns Delete Button
	 * @param var String used to do the lookup for the data bean
	 * @return
	 */
	private UIComponent getButtonUIComponent(String var) {
		UIColumn buttonCol = new UIColumn();
		buttonCol.setHeader(ContentBlock.getBundle().getLocalizedText("type"));

		HtmlCommandButton deleteButton = localizer.getButtonVB("delete", "delete", this);
		deleteButton.getAttributes().put("var",var);
		deleteButton.getAttributes().put("resourcePath",this.resourcePath);
		
		buttonCol.getChildren().add(deleteButton);
		return buttonCol;
	}
	
	/* (non-Javadoc)
	 * @see com.idega.webface.bean.AbstractWFEditableListManagedBean#getHeader(int)
	 */
	@Override
	public UIComponent getHeader(int columnIndex) {
		if(columnIndex==2){
			return WFUtil.getText("");
		}
		else{
			return ContentBlock.getBundle().getLocalizedText(this.localizationKey[columnIndex]);
		}
	}
}
