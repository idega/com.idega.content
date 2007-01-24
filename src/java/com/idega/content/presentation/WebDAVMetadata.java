/*
 * $Id: WebDAVMetadata.java,v 1.17.2.1 2007/01/24 13:35:58 gediminas Exp $
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
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import javax.faces.component.UIComponent;
import javax.faces.component.UIInput;
import javax.faces.component.UISelectItems;
import javax.faces.component.html.HtmlCommandButton;
import javax.faces.component.html.HtmlInputText;
import javax.faces.component.html.HtmlSelectOneMenu;
import javax.faces.context.FacesContext;
import javax.faces.event.AbortProcessingException;
import javax.faces.event.ActionEvent;
import javax.faces.event.ActionListener;
import javax.faces.model.SelectItem;
import com.idega.business.IBOLookup;
import com.idega.business.IBOLookupException;
import com.idega.content.bean.ManagedContentBeans;
import com.idega.content.business.MetadataUtil;
import com.idega.content.business.WebDAVMetadataResource;
import com.idega.content.data.MetadataValueBean;
import com.idega.presentation.IWBaseComponent;
import com.idega.presentation.IWContext;
import com.idega.presentation.Table;
import com.idega.webface.WFContainer;
import com.idega.webface.WFList;
import com.idega.webface.WFResourceUtil;
import com.idega.webface.WFUtil;

/**
 * 
 * Last modified: $Date: 2007/01/24 13:35:58 $ by $Author: gediminas $
 * 
 * Display the UI for adding metadata type - values to a file.
 *
 * @author Joakim Johnson
 * @version $Revision: 1.17.2.1 $
 */
public class WebDAVMetadata extends IWBaseComponent implements ManagedContentBeans, ActionListener{
	
	private static final String DEFAULT_METADATA_BLOCK_ID = "metadataBlock";
	/*
	private static final String NEW_VALUES_ID = "newValueID";
	private static final String DROPDOWN_ID = "dropdownID";
	private static final String ADD_ID = "addID";
	*/
	private static final String RESOURCE_PATH = "resourcePath";
	private static final String METADATA_LIST_BEAN = "MetadataList";
	private String resourcePath = "";

	public WebDAVMetadata() {
		setId(DEFAULT_METADATA_BLOCK_ID);
	}
	
	public WebDAVMetadata(String path){
		this();
		this.resourcePath = path;
	}
	
	public void setResourcePath(String path){
		this.resourcePath = path;
	}
	
	protected void initializeComponent(FacesContext context) {
		
		if(this.resourcePath!=null){
//			System.out.println("Initialize. Setting resourcePath to "+resourcePath);
			//WFUtil.invoke(METADATA_LIST_BEAN, "setResourcePath", resourcePath);
			getMetadataListBean().setResourcePath(this.resourcePath);
		} else {
			System.err.println("[WARNING]["+getClass().getName()+"]: resource path can not be restored for managed beans");
		}
		add(ContentBlock.getBundle().getLocalizedText("metadata"));
		WFList list = new WFList(METADATA_LIST_BEAN);
		add(list);
		//add(getMetadataTable(resourcePath));
		add(getEditContainer());
	}
	
	/**
	 * <p>
	 * TODO tryggvil describe method getMetadataListBean
	 * </p>
	 * @return
	 */
	private MetadataListManagedBean getMetadataListBean() {
		return (MetadataListManagedBean) WFUtil.getBeanInstance(METADATA_LIST_BEAN);
	}

	/**
	 * @return
	 */
	private UIComponent getEditContainer() {
		WFContainer mainContainer = new WFContainer();
		mainContainer.add(getMetadataTable(this.resourcePath));

		return mainContainer;
	}
	
	/**
	 * Creates the metadata UI for the specified resource
	 * 
	 * @param resourcePath
	 * @return
	 */
	public Table getMetadataTable(String resourcePath) {
		WFResourceUtil localizer = WFResourceUtil.getResourceUtilContent();
		
		//Create the table
		Table metadataTable = new Table(3,2);
		metadataTable.setId(metadataTable.getId() + "_ver");
		metadataTable.setRowStyleClass(1,"wf_listheading");
		metadataTable.setStyleClass("wf_listtable");
		
		//Add the lines
		List l = new ArrayList();
		
		//Type dropdown selector
		UIInput dropdown = new HtmlSelectOneMenu();
		dropdown.setId(getDropdownId());

		Locale locale = IWContext.getInstance().getCurrentLocale();
		
		//First get the list with all metadata types
		ArrayList tempTypes = new ArrayList(MetadataUtil.getMetadataTypes());
		IWContext iwc = IWContext.getInstance();
		WebDAVMetadataResource resource;
		try {
			resource = (WebDAVMetadataResource) IBOLookup.getSessionInstance(
					iwc, WebDAVMetadataResource.class);
			MetadataValueBean[] ret = resource.getMetadata(resourcePath);
			//Remove already used types from the dropdown list
			for(int i=0; i<ret.length;i++) {
				tempTypes.remove(ret[i].getType());
			}
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
		int row = 1;
		
		//Display dropdown if there are any more metadata types left to add
		if(tempTypes.size()>0) {
			Iterator iter = tempTypes.iterator();
			
	//		Iterator iter = MetadataUtil.getMetadataTypes().iterator();
			while(iter.hasNext()) {
				String type = (String)iter.next();
				String label = ContentBlock.getBundle().getLocalizedString(type,locale);
				
				SelectItem item = new SelectItem(type, label, type, false);
				l.add(item);
			}
	
			UISelectItems sItems = new UISelectItems();
			sItems.setValue(l) ;
			dropdown.getChildren().add(sItems);
			
			metadataTable.add(dropdown, 1, row);
			
			HtmlInputText newValueInput = new HtmlInputText();
			newValueInput.setSize(40);
			newValueInput.setId(getNewInputId());
			metadataTable.add(newValueInput, 2, row++);
		}
		
		HtmlCommandButton addButton = localizer.getButtonVB(getAddButtonId(), "save", this);
		addButton.getAttributes().put(RESOURCE_PATH,resourcePath);

		metadataTable.add(addButton, 2, row);
		
//		mainContainer.add(metadataTable);
		return metadataTable;
	}
	
	/**
	 * <p>
	 * TODO tryggvil describe method getAddButtonId
	 * </p>
	 * @return
	 */
	private String getAddButtonId() {
		return getId()+"_add";
	}

	/**
	 * <p>
	 * TODO tryggvil describe method getNewInputId
	 * </p>
	 * @return
	 */
	private String getNewInputId() {
		return getId()+"_newinput";
	}

	/**
	 * <p>
	 * TODO tryggvil describe method getDropdownId
	 * </p>
	 * @return
	 */
	private String getDropdownId() {
		return this.getId()+"_dropdown";
	}

	/**
	 * Will add the specified type - value metadata as a property to the selected resource.
	 */
	public void processAction(ActionEvent actionEvent) throws AbortProcessingException {
		UIComponent comp = actionEvent.getComponent();
		this.resourcePath = (String)comp.getAttributes().get(RESOURCE_PATH);

		HtmlInputText newValueInput = (HtmlInputText) actionEvent.getComponent().getParent().findComponent(getNewInputId());
		UIInput dropdown = (UIInput) comp.getParent().findComponent(getDropdownId());
		String val = "";
		String type = "";
		if(null!=dropdown) {
			val = newValueInput.getValue().toString();
			type = dropdown.getValue().toString();
		}
		
		IWContext iwuc = IWContext.getInstance();
		try {
			WebDAVMetadataResource resource = (WebDAVMetadataResource) IBOLookup.getSessionInstance(iwuc, WebDAVMetadataResource.class);
			resource.setMetadata(this.resourcePath, type, val);
		} catch (IBOLookupException e) {
			throw new RuntimeException(e);
		} catch (RemoteException e) {
			throw new RuntimeException(e);
		}
	}

	/**
	 * @see javax.faces.component.StateHolder#saveState(javax.faces.context.FacesContext)
	 */
	public Object saveState(FacesContext ctx) {
		Object values[] = new Object[2];
		values[0] = super.saveState(ctx);
		values[1] = this.resourcePath;

		return values;
	}

	/**
	 * @see javax.faces.component.StateHolder#restoreState(javax.faces.context.FacesContext,
	 *      java.lang.Object)
	 */
	public void restoreState(FacesContext ctx, Object state) {
		Object values[] = (Object[]) state;
		super.restoreState(ctx, values[0]);
		this.resourcePath = ((String) values[1]);

		if(this.resourcePath!=null){
				//WFUtil.invoke(METADATA_LIST_BEAN, "setResourcePath", resourcePath);
				getMetadataListBean().setResourcePath(this.resourcePath);
		} else {
			System.err.println("[WARNING]["+getClass().getName()+"]: resource path can not be restored for managed beans");
		}
		
	}

}
