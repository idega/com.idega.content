/*
 * $Id: WebDAVMetadata.java,v 1.9 2005/03/17 17:34:08 joakim Exp $
 *
 * Copyright (C) 2004 Idega. All Rights Reserved.
 *
 * This software is the proprietary information of Idega.
 * Use is subject to license terms.
 *
 */
package com.idega.content.presentation;

import java.io.File;
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
import javax.faces.component.html.HtmlSelectBooleanCheckbox;
import javax.faces.component.html.HtmlSelectOneMenu;
import javax.faces.context.FacesContext;
import javax.faces.event.AbortProcessingException;
import javax.faces.event.ActionEvent;
import javax.faces.event.ActionListener;
import javax.faces.model.SelectItem;
import net.sourceforge.myfaces.component.html.ext.HtmlOutputText;
import org.apache.commons.httpclient.HttpException;
import org.apache.webdav.lib.PropertyName;
import com.idega.business.IBOLookup;
import com.idega.business.IBOLookupException;
import com.idega.content.bean.ManagedContentBeans;
import com.idega.content.business.MetadataUtil;
import com.idega.content.business.WebDAVMetadataResource;
import com.idega.content.data.MetadataValueBean;
import com.idega.presentation.IWBaseComponent;
import com.idega.presentation.IWContext;
import com.idega.presentation.Table;
import com.idega.slide.business.IWSlideService;
import com.idega.slide.business.IWSlideSession;
import com.idega.slide.util.WebdavRootResource;
import com.idega.webface.WFContainer;
import com.idega.webface.WFList;
import com.idega.webface.WFResourceUtil;
import com.idega.webface.WFUtil;

/**
 * 
 * Last modified: $Date: 2005/03/17 17:34:08 $ by $Author: joakim $
 * 
 * Display the UI for adding metadata type - values to a file.
 *
 * @author Joakim Johnson
 * @version $Revision: 1.9 $
 */
public class WebDAVMetadata extends IWBaseComponent implements ManagedContentBeans, ActionListener{
	
	private static final String METADATA_BLOCK_ID = "metadataBlockID";
	private static final String NEW_VALUES_ID = "newValueID";
	private static final String DROPDOWN_ID = "dropdownID";
	private static final String ADD_ID = "addID";
	private static final String RESOURCE_PATH = "resourcePath";
	private String resourcePath = "";
	private static final String METADATA_LIST_BEAN = "MetadataList";
	
	public WebDAVMetadata() {
	}
	
	public WebDAVMetadata(String path){
		resourcePath = path;
	}
	
	public void setResourcePath(String path){
		resourcePath = path;
	}
	
	protected void initializeContent() {
		
		if(resourcePath!=null){
			System.out.println("Initialize. Setting resourcePath to "+resourcePath);
			WFUtil.invoke(METADATA_LIST_BEAN, "setResourcePath", resourcePath);
		} else {
			System.err.println("[WARNING]["+getClass().getName()+"]: resource path can not be restored for managed beans");
		}
		
		
		setId(METADATA_BLOCK_ID);
		add(ContentBlock.getBundle().getLocalizedText("metadata"));
		WFList list = new WFList(METADATA_LIST_BEAN);
		add(list);
		add(getEditContainer());
	}
	
	/**
	 * @return
	 */
	private UIComponent getEditContainer() {
		WFResourceUtil localizer = WFResourceUtil.getResourceUtilContent();
		WFContainer mainContainer = new WFContainer();
		mainContainer.add(getMetadataTable(resourcePath));
		mainContainer.add(localizer.getHeaderTextVB("categories"));
		mainContainer.add(getCategoriesTable(resourcePath));
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
		
//		Timer timer = new Timer();
//		timer.start();

		//Create the table
		Table metadataTable = new Table(3,2);
		metadataTable.setId(metadataTable.getId() + "_ver");
		metadataTable.setRowStyleClass(1,"wf_listheading");
		metadataTable.setStyleClass("wf_listtable");
		
		//Add the lines
		List l = new ArrayList();
		
		//Type dropdown selector
		UIInput dropdown = new HtmlSelectOneMenu();
		dropdown.setId(DROPDOWN_ID);

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
			newValueInput.setId(NEW_VALUES_ID);
			metadataTable.add(newValueInput, 2, row++);
		}
		
		HtmlCommandButton addButton = localizer.getButtonVB(ADD_ID, "save", this);
		addButton.getAttributes().put(RESOURCE_PATH,resourcePath);

		metadataTable.add(addButton, 2, row);
		
//		mainContainer.add(metadataTable);
		return metadataTable;
	}
	
	/**
	 * <p> Creates a table with checkboxes for all the available categories </p>
	 * @param resourcePath
	 * @return table
	 */
	private Table getCategoriesTable(String resourcePath) {
//		WFResourceUtil localizer = WFResourceUtil.getResourceUtilContent();
		Table categoriesTable = new Table();
		
		IWContext iwc = IWContext.getInstance();
		WebDAVMetadataResource resource;
		int count = 0;
		try {
			resource = (WebDAVMetadataResource) IBOLookup.getSessionInstance(
					iwc, WebDAVMetadataResource.class);
			Iterator iter = resource.getCategories(resourcePath).iterator();
			while(iter.hasNext()) {
//				UISelectMany uism = new UISelectMany();
				HtmlSelectBooleanCheckbox smc = new HtmlSelectBooleanCheckbox();
				smc.setValue(new Boolean(true));
				categoriesTable.add(smc,count%3 + 1,count/3 + 1);
				HtmlOutputText catText = new HtmlOutputText();
				catText.setValue(iter.next().toString());
				categoriesTable.add(catText,count%3 + 1,count/3 + 1);
				count++;
			}
			count--;
			categoriesTable.setColumns(Math.min(count,3));
			categoriesTable.setRows(count/3 + 1);
			categoriesTable.setId(categoriesTable.getId() + "_ver");
			categoriesTable.setRowStyleClass(1,"wf_listheading");
			categoriesTable.setStyleClass("wf_listtable");
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
		
		return categoriesTable;
	}

	/**
	 * Will add the specified type - value as a property to the selected resource.
	 */
	public void processAction(ActionEvent actionEvent) throws AbortProcessingException {
		UIComponent comp = actionEvent.getComponent();
		resourcePath = (String)comp.getAttributes().get(RESOURCE_PATH);
		HtmlInputText newValueInput = (HtmlInputText) actionEvent.getComponent().getParent().findComponent(NEW_VALUES_ID);
		UIInput dropdown = (UIInput) actionEvent.getComponent().getParent().findComponent(DROPDOWN_ID);
		String val = "";
		String type = "";
		if(null!=dropdown) {
			val = newValueInput.getValue().toString();
			type = dropdown.getValue().toString();
		}
		MetadataValueBean[] ret = new MetadataValueBean[0];

		try {
			IWContext iwc = IWContext.getInstance();
			IWSlideSession session = (IWSlideSession)IBOLookup.getSessionInstance(iwc,IWSlideSession.class);
			IWSlideService service = (IWSlideService)IBOLookup.getServiceInstance(iwc,IWSlideService.class);
	
			WebdavRootResource rootResource = session.getWebdavRootResource();
			String filePath = service.getURI(resourcePath);

			//Store new settings
			if(type.length()>0) {
				System.out.println("Proppatch: filepath="+filePath+" type="+type+" value="+val);
				rootResource.proppatchMethod(filePath,new PropertyName("DAV:",type),val,true);
				//Also set the metadata on the parent folder
				rootResource.proppatchMethod(new File(filePath).getParent(),new PropertyName("DAV:",type),val,true);
			}
			
			//Store changes to previously created metadata
			WebDAVMetadataResource resource = (WebDAVMetadataResource) IBOLookup.getSessionInstance(
					iwc, WebDAVMetadataResource.class);
			ret = resource.getMetadata(resourcePath);

			for(int i=0; i<ret.length;i++) {
				type=ret[i].getType();
				val=ret[i].getMetadatavalues();
				System.out.println("type="+type+"  val="+val);
				rootResource.proppatchMethod(filePath,new PropertyName("DAV:",type),val,true);
				//Also set the metadata on the parent folder
				rootResource.proppatchMethod(new File(filePath).getParent(),new PropertyName("DAV:",type),val,true);
			}
			resource.clear();
		} catch (HttpException ex) {
			ex.printStackTrace();
		} catch (IOException ex) {
			ex.printStackTrace();
		} catch (NullPointerException ex) {
			ex.printStackTrace();
		}
		UIComponent tmp = comp.getParent();
		while ( tmp != null) {
			tmp = tmp.getParent();
		}
	}
	/**
	 * @see javax.faces.component.StateHolder#saveState(javax.faces.context.FacesContext)
	 */
	public Object saveState(FacesContext ctx) {
		Object values[] = new Object[2];
		values[0] = super.saveState(ctx);
		values[1] = resourcePath;

		return values;
	}

	/**
	 * @see javax.faces.component.StateHolder#restoreState(javax.faces.context.FacesContext,
	 *      java.lang.Object)
	 */
	public void restoreState(FacesContext ctx, Object state) {
		Object values[] = (Object[]) state;
		super.restoreState(ctx, values[0]);
		resourcePath = ((String) values[1]);

		if(resourcePath!=null){
				WFUtil.invoke(METADATA_LIST_BEAN, "setResourcePath", resourcePath);
		} else {
			System.err.println("[WARNING]["+getClass().getName()+"]: resource path can not be restored for managed beans");
		}
		
	}
}
