/*
 * $Id: WebDAVMetadata.java,v 1.3 2005/01/24 17:23:17 joakim Exp $
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
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import javax.faces.component.UIComponent;
import javax.faces.component.UIInput;
import javax.faces.component.UISelectItems;
import javax.faces.component.html.HtmlCommandButton;
import javax.faces.component.html.HtmlInputText;
import javax.faces.component.html.HtmlSelectOneMenu;
import javax.faces.event.AbortProcessingException;
import javax.faces.event.ActionEvent;
import javax.faces.event.ActionListener;
import javax.faces.model.SelectItem;
import org.apache.commons.httpclient.HttpException;
import org.apache.webdav.lib.PropertyName;
import com.idega.business.IBOLookup;
import com.idega.content.business.MetadataUtil;
import com.idega.presentation.IWBaseComponent;
import com.idega.presentation.IWContext;
import com.idega.presentation.Table;
import com.idega.slide.business.IWSlideService;
import com.idega.slide.business.IWSlideSession;
import com.idega.slide.util.WebdavRootResource;
import com.idega.util.Timer;
import com.idega.webface.WFContainer;
import com.idega.webface.WFResourceUtil;
import com.idega.webface.test.bean.ManagedContentBeans;

/**
 * 
 * Last modified: $Date: 2005/01/24 17:23:17 $ by $Author: joakim $
 * 
 * Display the UI for adding metadata type - values to a file.
 *
 * @author Joakim Johnson
 * @version $Revision: 1.3 $
 */
public class WebDAVMetadata extends IWBaseComponent implements ManagedContentBeans, ActionListener{
	
	private static final String METADATA_BLOCK_ID = "metadataBlockID";
	private static final String NEW_VALUES_ID = "newValueID";
	private static final String DROPDOWN_ID = "dropdownID";
	private static final String ADD_ID = "addID";
	private static final String RESOURCE_PATH = "resourcePath";
	private String resourcePath = "";
	
	public WebDAVMetadata() {
	}
	
	public WebDAVMetadata(String path){
		resourcePath = path;
	}
	
	public void setResourcePath(String path){
		resourcePath = path;
	}
	
	protected void initializeContent() {
		setId(METADATA_BLOCK_ID);
		add(getEditContainer());
	}
	
	/**
	 * @return
	 */
	private UIComponent getEditContainer() {
		return getMetadataTable(resourcePath);
	}
	
	/**
	 * Creates the metadata UI for the specified resource
	 * 
	 * @param resourcePath
	 * @return
	 */
	public WFContainer getMetadataTable(String resourcePath) {
		WFResourceUtil localizer = WFResourceUtil.getResourceUtilContent();
		WFContainer mainContainer = new WFContainer();
		
		Timer timer = new Timer();
		timer.start();

		Table metadataTable = new Table(3,2);
		metadataTable.setId(metadataTable.getId() + "_ver");
		metadataTable.setRowStyleClass(1,"wf_listheading");
		metadataTable.setStyleClass("wf_listtable");
		
		//Add line
		List l = new ArrayList();
		
		UIInput dropdown = new HtmlSelectOneMenu();
		dropdown.setId(DROPDOWN_ID);

		Locale locale = IWContext.getInstance().getCurrentLocale();
		
		Iterator iter = MetadataUtil.getMetadataTypes().iterator();
		while(iter.hasNext()) {
			String type = (String)iter.next();
			String label = ContentBlock.getBundle().getLocalizedString(type,locale);
			
			SelectItem item = new SelectItem(type, label, type, false);
			l.add(item);
		}

		UISelectItems sItems = new UISelectItems();
		sItems.setValue(l) ;
		dropdown.getChildren().add(sItems);
		
		metadataTable.add(dropdown, 1, 1);
		
		HtmlInputText newValueInput = new HtmlInputText();
		newValueInput.setSize(40);
		newValueInput.setId(NEW_VALUES_ID);
		metadataTable.add(newValueInput, 2, 1);
		
		HtmlCommandButton addButton = localizer.getButtonVB(ADD_ID, "save", this);
		addButton.getAttributes().put(RESOURCE_PATH,resourcePath);

		metadataTable.add(addButton, 2, 2);
		
		mainContainer.add(metadataTable);
		return mainContainer;
	}

	/**
	 * Will add the specified type - value as a property to the selected resource.
	 */
	public void processAction(ActionEvent actionEvent) throws AbortProcessingException {
		UIComponent comp = actionEvent.getComponent();
		resourcePath = (String)comp.getAttributes().get(RESOURCE_PATH);
		HtmlInputText newValueInput = (HtmlInputText) actionEvent.getComponent().getParent().findComponent(NEW_VALUES_ID);
		UIInput dropdown = (UIInput) actionEvent.getComponent().getParent().findComponent(DROPDOWN_ID);
		String val = newValueInput.getValue().toString();
		String type = dropdown.getValue().toString();

		try {

			IWContext iwc = IWContext.getInstance();
			IWSlideSession session = (IWSlideSession)IBOLookup.getSessionInstance(iwc,IWSlideSession.class);
			IWSlideService service = (IWSlideService)IBOLookup.getServiceInstance(iwc,IWSlideService.class);
	
			WebdavRootResource rootResource = session.getWebdavRootResource();

			String filePath = service.getURI(resourcePath);
			rootResource.proppatchMethod(filePath,new PropertyName("DAV:",type),val,true);
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
}
