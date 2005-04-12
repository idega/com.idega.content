/*
 * $Id: WebDAVCategories.java,v 1.2 2005/04/12 14:03:29 joakim Exp $
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
import java.util.Collection;
import java.util.Iterator;
import javax.faces.component.UIComponent;
import javax.faces.component.html.HtmlCommandButton;
import javax.faces.component.html.HtmlInputText;
import javax.faces.component.html.HtmlOutputText;
import javax.faces.component.html.HtmlSelectBooleanCheckbox;
import javax.faces.context.FacesContext;
import javax.faces.event.AbortProcessingException;
import javax.faces.event.ActionEvent;
import javax.faces.event.ActionListener;
import org.apache.commons.httpclient.HttpException;
import org.apache.webdav.lib.PropertyName;
import com.idega.business.IBOLookup;
import com.idega.business.IBOLookupException;
import com.idega.content.bean.ManagedContentBeans;
import com.idega.content.business.CategoryUtil;
import com.idega.content.business.WebDAVMetadataResource;
import com.idega.presentation.IWBaseComponent;
import com.idega.presentation.IWContext;
import com.idega.presentation.Table;
import com.idega.slide.business.IWSlideService;
import com.idega.slide.business.IWSlideSession;
import com.idega.slide.util.WebdavRootResource;
import com.idega.webface.WFContainer;
import com.idega.webface.WFResourceUtil;


/**
 * <p>
 * Presentation object for the categories.<br>
 * Displays checkboxes for the categoriues that an article can belong to, so that a user can
 * select them accordingly.<br>
 * Also allows for adding categories if needed
 * </p>
 *  Last modified: $Date: 2005/04/12 14:03:29 $ by $Author: joakim $
 * 
 * @author <a href="mailto:Joakim@idega.com">Joakim</a>
 * @version $Revision: 1.2 $
 */
public class WebDAVCategories  extends IWBaseComponent implements ManagedContentBeans, ActionListener{
	//Constants
	private static final String ADD_ID = "addID";
	private static final String SAVE_ID = "saveID";
	private static final String ADD_CATEGORY_ID = "addCategoryID";
	private static final String CATEGORIES_BLOCK_ID = "categoriesBlockID";
	private static final String CATEGORY = "category_";		//Prefix for the id in the userinterface
	private static final String RESOURCE_PATH = "resourcePath";	//key

	private String resourcePath = "";
	
//	List categoryStatus = new ArrayList();

	public WebDAVCategories() {
	}
	
	public WebDAVCategories(String path){
		resourcePath = path;
	}
	
	public void setResourcePath(String path){
		resourcePath = path;
	}
	
	protected void initializeContent() {
		setId(CATEGORIES_BLOCK_ID);
		add(getEditContainer());
	}

	/**
	 * Creates the edit container
	 * @return editContainer
	 */
	private UIComponent getEditContainer() {
		WFContainer mainContainer = new WFContainer();
		WFResourceUtil localizer = WFResourceUtil.getResourceUtilContent();
		mainContainer.add(localizer.getHeaderTextVB("categories"));
		mainContainer.add(getCategoriesTable(resourcePath));

		mainContainer.add(getAddCategoryContainer());

		return mainContainer;
	}
	
	/**
	 * <p> Creates a table with checkboxes for all the available categories </p>
	 * @param resourcePath
	 * @return table
	 */
	private Table getCategoriesTable(String resourcePath) {
		Table categoriesTable = new Table();
		
		IWContext iwc = IWContext.getInstance();
		WebDAVMetadataResource resource;
		int count = 0;
		try {
			//Get all the selected categories for this article and display them as selected
			resource = (WebDAVMetadataResource) IBOLookup.getSessionInstance(
					iwc, WebDAVMetadataResource.class);
			Collection selectedCategories = resource.getCategories(resourcePath);
			Iterator selectedIter = selectedCategories.iterator();
			while(selectedIter.hasNext()) {
				String text = selectedIter.next().toString();
				//Checkbox
				HtmlSelectBooleanCheckbox smc = new HtmlSelectBooleanCheckbox();
				smc.setValue(new Boolean(true));
				smc.setId(CATEGORY+text);
				smc.getAttributes().put(RESOURCE_PATH,resourcePath);
				categoriesTable.add(smc,count%3 + 1,count/3 + 1);
				//Text
				HtmlOutputText catText = new HtmlOutputText();
				catText.setValue(text);
				categoriesTable.add(catText,count%3 + 1,count/3 + 1);
				count++;
			}

			//Display all the non-selected categories
			Iterator nonSelectedIter = CategoryUtil.getCategories().iterator();
			while(nonSelectedIter.hasNext()) {
				Object nonSel = nonSelectedIter.next();
				if(!selectedCategories.contains(nonSel)) {
					String text = nonSel.toString();
					//Checkbox
					HtmlSelectBooleanCheckbox smc = new HtmlSelectBooleanCheckbox();
					smc.setValue(new Boolean(false));
					smc.setId(CATEGORY+text);
					smc.getAttributes().put(RESOURCE_PATH,resourcePath);
					categoriesTable.add(smc,count%3 + 1,count/3 + 1);
					//Text
					HtmlOutputText catText = new HtmlOutputText();
					catText.setValue(text);
					categoriesTable.add(catText,count%3 + 1,count/3 + 1);
					count++;
				}
			}

			count--;
			//Add the save button
			WFResourceUtil localizer = WFResourceUtil.getResourceUtilContent();
			HtmlCommandButton addCategoryButton = localizer.getButtonVB(SAVE_ID, "save", this);
			categoriesTable.add(addCategoryButton,1,count/3 + 2);
			
			categoriesTable.setColumns(Math.min(count+1,3));
			categoriesTable.setRows(count/3 + 2);
			categoriesTable.setId(categoriesTable.getId() + "_ver");
//			categoriesTable.setRowStyleClass(1,"wf_listheading");
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
	 * <p> Returns a container with "add category" UI</p>
	 * @return WFContainer
	 */
	private WFContainer getAddCategoryContainer() {
		WFContainer container = new WFContainer();
		WFResourceUtil localizer = WFResourceUtil.getResourceUtilContent();
		//Text
		HtmlOutputText addText = localizer.getTextVB("new_category");
		container.add(addText);
		//Input
		HtmlInputText newCategoryInput = new HtmlInputText();
		newCategoryInput.setSize(40);
		newCategoryInput.setId(ADD_CATEGORY_ID);
		container.add(newCategoryInput);
		//Button
		HtmlCommandButton addCategoryButton = localizer.getButtonVB(ADD_ID, "add", this);
		container.add(addCategoryButton);

		return container;
	}

	/**
	 * Will add the specified type - value metadata as a property to the selected resource.
	 */
	public void processAction(ActionEvent actionEvent) throws AbortProcessingException {
		UIComponent comp = actionEvent.getComponent();
		String id = comp.getId();
		System.out.println("action event id="+ id);
//		resourcePath = (String)comp.getAttributes().get(RESOURCE_PATH);

		if(id.equalsIgnoreCase(ADD_ID)) {
			//Add a category to the list of selectable categories
			//This is input for adding a category
			HtmlInputText newCategoryInput = (HtmlInputText) comp.getParent().findComponent(ADD_CATEGORY_ID);

			CategoryUtil.addCategory(newCategoryInput.getValue().toString());
			return;
		}
		if(id.equalsIgnoreCase(SAVE_ID)) {
			//save the selection of categories to the article
			//Build together the categories string
			StringBuffer categories = new StringBuffer();
			Iterator iter = CategoryUtil.getCategories().iterator();
			HtmlSelectBooleanCheckbox smc = null;
			while(iter.hasNext()) {
				String text = iter.next().toString();
				smc = (HtmlSelectBooleanCheckbox)comp.getParent().findComponent(CATEGORY+text);

				boolean bool = ((Boolean)smc.getValue()).booleanValue();
				System.out.println("Category "+text+" was set to "+bool);
				if(bool) {
					categories.append(text).append(",");
				}
			}
			if(smc!=null) {
				//Store categories to file and folder
				resourcePath = (String)smc.getAttributes().get(RESOURCE_PATH);
				IWContext iwc = IWContext.getInstance();
				try {
					IWSlideSession session = (IWSlideSession)IBOLookup.getSessionInstance(iwc,IWSlideSession.class);
					IWSlideService service = (IWSlideService)IBOLookup.getServiceInstance(iwc,IWSlideService.class);
					String filePath = service.getURI(resourcePath);
					WebdavRootResource rootResource = session.getWebdavRootResource();
	
					//Store new settings
					if(categories.length()>0) {
						System.out.println("Proppatch: filepath="+filePath+" categories value="+categories);
						rootResource.proppatchMethod(filePath,new PropertyName("DAV:","categories"),categories.toString(),true);
						//Also set the metadata on the parent folder
						String parent = new File(filePath).getParent().substring(4);
						rootResource.proppatchMethod(parent,new PropertyName("DAV:","categories"),categories.toString(),true);
					}
					//Clear the cashed data so that it will be reloaded.
					WebDAVMetadataResource resource = (WebDAVMetadataResource) IBOLookup.getSessionInstance(
							iwc, WebDAVMetadataResource.class);
					resource.clear();
				}
				catch (RemoteException e) {
					e.printStackTrace();
				}
				catch (HttpException e) {
					e.printStackTrace();
				}
				catch (IOException e) {
					e.printStackTrace();
				}
			}

			return;
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
	}
}
