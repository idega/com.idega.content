/*
 * $Id: CategoriesEditor.java,v 1.2 2007/09/24 15:04:05 valdas Exp $
 *
 * Copyright (C) 2004 Idega. All Rights Reserved.
 *
 * This software is the proprietary information of Idega.
 * Use is subject to license terms.
 *
 */
package com.idega.content.presentation;

import java.rmi.RemoteException;
import java.util.Collection;
import javax.faces.component.UIComponent;
import javax.faces.component.html.HtmlCommandButton;
import javax.faces.component.html.HtmlInputText;
import javax.faces.component.html.HtmlOutputText;
import javax.faces.component.html.HtmlSelectBooleanCheckbox;
import javax.faces.context.FacesContext;
import javax.faces.event.AbortProcessingException;
import javax.faces.event.ActionEvent;
import javax.faces.event.ActionListener;
import com.idega.business.IBOLookup;
import com.idega.business.IBOLookupException;
import com.idega.content.business.WebDAVMetadataResource;
import com.idega.content.business.categories.CategoryBean;
import com.idega.content.data.ContentCategory;
import com.idega.presentation.IWBaseComponent;
import com.idega.presentation.IWContext;
import com.idega.presentation.Table;
import com.idega.webface.WFContainer;
import com.idega.webface.WFResourceUtil;


/**
 * <p>
 * Presentation object for the categories.<br>
 * Displays checkboxes for the categoriues that an article can belong to, so that a user can
 * select them accordingly.<br>
 * Also allows for adding categories if needed
 * </p>
 *  Last modified: $Date: 2007/09/24 15:04:05 $ by $Author: valdas $
 * 
 * @author <a href="mailto:gediminas@idega.com">Gediminas Paulauskas</a>
 * @version $Revision: 1.2 $
 */
public class CategoriesEditor  extends IWBaseComponent implements ActionListener {
	//Constants
	private static final String CATEGORIES_EDITOR_ID = "categoriesEditor";

	public CategoriesEditor() {
		setId(CATEGORIES_EDITOR_ID);
	}
	
	protected void initializeComponent(FacesContext context) {
		add(getEditContainer());
	}

	public void reset(){
		getChildren().clear();
		//initializeContent();
		//add(new Text("Crap"));
		setInitialized(false);
		IWContext iwuc = IWContext.getInstance();
		try {
			WebDAVMetadataResource resource = (WebDAVMetadataResource) IBOLookup.getSessionInstance(iwuc, WebDAVMetadataResource.class);
			resource.clear();
		} catch (IBOLookupException e) {
			throw new RuntimeException(e);
		} catch (RemoteException e) {
			throw new RuntimeException(e);
		}
	}
	
	
	/**
	 * Creates the edit container
	 * @return editContainer
	 */
	private UIComponent getEditContainer() {
		WFContainer mainContainer = new WFContainer();

		mainContainer.add(getCategoriesTable());

		mainContainer.add(getAddCategoryContainer());

		return mainContainer;
	}
	
	/**
	 * <p> Creates a table with checkboxes for all the available categories </p>
	 * @param resourcePath
	 * @return table
	 */
	private Table getCategoriesTable() {
		Table categoriesTable = new Table();
		int count = 0;

		String lang = IWContext.getInstance().getCurrentLocale().toString();

		//Display all categories
		Collection<ContentCategory> categories = CategoryBean.getInstance().getCategories();
		for (ContentCategory category : categories) {
			String categoryKey = category.getId();
			//Checkbox
			HtmlSelectBooleanCheckbox smc = new HtmlSelectBooleanCheckbox();
			setCategory(smc,categoryKey);
			smc.setValue(Boolean.FALSE);
			String id = getCategoryId(count);
			smc.setId(id);
			categoriesTable.add(smc,2,count + 1);
			//Text
			HtmlOutputText catText = new HtmlOutputText();
			String catLabel = category.getName(lang);
			catText.setValue(catLabel);
			categoriesTable.add(catText,2,count + 1);
			count++;
		}

		WFResourceUtil localizer = WFResourceUtil.getResourceUtilContent();
		HtmlCommandButton saveCategoryButton = localizer.getButtonVB(getSaveButtonId(), "save", this);
		categoriesTable.add(saveCategoryButton,1,count + 1);
		categoriesTable.setColumns(3);
		categoriesTable.setRows(count + 1);
		
		categoriesTable.setId(categoriesTable.getId() + "_ver");
		categoriesTable.setStyleClass("wf_listtable");
		
		return categoriesTable;
	}
	
	/**
	 * <p>
	 * TODO tryggvil describe method getSaveButtonId
	 * </p>
	 * @return
	 */
	private String getSaveButtonId() {
		return this.getId()+"_save";
	}

	/**
	 * <p>
	 * TODO tryggvil describe method getCategoryId
	 * </p>
	 * @param count
	 * @return
	 */
	private String getCategoryId(int count) {
		return getCategoryId()+count;
	}

	/**
	 * <p>
	 * TODO tryggvil describe method getCategoryId
	 * </p>
	 * @return
	 */
	private String getCategoryId() {
		return this.getId()+"_category_";
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
		newCategoryInput.setId(getAddCategoryInputId());
		container.add(newCategoryInput);
		//Button
		HtmlCommandButton addCategoryButton = localizer.getButtonVB(getAddButtonId(), "add", this);
		container.add(addCategoryButton);

		return container;
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
	 * TODO tryggvil describe method getAddCategoryTextId
	 * </p>
	 * @return
	 */
	private String getAddCategoryInputId() {
		return this.getId()+"_add_cat_input";
	}

	/**
	 * Will add the specified type - value metadata as a property to the selected resource.
	 */
	public void processAction(ActionEvent actionEvent) throws AbortProcessingException {
		UIComponent comp = actionEvent.getComponent();
		String id = comp.getId();
		UIComponent superParent=comp;
		CategoriesEditor realCategories = null;
		while(superParent.getParent()!=null){
			superParent=superParent.getParent();
			if(superParent instanceof CategoriesEditor){
				realCategories = (CategoriesEditor)superParent;
			}
		}
		
		if(realCategories==null){
			realCategories = (CategoriesEditor) superParent.findComponent(getId());
		}
		
		if(id.equalsIgnoreCase(getAddButtonId())) {
			//Add a category to the list of selectable categories
			//This is input for adding a category
			HtmlInputText newCategoryInput = (HtmlInputText) comp.getParent().findComponent(getAddCategoryInputId());

			String newCategoryName=newCategoryInput.getValue().toString();
			CategoryBean.getInstance().addCategory(newCategoryName);
			if(realCategories!=null){
				realCategories.reset();
			}
			return;
		}
		else if (id.equalsIgnoreCase(getSaveButtonId())) {
			realCategories.saveCategoriesSettings();
		}
	}
	
	/**
	 * <p>
	 * TODO menesis describe method saveCategoriesSettings
	 * </p>
	 */
	private void saveCategoriesSettings() {
		CategoryBean categoryBean = CategoryBean.getInstance();
		
		categoryBean.storeCategories();
	}

	public String getEnabledCategories() {
		StringBuffer categories = new StringBuffer(CategoryBean.CATEGORY_DELIMETER);
		CategoryBean categoryBean = CategoryBean.getInstance();
		int categoriesCount = categoryBean.getCategories().size();
		HtmlSelectBooleanCheckbox smc = null;
		//int count = 0;
		String categoryKey;
		String checkId;
		for (int i = 0; i < categoriesCount; i++) {
			checkId = getCategoryId(i);
			smc = (HtmlSelectBooleanCheckbox) getParent().findComponent(checkId);
			categoryKey = getCategory(smc);
			if (smc.isSelected()) {
				categories.append(categoryKey).append(CategoryBean.CATEGORY_DELIMETER);
			}
		}
		return categories.toString();
	}
	
	public String getCategory(HtmlSelectBooleanCheckbox checkbox){
		String categoryKey =  (String) checkbox.getAttributes().get("categoryKey");
		return categoryKey;
	}
	
	public void setCategory(HtmlSelectBooleanCheckbox checkbox,String categoryKey){
		checkbox.getAttributes().put("categoryKey",categoryKey);
	}

}
