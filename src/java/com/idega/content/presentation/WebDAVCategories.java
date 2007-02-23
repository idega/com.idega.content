/*
 * $Id: WebDAVCategories.java,v 1.20 2007/02/23 14:35:48 gediminas Exp $
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
import com.idega.business.IBOLookup;
import com.idega.business.IBOLookupException;
import com.idega.content.bean.ManagedContentBeans;
import com.idega.content.business.CategoryBean;
import com.idega.content.business.WebDAVMetadataResource;
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
 *  Last modified: $Date: 2007/02/23 14:35:48 $ by $Author: gediminas $
 * 
 * @author <a href="mailto:Joakim@idega.com">Joakim</a>
 * @version $Revision: 1.20 $
 */
public class WebDAVCategories  extends IWBaseComponent implements ManagedContentBeans, ActionListener{
	//Constants
	private static final String DEFAULT_CATEGORIES_BLOCK_ID = "categoriesBlock";
	/*public static final String CATEGORIES_BLOCK_ID = "categoriesBlockID";
	private static final String ADD_BUTTON_ID = "add_button_ID";
	private static final String SAVE_ID = "save_category_ID";
	private static final String ADD_CATEGORY_TEXT_ID = "addCategoryID";
	private static final String CATEGORY = "category_";		//Prefix for the id in the userinterface
	*/
	private static final int COLLUMNS = 3;		//Number of collumns to display the categories in

	private String resourcePath;
	private boolean setOnParent=false;
	private boolean displaySaveButton=true;
	private String setCategories;
	private boolean displayHeader=true;
	
//	List categoryStatus = new ArrayList();

	public WebDAVCategories() {
		setId(DEFAULT_CATEGORIES_BLOCK_ID);
	}
	
	public WebDAVCategories(String path){
		this();
		this.resourcePath = path;
	}
	
	public void setResourcePath(String path){
		this.resourcePath = path;
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
		WFResourceUtil localizer = WFResourceUtil.getResourceUtilContent();
		if(getDisplayHeader()){
			mainContainer.add(localizer.getHeaderTextVB("categories"));
		}
		mainContainer.add(getCategoriesTable());

		mainContainer.add(getAddCategoryContainer());

		return mainContainer;
	}
	
	/**
	 * <p>
	 * TODO tryggvil describe method getAddHeader
	 * </p>
	 * @return
	 */
	private boolean getDisplayHeader() {
		return this.displayHeader;
	}
	
	public void setDisplayHeader(boolean display){
		this.displayHeader=display;
	}

	/**
	 * <p> Creates a table with checkboxes for all the available categories </p>
	 * @param resourcePath
	 * @return table
	 */
	private Table getCategoriesTable() {
		Table categoriesTable = new Table();
		int count = 0;
			Collection selectedCategories = getSetCategoriesList();
			if(selectedCategories!=null){
				Iterator selectedIter = selectedCategories.iterator();
				while(selectedIter.hasNext()) {
					String categoryKey = selectedIter.next().toString();
					//Checkbox
					HtmlSelectBooleanCheckbox smc = new HtmlSelectBooleanCheckbox();
					setCategory(smc,categoryKey);
					smc.setValue(Boolean.TRUE);
					String id = getCategoryId()+count;
	//				System.out.println("CATEGORY-COMPONENT-ID:"+id);
					smc.setId(id);
					categoriesTable.add(smc,count%COLLUMNS + 1,count/COLLUMNS + 1);
					//Text
					HtmlOutputText catText = new HtmlOutputText();
					String catLabel = CategoryBean.getInstance().getCategoryName(categoryKey);
					catText.setValue(catLabel);
					categoriesTable.add(catText,count%COLLUMNS + 1,count/COLLUMNS + 1);
					count++;
				}
			}

			//Display all the non-selected categories
			Iterator nonSelectedIter = CategoryBean.getInstance().getCategories().iterator();
			while(nonSelectedIter.hasNext()) {
				Object nonSel = nonSelectedIter.next();
				if(selectedCategories == null || !selectedCategories.contains(nonSel)) {
					String categoryKey = nonSel.toString();
					//Checkbox
					HtmlSelectBooleanCheckbox smc = new HtmlSelectBooleanCheckbox();
					setCategory(smc,categoryKey);
					smc.setValue(Boolean.FALSE);
					String id = getCategoryId(count);
//					System.out.println("CATEGORY-COMPONENT-ID:"+id);
					smc.setId(id);
					categoriesTable.add(smc,count%COLLUMNS + 1,count/COLLUMNS + 1);
					//Text
					HtmlOutputText catText = new HtmlOutputText();
					String catLabel = CategoryBean.getInstance().getCategoryName(categoryKey);
					catText.setValue(catLabel);
					categoriesTable.add(catText,count%COLLUMNS + 1,count/COLLUMNS + 1);
					count++;
				}
			}

			
			categoriesTable.setColumns(Math.min(count,COLLUMNS));
			count--;
			categoriesTable.setRows(count/COLLUMNS + 1);

			if(getDisplaySaveButton()){
				//Add the save button
				if(this.resourcePath!=null && this.resourcePath.length()>0) {
					WFResourceUtil localizer = WFResourceUtil.getResourceUtilContent();
					HtmlCommandButton addCategoryButton = localizer.getButtonVB(getSaveButtonId(), "save", this);
					categoriesTable.add(addCategoryButton,1,count/COLLUMNS + 2);
					categoriesTable.setRows(count/COLLUMNS + 2);
				}
			}
			
			categoriesTable.setId(categoriesTable.getId() + "_ver");
//			categoriesTable.setRowStyleClass(1,"wf_listheading");
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
	 * <p>
	 * Gets the set categories, either from the resourcePath property or
	 * the setCategories property
	 * </p>
	 * @return
	 */
	private Collection getSetCategoriesList() {
		if(this.resourcePath!=null){
			IWContext iwuc = IWContext.getInstance();
			try {
				WebDAVMetadataResource resource = (WebDAVMetadataResource) IBOLookup.getSessionInstance(iwuc, WebDAVMetadataResource.class);
				return resource.getCategories(this.resourcePath);
			} catch (IBOLookupException e) {
				throw new RuntimeException(e);
			} catch (IOException e) {
				throw new RuntimeException(e);
			}
		}
		else if(this.setCategories!=null){
			return CategoryBean.getCategoriesFromString(this.setCategories);
		}
		return null;
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
		WebDAVCategories realCategories = null;
		while(superParent.getParent()!=null){
			superParent=superParent.getParent();
			if(superParent instanceof WebDAVCategories){
				realCategories = (WebDAVCategories)superParent;
			}
		}
		
		if(realCategories==null){
			realCategories = (WebDAVCategories) superParent.findComponent(getId());
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
	
	public void saveCategoriesSettings() {
		if (this.resourcePath == null){
			throw new RuntimeException("resourcePath is null");
		}
		//save the selection of categories to the article
		String categories = getEnabledCategories();
			
		IWContext iwuc = IWContext.getInstance();
		try {
			WebDAVMetadataResource resource = (WebDAVMetadataResource) IBOLookup.getSessionInstance(iwuc, WebDAVMetadataResource.class);
			resource.setCategories(resourcePath, categories.toString(), getSetCategoriesOnParent());
		} catch (IOException e) {
			throw new RuntimeException(e);
		}
	}

	/**
	 * @see javax.faces.component.StateHolder#saveState(javax.faces.context.FacesContext)
	 */
	public Object saveState(FacesContext ctx) {
		Object values[] = new Object[6];
		values[0] = super.saveState(ctx);
		values[1] = this.resourcePath;
		values[2] = Boolean.valueOf(this.setOnParent);
		values[3] = Boolean.valueOf(this.displaySaveButton);
		values[4] = this.setCategories;
		values[5] = Boolean.valueOf(this.displayHeader);
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
		this.setOnParent = ((Boolean) values[2]).booleanValue();
		this.displaySaveButton = ((Boolean) values[3]).booleanValue();
		this.setCategories=(String)values[4];
		this.displayHeader=((Boolean)values[5]).booleanValue();
	}
	
	
	public void encodeBegin(FacesContext context) throws IOException{
		super.encodeBegin(context);
	}

	public String getCategory(HtmlSelectBooleanCheckbox checkbox){
		String categoryKey =  (String) checkbox.getAttributes().get("categoryKey");
		return categoryKey;
	}
	
	public void setCategory(HtmlSelectBooleanCheckbox checkbox,String categoryKey){
		checkbox.getAttributes().put("categoryKey",categoryKey);
	}

	/**
	 * <p>
	 * Sets if the parent folder (of the current resource)should be set with the same category attributes as the current resource.
	 * </p>
	 * @param setOnParent
	 */
	public void setCategoriesOnParent(boolean setOnParent){
		this.setOnParent=setOnParent;
	}
	
	public boolean getSetCategoriesOnParent(){
		return this.setOnParent;
	}
	
	public void setDisplaySaveButton(boolean displaySave){
		this.displaySaveButton=displaySave;
	}
	
	public boolean getDisplaySaveButton(){
		return this.displaySaveButton;
	}

	/**
	 * <p>
	 * Set categories to preset with, this works only if resourcePath is not set.
	 * </p>
	 * @param setCategories
	 */
	public void setCategories(String setCategories) {
		this.setCategories=setCategories;
	}
}
