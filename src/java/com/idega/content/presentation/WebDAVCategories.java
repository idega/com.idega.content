/*
 * $Id: WebDAVCategories.java,v 1.26 2008/02/11 09:18:00 valdas Exp $
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
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;

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
import com.idega.content.business.WebDAVMetadataResource;
import com.idega.content.business.categories.CategoryBean;
import com.idega.content.data.ContentCategory;
import com.idega.core.localisation.business.ICLocaleBusiness;
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
 *  Last modified: $Date: 2008/02/11 09:18:00 $ by $Author: valdas $
 * 
 * @author <a href="mailto:Joakim@idega.com">Joakim</a>
 * @version $Revision: 1.26 $
 */
public class WebDAVCategories extends IWBaseComponent implements ManagedContentBeans, ActionListener{
	//Constants
	private static final String DEFAULT_CATEGORIES_BLOCK_ID = "categoriesBlock";
	private static final int COLLUMNS = 3;		//Number of collumns to display the categories in

	private String resourcePath;
	private String setCategories;
	
	private boolean setOnParent=false;
	private boolean displaySaveButton=true;
	private boolean displayHeader=true;
	private boolean addCategoryCreator = true;
	private boolean needDisplayCategoriesSelection = true;
	private boolean selectAllCategories = false;
	
	private boolean areCategoriesFetched = false;
	
	private List<ContentCategory> selectedCategories = null;
	private List<ContentCategory> notSelectedCategories = null;
	
	private int localizedCategories = 0;
	private String localeIdentity = null;

	public WebDAVCategories() {
		setId(DEFAULT_CATEGORIES_BLOCK_ID);
		
		selectedCategories = new ArrayList<ContentCategory>();
		notSelectedCategories = new ArrayList<ContentCategory>();
		
		areCategoriesFetched = false;
	}
	
	public WebDAVCategories(String path, String localeIdentity){
		this();
		this.resourcePath = path;
		this.localeIdentity = localeIdentity;
	}
	
	public void setResourcePath(String path){
		this.resourcePath = path;
	}
	
	protected void initializeComponent(FacesContext context) {
		add(getEditContainer(IWContext.getIWContext(context)));
	}

	public void reset(){
		getChildren().clear();
		setInitialized(false);
		IWContext iwc = IWContext.getInstance();
		try {
			WebDAVMetadataResource resource = (WebDAVMetadataResource) IBOLookup.getSessionInstance(iwc, WebDAVMetadataResource.class);
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
	private UIComponent getEditContainer(IWContext iwc) {
		WFContainer mainContainer = new WFContainer();
		WFResourceUtil localizer = WFResourceUtil.getResourceUtilContent();
		if(getDisplayHeader()){
			mainContainer.add(localizer.getHeaderTextVB("categories"));
		}
		mainContainer.add(getCategoriesTable(iwc));

		if (isAddCategoryCreator()) {
			mainContainer.add(getAddCategoryContainer());
		}

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
	private Table getCategoriesTable(IWContext iwc) {
		if (!areCategoriesFetched) {
			getSelectedAndNotSelectedCategories(iwc);
		}
		
		Table categoriesTable = new Table();
		int selectedCategoriesCount = 0;
		ContentCategory category = null;
		Locale locale = getLocale(iwc);
		for (int i = 0; i < selectedCategories.size(); i++) {
			category = selectedCategories.get(i);
						
			//	Checkbox
			HtmlSelectBooleanCheckbox smc = new HtmlSelectBooleanCheckbox();
			setCategory(smc, category.getId());
			smc.setValue(Boolean.TRUE);
			String id = getCategoryId()+selectedCategoriesCount;
			smc.setId(id);
			categoriesTable.add(smc,selectedCategoriesCount%COLLUMNS + 1,selectedCategoriesCount/COLLUMNS + 1);
			
			//	Text
			HtmlOutputText catText = new HtmlOutputText();
			String catLabel = category.getName(locale.toString());
			catText.setValue(catLabel);
			categoriesTable.add(catText,selectedCategoriesCount%COLLUMNS + 1,selectedCategoriesCount/COLLUMNS + 1);
			selectedCategoriesCount++;
		}

		//	Display all the non-selected categories
		for (int i = 0; i < notSelectedCategories.size(); i++) {
			category = notSelectedCategories.get(i);
			
			//Checkbox
			HtmlSelectBooleanCheckbox smc = new HtmlSelectBooleanCheckbox();
			setCategory(smc, category.getId());
			if (notSelectedCategories.size() == 1 && selectedCategories.size() == 0) {
				smc.setValue(Boolean.TRUE);
				smc.setSelected(true);
			}
			else {
				smc.setValue(Boolean.FALSE);
			}
			String id = getCategoryId(selectedCategoriesCount);
			smc.setId(id);
			categoriesTable.add(smc,selectedCategoriesCount%COLLUMNS + 1,selectedCategoriesCount/COLLUMNS + 1);
	
			//Text
			HtmlOutputText catText = new HtmlOutputText();
			String catLabel = category.getName(locale.toString());
			catText.setValue(catLabel);
			categoriesTable.add(catText,selectedCategoriesCount%COLLUMNS + 1,selectedCategoriesCount/COLLUMNS + 1);
			selectedCategoriesCount++;
		}
		
		categoriesTable.setColumns(Math.min(selectedCategoriesCount,COLLUMNS));
		selectedCategoriesCount--;
		categoriesTable.setRows(selectedCategoriesCount/COLLUMNS + 1);

		if (getDisplaySaveButton()) {
			//	Add the save button
			if(this.resourcePath!=null && this.resourcePath.length()>0) {
				WFResourceUtil localizer = WFResourceUtil.getResourceUtilContent();
				HtmlCommandButton addCategoryButton = localizer.getButtonVB(getSaveButtonId(), "save", this);
				categoriesTable.add(addCategoryButton,1,selectedCategoriesCount/COLLUMNS + 2);
				categoriesTable.setRows(selectedCategoriesCount/COLLUMNS + 2);
			}
		}
			
		categoriesTable.setId(categoriesTable.getId() + "_ver");
		categoriesTable.setStyleClass("wf_listtable");

		return categoriesTable;
	}
	
	@Override
	protected void updateComponent(FacesContext context) {
		this.getChildren().clear();
		add(getEditContainer(IWContext.getIWContext(context)));
	}
	
	private Locale getLocale(IWContext iwc) {
		Locale l = null;
		if (localeIdentity == null) {
			l = iwc.getCurrentLocale();
		}
		else {
			l = ICLocaleBusiness.getLocaleFromLocaleString(localeIdentity);
		}
		
		if (l == null) {
			l = Locale.ENGLISH;
		}
		
		return l;
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
	private Collection<String> getSetCategoriesList() {
		if (this.resourcePath != null) {
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
		else if (this.setCategories != null) {
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
		HtmlSelectBooleanCheckbox checkBox = null;
		//int count = 0;
		String categoryKey;
		String checkId;
		for (int i = 0; i < categoriesCount; i++) {
			checkId = getCategoryId(i);
			checkBox = (HtmlSelectBooleanCheckbox) getParent().findComponent(checkId);
			if (checkBox != null) {
				categoryKey = getCategory(checkBox);
				if (checkBox.isSelected()) {
					categories.append(categoryKey).append(CategoryBean.CATEGORY_DELIMETER);
				}
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
		Object values[] = new Object[7];
		values[0] = super.saveState(ctx);
		values[1] = this.resourcePath;
		values[2] = Boolean.valueOf(this.setOnParent);
		values[3] = Boolean.valueOf(this.displaySaveButton);
		values[4] = this.setCategories;
		values[5] = Boolean.valueOf(this.displayHeader);
		values[6] = Boolean.valueOf(this.selectAllCategories);
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
		this.selectAllCategories = (Boolean) values[6];
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

	public boolean isAddCategoryCreator() {
		return addCategoryCreator;
	}

	public void setAddCategoryCreator(boolean addCategoryCreator) {
		this.addCategoryCreator = addCategoryCreator;
	}

	public boolean isNeedDisplayCategoriesSelection(IWContext iwc) {
		if (!areCategoriesFetched) {
			getSelectedAndNotSelectedCategories(iwc);
		}
		
		return needDisplayCategoriesSelection;
	}
	
	public void getSelectedAndNotSelectedCategories(IWContext iwc) {
		this.selectedCategories = new ArrayList<ContentCategory>();
		notSelectedCategories = new ArrayList<ContentCategory>();
		
		Locale locale = getLocale(iwc);
		
		Collection<String> selectedCategories = getSetCategoriesList();
		if (selectedCategories != null) {
			ContentCategory category = null;
			for (Iterator<String> selectedIter = selectedCategories.iterator(); selectedIter.hasNext();) {
				String categoryKey = selectedIter.next();
				category = CategoryBean.getInstance().getCategory(categoryKey);
				if (category != null) {
					if (category.getName(locale.toString()) != null && !category.isDisabled()) {
						//	If category exists for current locale and it's not disabled
						this.selectedCategories.add(category);
					}
				}
			}
		}
		
		Collection<ContentCategory> categories = CategoryBean.getInstance().getCategories(locale);
		if (categories == null) {
			localizedCategories = 0;
		}
		else {
			localizedCategories = categories.size();
			
			for (ContentCategory category : categories) {
				if (!category.isDisabled() && (selectedCategories == null || !selectedCategories.contains(category.getId()))) {
					if (selectAllCategories) {
						this.selectedCategories.add(category);
					}
					else {
						this.notSelectedCategories.add(category);
					}
				}
			}
		}
		
		if (categories == null || categories.size() == 0) {
			needDisplayCategoriesSelection = false;
		}
		else if (this.selectedCategories.size() == 0 && notSelectedCategories.size() == 1) {
			needDisplayCategoriesSelection = false;
		}
		else {
			needDisplayCategoriesSelection = true;
		}
		
		areCategoriesFetched = true;
	}

	public int getLocalizedCategories() {
		return localizedCategories;
	}

	public void setLocaleIdentity(String localeIdentity) {
		this.localeIdentity = localeIdentity;
	}

	public boolean isSelectAllCategories() {
		return selectAllCategories;
	}

	public void setSelectAllCategories(boolean selectAllCategories) {
		this.selectAllCategories = selectAllCategories;
	}
	
}
