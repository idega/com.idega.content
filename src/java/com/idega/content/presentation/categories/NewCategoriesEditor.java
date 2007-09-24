package com.idega.content.presentation.categories;

import java.util.Locale;

import javax.faces.component.UIComponent;

import com.idega.content.business.ContentConstants;
import com.idega.content.business.ContentUtil;
import com.idega.core.localisation.presentation.LocalePresentationUtil;
import com.idega.idegaweb.IWResourceBundle;
import com.idega.presentation.Block;
import com.idega.presentation.IWContext;
import com.idega.presentation.Layer;
import com.idega.presentation.PresentationObject;
import com.idega.presentation.Span;
import com.idega.presentation.text.Text;
import com.idega.presentation.ui.DropdownMenu;
import com.idega.presentation.ui.GenericButton;
import com.idega.presentation.ui.TextInput;

public class NewCategoriesEditor extends Block {

	@Override
	public void main(IWContext iwc) {
		IWResourceBundle iwrb = getResourceBundle(iwc);
		
		Layer container = new Layer();
		add(container);
		
		if (!ContentUtil.hasContentEditorRoles(iwc)) {
			container.add(new Text(iwrb.getLocalizedString("insufficient_rights", "Sorry, You do not have enough rights to view this page.")));
			return;
		}
		
		Layer categoriesByLocaleContainer = new Layer();
		String categoriesId = categoriesByLocaleContainer.getId();
		
		Span label = new Span();
		label.add(iwrb.getLocalizedString("select_locale", "Select locale: "));
		label.setStyleClass("categoriesFontStyle");
		container.add(label);
		
		DropdownMenu locales = LocalePresentationUtil.getAvailableLocalesDropdown(iwc.getIWMainApplication(), "availableLocales");
		container.add(locales);
		locales.setStyleClass("localesForCategoriesDropDownMenuStyle");
		Locale currentLocale = iwc.getCurrentLocale();
		locales.setSelectedElement(currentLocale.toString());
		locales.setMarkupAttribute("containerid", categoriesId);
		
		container.add(addNewCategoryContainer(iwc, locales.getId(), categoriesId));
		
		categoriesByLocaleContainer.setStyleClass("categoriesByLocaleContainerStyle");
		categoriesByLocaleContainer.add(new CategoriesListViewer(currentLocale));
		container.add(categoriesByLocaleContainer);
	}
	
	private UIComponent addNewCategoryContainer(IWContext iwc, String dropdownId, String containerId) {
		Layer newCategory = new Layer();
		
		IWResourceBundle iwrb = getResourceBundle(iwc);
		Span label = new Span();
		label.add(new Text(iwrb.getLocalizedString("new_category", "New category")));
		newCategory.add(label);
		
		TextInput categoryName = new TextInput();
		String categoryNameId = categoryName.getId();
		addMarkupAttributes(categoryName, dropdownId, categoryNameId, containerId);
		categoryName.setStyleClass("nameForNewCategoryInputStyle");
		newCategory.add(categoryName);
		
		GenericButton save = new GenericButton(iwrb.getLocalizedString("add", "Add"));
		addMarkupAttributes(save, dropdownId, categoryNameId, containerId);
		save.setStyleClass("addCategoryButtonStyle");
		newCategory.add(save);
		
		return newCategory;
	}
	
	private void addMarkupAttributes(PresentationObject po, String dropdownId, String categoryNameId, String containerId) {
		po.setMarkupAttribute("localesid", dropdownId);
		po.setMarkupAttribute("categorynameid", categoryNameId);
		po.setMarkupAttribute("categorieslistid", containerId);
	}

	@Override
	public String getBundleIdentifier() {
		return ContentConstants.IW_BUNDLE_IDENTIFIER;
	}
	
}
