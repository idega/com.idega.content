package com.idega.content.presentation.categories;

import java.util.Locale;

import javax.faces.component.UIComponent;

import com.idega.content.business.ContentConstants;
import com.idega.content.business.ContentUtil;
import com.idega.idegaweb.IWResourceBundle;
import com.idega.presentation.Block;
import com.idega.presentation.IWContext;
import com.idega.presentation.Layer;
import com.idega.presentation.PresentationObject;
import com.idega.presentation.Script;
import com.idega.presentation.Span;
import com.idega.presentation.text.Text;
import com.idega.presentation.ui.GenericButton;
import com.idega.presentation.ui.TextInput;

public class NewCategoriesEditor extends Block {

	@Override
	public void main(IWContext iwc) {
		IWResourceBundle iwrb = getResourceBundle(iwc);
		Locale currentLocale = iwc.getCurrentLocale();
		
		Layer container = new Layer();
		container.setStyleClass("categoryEditor");
		add(container);
		
		if (!ContentUtil.hasContentEditorRoles(iwc)) {
			container.add(new Text(iwrb.getLocalizedString("insufficient_rights", "Sorry, you do not have enough rights to view this page.")));
			return;
		}
		
		Layer categoriesByLocaleContainer = new Layer();
		categoriesByLocaleContainer.setStyleClass("categoryLocaleEditor");
		
		container.add(addNewCategoryContainer(iwc, categoriesByLocaleContainer.getId(), currentLocale.toString()));
		
		categoriesByLocaleContainer.setStyleClass("categoriesByLocaleContainerStyle");
		categoriesByLocaleContainer.add(new CategoriesListViewer(currentLocale));
		container.add(categoriesByLocaleContainer);
	}
	
	private UIComponent addNewCategoryContainer(IWContext iwc, String containerId, String language) {
		Layer newCategory = new Layer();
		newCategory.setStyleClass("newCategoryContainer");
		
		IWResourceBundle iwrb = getResourceBundle(iwc);
		Span label = new Span();
		label.add(new Text(iwrb.getLocalizedString("new_category", "New category")));
		label.setStyleClass("newCategoryNameLabelStyle");
		newCategory.add(label);
		
		TextInput categoryName = new TextInput();
		String categoryNameId = categoryName.getId();
		addMarkupAttributes(categoryName, categoryNameId, containerId, language);
		categoryName.setStyleClass("nameForNewCategoryInputStyle");
		newCategory.add(categoryName);
		
		GenericButton save = new GenericButton(iwrb.getLocalizedString("add", "Add"));
		addMarkupAttributes(save, categoryNameId, containerId, language);
		save.setStyleClass("addCategoryButtonStyle");
		newCategory.add(save);
		
		Script script = new Script();
		script.addScriptLine(new StringBuilder("$('").append(categoryNameId).append("').focus();").toString());
		newCategory.add(script);
		
		return newCategory;
	}
	
	private void addMarkupAttributes(PresentationObject po, String categoryNameId, String containerId, String language) {
		po.setMarkupAttribute("language", language);
		po.setMarkupAttribute("categorynameid", categoryNameId);
		po.setMarkupAttribute("categorieslistid", containerId);
	}

	@Override
	public String getBundleIdentifier() {
		return ContentConstants.IW_BUNDLE_IDENTIFIER;
	}
	
}
