package com.idega.content.presentation.categories;

import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

import javax.faces.component.UIComponent;

import com.idega.content.business.ContentConstants;
import com.idega.content.business.categories.CategoryBean;
import com.idega.content.data.ContentCategory;
import com.idega.core.localisation.business.ICLocaleBusiness;
import com.idega.idegaweb.IWResourceBundle;
import com.idega.presentation.Block;
import com.idega.presentation.IWContext;
import com.idega.presentation.Layer;
import com.idega.presentation.PresentationObject;
import com.idega.presentation.Script;
import com.idega.presentation.text.Text;
import com.idega.presentation.ui.GenericButton;
import com.idega.presentation.ui.TextInput;
import com.idega.util.CoreConstants;

public class CategoryEditor extends Block {
	
	private String categoryId = null;
	private String locale = null;
	
	private String changeCategoryNameInputStyle = "changeCategoryNameInputStyle";
	
	public CategoryEditor() {
		super();
	}
	
	public CategoryEditor(String categoryId, String locale) {
		this();
		this.categoryId = categoryId;
		this.locale = locale;
	}

	@Override
	public void main(IWContext iwc) throws Exception {
		initParameters(iwc);
		
		Layer container = new Layer();
		add(container);
		
		IWResourceBundle iwrb = getResourceBundle(iwc);
		
		if (categoryId == null || locale == null) {
			container.add(iwrb.getLocalizedString("missing_parameters_for_category_editor", "Please, select category and language first!"));
			return;
		}
		
		Text label = new Text(iwrb.getLocalizedString("rename_category", "Rename category"));
		container.add(label);
		
		TextInput newName = new TextInput();
		String newNameInputId = newName.getId();

		String oldName = null;
		try {
			oldName = CategoryBean.getInstance().getCategory(categoryId).getName(locale);
		} catch (Exception e) {}
		if (oldName != null) {
			newName.setValue(oldName);
		}
		addAttributes(newName, newNameInputId, changeCategoryNameInputStyle, true, null);
		container.add(newName);
		
		GenericButton save = new GenericButton(iwrb.getLocalizedString("save", "Save"));
		addAttributes(save, newNameInputId, "changeCategoryNameButtonStyle", true, null);
		if (iwc.isIE()) {
			//	TODO add action
		}
		container.add(save);
		
		UIComponent notLocalized = getNotLocalizedCategories(iwc);
		if (notLocalized != null) {
			container.add(notLocalized);
		}
		
		if (!iwc.isIE()) {
			Script script = new Script();
			script.addScriptLine("initializeCategoryEditorWindowActions();");
			script.addScriptLine(new StringBuilder("$('").append(newNameInputId).append("').focus();").toString());
			container.add(script);
		}
	}
	
	@SuppressWarnings("unchecked")
	private UIComponent getNotLocalizedCategories(IWContext iwc) {
		List<Locale> locales = ICLocaleBusiness.getListOfLocalesJAVA();
		if (locales == null) {
			return null;
		}
		if (locales.size() == 0) {
			return null;
		}
		
		ContentCategory category = CategoryBean.getInstance().getCategory(categoryId);
		List<Locale> notLocalized = new ArrayList<Locale>();
		Locale l = null;
		String language = null;
		for (int i = 0; i < locales.size(); i++) {
			l = locales.get(i);
			language = l.toString();
			if (!locale.equals(language) && category.getName(language) == null) {
				notLocalized.add(l);
			}
		}
		if (notLocalized.size() == 0) {
			return null;
		}
		
		IWResourceBundle iwrb = getResourceBundle(iwc);
		Layer container = new Layer();
		
		container.add(new Text(iwrb.getLocalizedString("no_localization_warning", "This category misses localization for:")));
		String lbl = iwrb.getLocalizedString("add_localized_name", "add localized name");
		List<String> inputIds = new ArrayList<String>();
		String newNameInputId = null;
		String localizedCategoryContainerStyle = "localizedCategoryContainerStyle";
		String localizedCategoryLabelStyle = "localizedCategoryLabelStyle";
		String removeContainerId = "removecontainerid";
		for (int i = 0; i < notLocalized.size(); i++) {
			l = notLocalized.get(i);
			
			Layer localization = new Layer();
			localization.setStyleClass(localizedCategoryContainerStyle);
			
			Text localizedCategory = new Text(new StringBuilder(l.getDisplayLanguage()).append(CoreConstants.COMMA).append(CoreConstants.SPACE).append(lbl).toString());
			localizedCategory.setStyleClass(localizedCategoryLabelStyle);
			localization.add(localizedCategory);
			
			TextInput localizedNameInput = new TextInput();
			newNameInputId = localizedNameInput.getId();
			addAttributes(localizedNameInput, newNameInputId, changeCategoryNameInputStyle, false, l.toString());
			inputIds.add(newNameInputId);
			localizedNameInput.setMarkupAttribute(removeContainerId, localization.getId());
			localization.add(localizedNameInput);
			
			container.add(localization);
		}
		
		return container;
	}
	
	private void addAttributes(PresentationObject po, String newNameInputId, String styleClass, boolean needReload, String language) {
		po.setMarkupAttribute("categoryid", categoryId);
		po.setMarkupAttribute("newnameinputid", newNameInputId);
		//po.setMarkupAttribute("reloadcategories", needReload);
		if (language == null) {
			language = locale;
		}
		po.setMarkupAttribute("language", language);
		po.setStyleClass(styleClass);
	}
	
	private void initParameters(IWContext iwc) {
		if (categoryId == null) {
			Object o = iwc.getParameter("categoryId");
			if (o instanceof String) {
				categoryId = (String) o;
			}
		}
		if (locale == null) {
			Object o = iwc.getParameter("categoryLocale");
			if (o instanceof String) {
				locale = (String) o;
			}
		}
	}

	public void setCategoryId(String categoryId) {
		this.categoryId = categoryId;
	}

	public void setLocale(String locale) {
		this.locale = locale;
	}
	
	@Override
	public String getBundleIdentifier() {
		return ContentConstants.IW_BUNDLE_IDENTIFIER;
	}

}
