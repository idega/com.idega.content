package com.idega.content.presentation.categories;

import java.util.List;
import java.util.Locale;

import javax.faces.component.UIComponent;

import com.idega.business.SpringBeanLookup;
import com.idega.content.business.ContentConstants;
import com.idega.content.business.categories.CategoriesEngine;
import com.idega.content.data.ContentCategory;
import com.idega.idegaweb.IWBundle;
import com.idega.idegaweb.IWResourceBundle;
import com.idega.presentation.Block;
import com.idega.presentation.IWContext;
import com.idega.presentation.Image;
import com.idega.presentation.Layer;
import com.idega.presentation.PresentationObject;
import com.idega.presentation.Span;
import com.idega.presentation.text.Text;

public class CategoriesListViewer extends Block {

	private String locale = null;
	private String isEnabledProperty = "isenabled";

	public CategoriesListViewer(Locale locale) {
		this(locale.toString());
	}
	
	public CategoriesListViewer(String locale) {
		this.locale = locale;
	}
	
	@Override
	public void main(IWContext iwc) {		
		Layer container = new Layer();
		add(container);
		
		if (locale == null) {
			container.add(new Text(getResourceBundle(iwc).getLocalizedString("select_locale_first", "Please, select locale first!")));
			return;
		}
		
		addCategoriesList(iwc, container);
	}
	
	private void addCategoriesList(IWContext iwc, Layer container) {
		List<ContentCategory> categories = null;
		categories = ((CategoriesEngine) SpringBeanLookup.getInstance().getSpringBean(iwc, CategoriesEngine.class)).getCategoriesByLocale(locale);
		
		if (categories == null) {
			addNoCategoriesMessage(iwc, container);
			return;
		}
		if (categories.size() == 0) {
			addNoCategoriesMessage(iwc, container);
			return;
		}

		for (int i = 0; i < categories.size(); i++) {
			container.add(getCategoryContainer(iwc, categories.get(i)));
		}
	}
	
	private UIComponent getCategoryContainer(IWContext iwc, ContentCategory category) {
		IWBundle bundle = getBundle(iwc);
		IWResourceBundle iwrb = getResourceBundle(iwc);
		Layer container = new Layer();
		container.setStyleClass("categoryContainer");
		
		Span name = new Span();
		name.add(category.getName(locale));
		addAttributes(name, category.getId(), "changeCategoryNameLabelStyle");
		name.setToolTip(iwrb.getLocalizedString("change_category_name", "Click to change name"));
		container.add(name);
		
		Image usage = null;
		if (category.isDisabled()) {
			usage = new Image(bundle.getVirtualPathWithFileNameString("images/disabled.png"), iwrb.getLocalizedString("enable_category", "Enable category"), 24, 24);
			usage.setMarkupAttribute(isEnabledProperty, "false");
		}
		else {
			usage = new Image(bundle.getVirtualPathWithFileNameString("images/enabled.png"), iwrb.getLocalizedString("disable_category", "Disable category"), 24, 24);
			usage.setMarkupAttribute(isEnabledProperty, "true");
		}
		addAttributes(usage, category.getId(), "changeCategoryUsageImageStyle");
		container.add(usage);
		
		Image delete = new Image(bundle.getVirtualPathWithFileNameString("images/delete.png"), iwrb.getLocalizedString("delete_category", "Delete category"), 24, 24);
		addAttributes(delete, category.getId(), "deleteCategoryImageStyle");
		container.add(delete);
		
		return container;
	}
	
	private void addAttributes(PresentationObject po, String id, String styleClass) {
		po.setMarkupAttribute("categoryid", id);
		po.setMarkupAttribute("language", locale);
		po.setStyleClass(styleClass);
	}
	
	private void addNoCategoriesMessage(IWContext iwc, Layer container) {
		Span message = new Span();
		message.setStyleClass("categoriesFontStyle");
		message.add(getResourceBundle(iwc).getLocalizedString("no_categories_found", "There are no categories."));
		container.add(message);
	}
	
	@Override
	public String getBundleIdentifier() {
		return ContentConstants.IW_BUNDLE_IDENTIFIER;
	}

}