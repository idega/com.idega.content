package com.idega.content.presentation.categories;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Locale;

import javax.faces.component.UIComponent;

import com.idega.content.business.ContentConstants;
import com.idega.content.business.categories.CategoryBean;
import com.idega.content.data.ContentCategory;
import com.idega.core.localisation.business.ICLocaleBusiness;
import com.idega.core.localisation.business.LocalesComparator;
import com.idega.idegaweb.IWBundle;
import com.idega.idegaweb.IWResourceBundle;
import com.idega.presentation.Block;
import com.idega.presentation.IWContext;
import com.idega.presentation.Image;
import com.idega.presentation.Layer;
import com.idega.presentation.PresentationObject;
import com.idega.presentation.Span;
import com.idega.presentation.Table2;
import com.idega.presentation.TableBodyRowGroup;
import com.idega.presentation.TableCell2;
import com.idega.presentation.TableHeaderRowGroup;
import com.idega.presentation.TableRow;
import com.idega.presentation.text.Text;
import com.idega.util.CoreConstants;

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
		IWResourceBundle iwrb = getResourceBundle(iwc);
		
		Layer container = new Layer();
		add(container);
		
		if (locale == null) {
			container.add(new Text(getResourceBundle(iwc).getLocalizedString("select_locale_first", "Please, select locale first!")));
			return;
		}
		
		Layer explanation = new Layer();
		explanation.setStyleClass("categoriesHelpTextStyle");
		container.add(explanation);
		StringBuilder explText = new StringBuilder(iwrb.getLocalizedString("click", "Click")).append(CoreConstants.SPACE).append("'");
		explText.append(iwrb.getLocalizedString("undefined", "Undefined")).append("' ");
		explText.append(iwrb.getLocalizedString("categories_explanation", "to make that category available in that locale and to localize it"));
		Text text = new Text(explText.toString());
		explanation.add(text);
		
		addCategoriesList(iwc, container);
	}
	
	private void addCategoriesList(IWContext iwc, Layer container) {
		Locale currentLocale = iwc.getCurrentLocale();
		if (currentLocale == null) {
			return;
		}
		
		IWBundle bundle = getBundle(iwc);
		IWResourceBundle iwrb = bundle.getResourceBundle(iwc);
		Collection<ContentCategory> categories = CategoryBean.getInstance().getCategories();
		
		if (categories == null) {
			addNoCategoriesMessage(iwc, container);
			return;
		}
		if (categories.size() == 0) {
			addNoCategoriesMessage(iwc, container);
			return;
		}
		
		Table2 table = new Table2();
		table.setStyleClass("categoriesTable");
		container.add(table);
		
		List<Locale> locales = ICLocaleBusiness.getListOfLocalesJAVA();
		if (locales == null) {
			return;
		}
		try {
			locales.remove(currentLocale);
		} catch(Exception e) {}
		
		Collections.sort(locales, new LocalesComparator());
		List<Locale> sortedLocales = new ArrayList<Locale>();
		sortedLocales.add(currentLocale);
		for (int i = 0; i < locales.size(); i++) {
			sortedLocales.add(locales.get(i));
		}
		
		TableHeaderRowGroup header = table.createHeaderRowGroup();
		TableRow headerRow = header.createRow();
		headerRow.setStyleClass("categoriesTableHeader");
		for (int i = 0; i < sortedLocales.size(); i++) {
			TableCell2 cell = headerRow.createCell();
			cell.setStyleClass("name");
			cell.add(new Text(sortedLocales.get(i).getDisplayLanguage()));
		}
		headerRow.createCell().add(new Text(iwrb.getLocalizedString("status", "Status")));
		headerRow.createCell().add(new Text(iwrb.getLocalizedString("delete", "Delete")));
		
		TableBodyRowGroup body = table.createBodyRowGroup();
		TableRow bodyRow = null;
		String name = null;
		String undefined = iwrb.getLocalizedString("undefined", "Undefined");
		String containerId = container.getId();
		String tableId = table.getId();
		String categoryId = null;
		Locale l = null;
		String language = null;
		TableCell2 cell = null;
		for (ContentCategory category : categories) {
			categoryId = category.getId();
			
			bodyRow = body.createRow();
			bodyRow.setStyleClass("categoryBodyRow");
			for (int i = 0; i < sortedLocales.size(); i++) {
				l = sortedLocales.get(i);
				language = l.toString();
				
				name = category.getName(language);
				cell = bodyRow.createCell();
				cell.setStyleClass("categoriesTableCellStyle name");
				cell.add(getCategoryNameCellContent(name == null ? undefined : name, categoryId, iwrb, language));
			}
			bodyRow.createCell().add(getDisableCellContent(category, bundle, iwrb, null));
			bodyRow.createCell().add(getDeleteCellContent(bundle, iwrb, categoryId, containerId, tableId, null));
		}
	}
	
	private UIComponent getCategoryNameCellContent(String name, String id, IWResourceBundle iwrb, String language) {
		Text container = new Text(name);
		addAttributes(container, id, "changeCategoryNameLabelStyle", language);
		container.setTitle(iwrb.getLocalizedString("change_category_name", "Click to change name"));
		return container;
	}
	
	private UIComponent getDisableCellContent(ContentCategory category, IWBundle bundle, IWResourceBundle iwrb, String language) {
		Image usage = null;
		if (category.isDisabled()) {
			usage = new Image(bundle.getVirtualPathWithFileNameString("images/disabled.png"), iwrb.getLocalizedString("enable_category", "Enable category"), 16, 16);
			usage.setMarkupAttribute(isEnabledProperty, "false");
		}
		else {
			usage = new Image(bundle.getVirtualPathWithFileNameString("images/enabled.png"), iwrb.getLocalizedString("disable_category", "Disable category"), 16, 16);
			usage.setMarkupAttribute(isEnabledProperty, "true");
		}
		addAttributes(usage, category.getId(), "changeCategoryUsageImageStyle", language);
		return usage;
	}
	
	private UIComponent getDeleteCellContent(IWBundle bundle, IWResourceBundle iwrb, String id, String containerId, String tableId, String language) {
		Image delete = new Image(bundle.getVirtualPathWithFileNameString("images/delete.png"), iwrb.getLocalizedString("delete_category", "Delete category"), 16, 16);
		addAttributes(delete, id, "deleteCategoryImageStyle", language);
		delete.setMarkupAttribute("categoriescontainerid", containerId);
		delete.setMarkupAttribute("categoriestableid", tableId);
		return delete;
	}
	
	private void addAttributes(PresentationObject po, String id, String styleClass, String language) {
		po.setMarkupAttribute("categoryid", id);
		if (language != null) {
			po.setMarkupAttribute("language", language);
		}
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