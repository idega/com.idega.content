package com.idega.content.business.categories;

import java.rmi.RemoteException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.jdom.Document;

import com.idega.content.business.ContentConstants;
import com.idega.content.business.categories.event.CategoryAddedEvent;
import com.idega.content.business.categories.event.CategoryDeletedEvent;
import com.idega.content.data.ContentCategory;
import com.idega.content.presentation.categories.CategoriesListViewer;
import com.idega.core.builder.business.BuilderService;
import com.idega.core.builder.business.BuilderServiceFactory;
import com.idega.idegaweb.IWBundle;
import com.idega.idegaweb.IWMainApplication;
import com.idega.idegaweb.IWResourceBundle;
import com.idega.presentation.IWContext;
import com.idega.util.CoreConstants;
import com.idega.util.CoreUtil;
import com.idega.util.StringUtil;
import com.idega.util.expression.ELUtil;

public class CategoriesEngineBean implements CategoriesEngine {

	public List<String> getInfo() {
		IWContext iwc = CoreUtil.getIWContext();
		if (iwc == null) {
			return null;
		}
		
		IWResourceBundle iwrb = null;
		try {
			iwrb = IWMainApplication.getIWMainApplication(iwc).getBundle(ContentConstants.IW_BUNDLE_IDENTIFIER).getResourceBundle(iwc);
		} catch(Exception e) {
			e.printStackTrace();
			return null;
		}
		if (iwrb == null) {
			return null;
		}
		
		List<String> info = new ArrayList<String>();
		info.add(iwrb.getLocalizedString("loading", "Loading..."));												//	0
		info.add(iwrb.getLocalizedString("disable_category_message", "Do you want to disable this category?"));	//	1
		info.add(iwrb.getLocalizedString("enable_category_message", "Do you want to enable this category?"));	//	2
		info.add(iwrb.getLocalizedString("delete_category_message", "Do you want to delete this category?"));	//	3
		info.add(iwrb.getLocalizedString("disable_category", "Disable category"));								//	4
		info.add(iwrb.getLocalizedString("enable_category", "Enable category"));								//	5
		info.add(iwrb.getLocalizedString("deleting", "Deleting..."));											//	6
		info.add(iwrb.getLocalizedString("enter_name_for_category", "Please, enter name for category!"));		//	7
		info.add(iwrb.getLocalizedString("saving", "Saving..."));												//	8
		info.add(iwrb.getLocalizedString("no_categories_found", "There are no categories."));					//	9
		
		return info;
	}
	
	public Document getCategoriesList(String locale) {
		return getCategoriesListViewer(locale);
	}
	
	public List<ContentCategory> getCategoriesByLocale(String locale) {
		CategoryBean bean = CategoryBean.getInstance();
		if (bean == null) {
			return null;
		}
		
		List<ContentCategory> categories = new ArrayList<ContentCategory>(bean.getCategories());
		if (categories.size() == 0) {
			return null;
		}
		
		ContentCategory category = null;
		Map<String, String> categoryNames = null;
		List<ContentCategory> filtered = new ArrayList<ContentCategory>();
		for (int i = 0; i < categories.size(); i++) {
			category = categories.get(i);
			categoryNames = category.getNames();
			if (categoryNames.get(locale) != null) {
				filtered.add(category);
			}
		}
		
		if (filtered.size() == 0) {
			return null;
		}
		
		return filtered;
	}

	public boolean deleteCategory(String id) {
		ContentCategory category = getCategory(id);
		if (category == null) {
			return false;
		}
		
		if (CategoryBean.getInstance().deleteCategory(id)) {
			CategoryBean.getInstance().storeCategories(true);
			ELUtil.getInstance().publishEvent(new CategoryDeletedEvent(id));
			return true;
		}
		
		return false;
	}

	public boolean renameCategory(String id, String locale, String newName) {
		ContentCategory category = getCategory(id);
		if (category == null) {
			return false;
		}
		
		category.addName(locale, newName);
		
		CategoryBean.getInstance().storeCategories(true);
		
		return true;
	}
	
	/**
	 * @param id Category id.
	 * @return ContentCategory instance if exists.
	 */
	private ContentCategory getCategory(String id) {
		if (id == null) {
			return null;
		}
		if (CoreConstants.EMPTY.equals(id)) {
			return null;
		}
		
		return CategoryBean.getInstance().getCategory(id);
	}
	
	public String manageCategoryUsage(String id, boolean disable) {
		ContentCategory category = getCategory(id);
		if (category == null) {
			return null;
		}
		
		category.setDisabled(disable);
		
		CategoryBean.getInstance().storeCategories(true);
		
		IWBundle bundle = IWMainApplication.getDefaultIWMainApplication().getBundle(ContentConstants.IW_BUNDLE_IDENTIFIER);
		if (bundle == null) {
			return null;
		}
		
		if (disable) {
			return bundle.getVirtualPathWithFileNameString("images/disabled.png");
		}
		return bundle.getVirtualPathWithFileNameString("images/enabled.png");	
	}
	
	public Document addCategory(String name, String locale) {
		if (name == null) {
			return null;
		}
		
		String categoryId = CategoryBean.getInstance().addCategory(name, locale);
		if (StringUtil.isEmpty(categoryId))
			return null;
			
		ELUtil.getInstance().publishEvent(new CategoryAddedEvent(categoryId));
		return getCategoriesListViewer(locale);
	}
	
	private Document getCategoriesListViewer(String locale) {
		if (locale == null) {
			return null;
		}
		if (CoreConstants.EMPTY.equals(locale)) {
			return null;
		}
		
		IWContext iwc = CoreUtil.getIWContext();
		if (iwc == null) {
			return null;
		}
		
		BuilderService builder = null;
		try {
			builder = BuilderServiceFactory.getBuilderService(iwc);
		} catch (RemoteException e) {
			e.printStackTrace();
			return null;
		}
		if (builder == null) {
			return null;
		}
		
		return builder.getRenderedComponent(iwc, new CategoriesListViewer(locale), false);
	}

}
