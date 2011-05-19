package com.idega.content.business.categories;

import java.util.List;

import org.jdom.Document;

import com.idega.business.SpringBeanName;
import com.idega.content.data.ContentCategory;

public interface CategoriesEngine {
	
	public List<String> getInfo();
	
	public Document getCategoriesList(String locale);
	
	public List<ContentCategory> getCategoriesByLocale(String locale);
	
	/**
	 * Disables or enables usage of some category.
	 * @param id Existing category id.
	 * @param disable true, if category should be disabled.
	 * @return 
	 */
	public String manageCategoryUsage(String id, boolean disable);
	
	/**
	 * Changes category name in categories.xml file.
	 * @param id Existing category id.
	 * @param locale Locale settings name. Example: "lt_LT" - for Lithuania, "is_IS" - for Iceland, for english language - "en"
	 * @param newName New category name that should be saved
	 * @return true, if passed category exists
	 */
	public boolean renameCategory(String id, String locale, String newName);
	
	/**
	 * Adds new category to categories.xml file and to database. Publishes CategoryAddedEvent event.
	 * @param name New category name.
	 * @param locale Locale settings name. Example: "lt_LT" - for Lithuania, "is_IS" - for Iceland, for english language - "en"
	 * @return An *.xml document based on org.jdom.Document or null on failure.
	 */
	public Document addCategory(String name, String locale);
	
	/**
	 * Deletes category form categories.xml file AND SHOULD FROM DATABASE. Publishes CategoryDeletedEvent event.
	 * @param id Category id. Example: "Category name"
	 * @return true, if category successfully deleted
	 */
	public boolean deleteCategory(String id);

}