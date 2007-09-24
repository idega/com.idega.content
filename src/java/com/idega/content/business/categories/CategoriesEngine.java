package com.idega.content.business.categories;

import java.util.List;

import org.jdom.Document;

import com.idega.business.SpringBeanName;
import com.idega.content.data.ContentCategory;

@SpringBeanName("content.CategoriesEngineBean")
public interface CategoriesEngine {
	
	public List<String> getInfo();
	
	public Document getCategoriesList(String locale);
	
	public List<ContentCategory> getCategoriesByLocale(String locale);
	
	public String manageCategoryUsage(String id, boolean disable);
	
	public boolean renameCategory(String id, String locale, String newName);
	
	public Document addCategory(String name, String locale);

}