package com.idega.content.business;

import java.util.List;
import java.util.Locale;

import com.idega.business.SpringBeanName;

@SpringBeanName("contentItemChecker")
public interface ContentItemChecker {
	
	public boolean deleteDummyArticles(List<String> paths);
	
	public boolean deleteContentItem(String path, Locale l);
	
}
