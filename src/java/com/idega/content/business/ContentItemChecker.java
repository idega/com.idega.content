package com.idega.content.business;

import java.util.List;

import com.idega.business.SpringBeanName;

@SpringBeanName("contentItemChecker")
public interface ContentItemChecker {
	
	public boolean deleteDummyArticles(List<String> paths);
	
}
