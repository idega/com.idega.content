package com.idega.content.data;

import java.text.Collator;
import java.util.Comparator;
import java.util.Locale;

public class ContentCategoryComparator implements Comparator<ContentCategory> {

	private Locale locale;
	
	public ContentCategoryComparator(Locale locale) {
		this.locale = locale;
	}
	
	public int compare(ContentCategory category1, ContentCategory category2) {
		String name1 = category1.getName(locale.toString());
		String name2 = category2.getName(locale.toString());
		
		Collator collator = Collator.getInstance(locale);
		return collator.compare(name1, name2);
	}

}
