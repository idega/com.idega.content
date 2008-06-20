package com.idega.content.themes.helpers.business;

import java.text.Collator;
import java.util.Comparator;
import java.util.Locale;

import com.idega.content.themes.helpers.bean.Theme;

public class ThemesComparator implements Comparator<Theme> {
	
	private Locale locale = null;
	
	public ThemesComparator(Locale locale) {
		this.locale = locale;
	}

	public int compare(Theme theme1, Theme theme2) {
		Collator collator = locale == null ? Collator.getInstance() : Collator.getInstance(locale);
		
		return collator.compare(theme1.getName(), theme2.getName());
	}

}
