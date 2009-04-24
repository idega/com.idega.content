package com.idega.content.themes.helpers.business;

import java.text.Collator;
import java.util.Comparator;
import java.util.Locale;

import com.idega.content.themes.helpers.bean.Theme;

public class ThemesComparator implements Comparator<Theme> {
	
	private Locale locale;
	private Collator collator;
	
	public ThemesComparator(Locale locale) {
		this.locale = locale;
		
		collator = this.locale == null ? Collator.getInstance() : Collator.getInstance(this.locale);
	}

	public int compare(Theme theme1, Theme theme2) {
		return collator.compare(theme1.getName().toLowerCase(locale), theme2.getName().toLowerCase(locale));
	}

}
