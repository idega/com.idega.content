/**
 * $Id: CategoryComparator.java,v 1.2 2007/09/25 12:00:48 valdas Exp $
 * Created in 2007 by gediminas
 *
 * Copyright (C) 2000-2007 Idega Software hf. All Rights Reserved.
 *
 * This software is the proprietary information of Idega hf.
 * Use is subject to license terms.
 */
package com.idega.content.data;

import java.io.Serializable;
import java.text.Collator;
import java.util.Comparator;
import java.util.Locale;


/**
 * Compares two <code>ContentCategory</code> objects by their name in given locale 
 * 
 * @author <a href="mailto:gediminas@idega.com">Gediminas Paulauskas</a>
 */
public class CategoryComparator implements Comparator<ContentCategory>, Serializable {

	private static final long serialVersionUID = -2229023916545708322L;
	
	private Collator collator;
	private String lang;
	
	public CategoryComparator() {
		this(Locale.getDefault());
	}
	
	public CategoryComparator(Locale locale) {
		collator = Collator.getInstance(locale);
		lang = locale.toString();
	}

	public int compare(ContentCategory o1, ContentCategory o2) {
		String value1 = o1.getName(lang);
		String value2 = o2.getName(lang);
		
		int result = 0;

		if (value1 == null && value2 == null) {
			result = 0;
		}
		else if (value2 == null) {
			result = 1;
		}
		else if (value1 == null) {
			result = -1;
		}
		else {
			result = collator.compare(value1, value2);
		}
		
		return result;
	}
	
}
