/**
 * $Id: CategoryComparator.java,v 1.1 2007/05/30 15:15:04 gediminas Exp $
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

	private Collator collator;
	private String lang;
	
	public CategoryComparator() {
		this(Locale.getDefault());
	}
	
	public CategoryComparator(Locale locale) {
		collator = Collator.getInstance(locale);
		lang = locale.toString();
	}

	/* (non-Javadoc)
	 * @see java.util.Comparator#compare(java.lang.Object, java.lang.Object)
	 */
	public int compare(ContentCategory o1, ContentCategory o2) {
		String n1 = o1.getName(lang);
		String n2 = o2.getName(lang);
		return collator.compare(n1, n2);
	}
	
}
