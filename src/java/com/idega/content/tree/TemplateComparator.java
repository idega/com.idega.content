package com.idega.content.tree;

import java.util.Comparator;

public class TemplateComparator implements Comparator<Template> {

	public int compare(Template prop1, Template prop2) {
		int result = 0;
		
		String value1 = prop1.getName();
		String value2 = prop2.getName();
		
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
			result = value1.compareTo(value2);
		}
		
		return result;
	}

}
