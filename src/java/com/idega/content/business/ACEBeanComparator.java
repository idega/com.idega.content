/*
 * $Id: ACEBeanComparator.java,v 1.1 2005/01/18 17:40:11 gummi Exp $
 * Created on 12.1.2005
 *
 * Copyright (C) 2005 Idega Software hf. All Rights Reserved.
 *
 * This software is the proprietary information of Idega hf.
 * Use is subject to license terms.
 */
package com.idega.content.business;

import java.text.Collator;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Locale;
import com.idega.content.data.ACEBean;
import com.idega.slide.util.AccessControlEntry;


/**
 * 
 *  Last modified: $Date: 2005/01/18 17:40:11 $ by $Author: gummi $
 * 
 * @author <a href="mailto:gummi@idega.com">Gudmundur Agust Saemundsson</a>
 * @version $Revision: 1.1 $
 */
public class ACEBeanComparator implements Comparator {

	  private Collator collator;
		private Locale iLocale;
		
		private List principalTypePriorityList = null;
		
		public ACEBeanComparator() {
			principalTypePriorityList = new ArrayList();
			principalTypePriorityList.add(String.valueOf(AccessControlEntry.PRINCIPAL_TYPE_STANDARD));
			principalTypePriorityList.add(String.valueOf(AccessControlEntry.PRINCIPAL_TYPE_OTHER));
			principalTypePriorityList.add(String.valueOf(AccessControlEntry.PRINCIPAL_TYPE_ROLE));
			principalTypePriorityList.add(String.valueOf(AccessControlEntry.PRINCIPAL_TYPE_GROUP));
			principalTypePriorityList.add(String.valueOf(AccessControlEntry.PRINCIPAL_TYPE_USER));
		}
		
		public ACEBeanComparator(Locale locale) {
			this();
			iLocale = locale;
		}
		
		/* (non-Javadoc)
		 * @see java.util.Comparator#compare(java.lang.Object, java.lang.Object)
		 */
		public int compare(Object o1, Object o2) {
			ACEBean b1 = (ACEBean) o1;
			ACEBean b2 = (ACEBean) o2;
			if(b1.getPrincipalType()!=b2.getPrincipalType()){
				return ((principalTypePriorityList.indexOf(String.valueOf(b1.getPrincipalType())) > principalTypePriorityList.indexOf(String.valueOf(b2.getPrincipalType())))?1:-1);
			} else if (iLocale != null) {
				collator = Collator.getInstance(iLocale);
				return collator.compare(b1.getPrincipalDisplayName(), b2.getPrincipalDisplayName());
			}
			else {
				collator = Collator.getInstance();
				return collator.compare(b1.getPrincipalDisplayName(), b2.getPrincipalDisplayName());
			}
		}
}
