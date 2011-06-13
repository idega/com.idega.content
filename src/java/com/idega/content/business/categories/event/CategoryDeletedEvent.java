/**
 * 
 */
package com.idega.content.business.categories.event;

import org.springframework.context.ApplicationEvent;

/**
 * @author martynas
 *
 */

public class CategoryDeletedEvent extends ApplicationEvent {

	private static final long serialVersionUID = 8965531315770628189L;

	private String categoryId = null;
	
	public CategoryDeletedEvent(String categoryId) {
		super(categoryId);
		this.categoryId = categoryId;
	}

	public String getCategoryId() {
		return categoryId;
	}

	public void setCategoryId(String categoryId) {
		this.categoryId = categoryId;
	}
}
