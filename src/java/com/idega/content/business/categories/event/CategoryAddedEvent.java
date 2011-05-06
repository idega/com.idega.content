/**
 * 
 */
package com.idega.content.business.categories.event;

import org.springframework.context.ApplicationEvent;

/**
 * @author martynas
 *
 */
public class CategoryAddedEvent extends ApplicationEvent {

	private static final long serialVersionUID = 2275382674128042070L;
	
	private String categoryId = null;
	
	public CategoryAddedEvent(String categoryId) {
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
