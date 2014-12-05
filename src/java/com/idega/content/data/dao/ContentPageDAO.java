package com.idega.content.data.dao;

import java.util.List;

import com.idega.content.data.ContentPage;
import com.idega.core.persistence.GenericDao;

public interface ContentPageDAO extends GenericDao {

	/**
	 * Returns content pages having all their fields loaded.
	 * @param pageId
	 * @return
	 */
	public List<ContentPage> getContentPagesInPage(Integer pageId);

	public ContentPage getContentPage(Long id);

	/**
	 * Returns the last positioned page position, if there is no page
	 * for this page returns -1.
	 * @param clubId
	 * @return
	 */
	public Integer getLastPositionForContentPage(int pageId);

	public void removeContentPage(Long id);

	public ContentPage storeContentPage(Integer pageId, Long id, String headline, String body);

}