package com.idega.content.data.dao.impl;

import java.util.Collections;
import java.util.List;

import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.context.annotation.Scope;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import com.idega.content.data.ContentPage;
import com.idega.content.data.dao.ContentPageDAO;
import com.idega.core.persistence.Param;
import com.idega.core.persistence.impl.GenericDaoImpl;

@Repository
@Scope(BeanDefinition.SCOPE_SINGLETON)
@Transactional(readOnly = true)
public class ContentPageDAOImpl extends GenericDaoImpl implements ContentPageDAO {

	@Override
	public List<ContentPage> getContentPagesInPage(Integer pageId) {
		if (pageId == null || pageId < 0) {
			return Collections.emptyList();
		}

		return getResultList(ContentPage.GET_BY_PAGE, ContentPage.class, new Param(ContentPage.PAGE_PROP, pageId));
	}

	@Override
	public ContentPage getContentPage(Long id) {
		if (id == null) {
			return null;
		}

		return getSingleResult(ContentPage.GET_BY_ID, ContentPage.class, new Param(ContentPage.ID_PROP, id));
	}

	@Override
	public Integer getLastPositionForContentPage(int pageId) {
		Integer lastPosition = getSingleResult(ContentPage.GET_LAST_POSITION, Integer.class, new Param(ContentPage.PAGE_PROP, pageId));
		return lastPosition == null ? -1 : lastPosition;
	}

	@Override
	@Transactional
	public void removeContentPage(Long id) {
		ContentPage contentPage = getContentPage(id);
		if (contentPage == null) {
			return;
		}

		remove(contentPage);
	}

	@Override
	@Transactional
	public ContentPage storeContentPage(Integer pageId, Long id, String headline, String body) {
		ContentPage page = getContentPage(id);
		boolean newPage = false;
		if (page == null) {
			page = new ContentPage();
			page.setPage(pageId);
			page.setPosition(getLastPositionForContentPage(pageId) + 1);
			newPage = true;
		}
		page.setHeadline(headline);
		page.setBody(body);
		page.setIsUri(false);

		if (newPage) {
			persist(page);
		} else {
			merge(page);
		}

		return page.getId() == null ? null : page;
	}

}