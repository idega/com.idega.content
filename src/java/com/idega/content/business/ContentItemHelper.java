package com.idega.content.business;

import java.rmi.RemoteException;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import com.idega.content.themes.business.ThemesService;
import com.idega.content.themes.helpers.ThemesHelper;
import com.idega.core.builder.business.BuilderService;
import com.idega.core.builder.business.BuilderServiceFactory;
import com.idega.core.builder.data.ICPage;
import com.idega.core.data.ICTreeNode;
import com.idega.presentation.IWContext;
import com.idega.util.CoreConstants;

public class ContentItemHelper {
	
	private boolean isUsedDefaultArticlePath = false;
	private boolean canEnableOrDisableComments = true;
	private String contentItemResourcePath = null;
	
	public ContentItemHelper(String contentItemResourcePath) {
		this.contentItemResourcePath = contentItemResourcePath;
	}
	
	public String getPageUrlByArticleResourcePath(IWContext iwc, String moduleClass) {
		if (iwc == null) {
			return getDefaultPageUrlByArticleResourcePath();
		}
		BuilderService builder = null;
		try {
			builder = BuilderServiceFactory.getBuilderService(iwc.getApplicationContext());
		} catch (RemoteException e) {
			e.printStackTrace();
		}
		if (builder == null) {
			return getDefaultPageUrlByArticleResourcePath();
		}
		Map tree = builder.getTree(iwc);
		if (tree == null) {
			return getDefaultPageUrlByArticleResourcePath();
		}
		Object o = null;
		String pageID = null;
		List<String> moduleIds = null;
		String propertyName = "resourcePath";
		String propertyValue = getPathAppliedForSearch();
		if (propertyValue == null) {
			return getDefaultPageUrlByArticleResourcePath();
		}
		ThemesService themesService = ThemesHelper.getInstance().getThemesService();
		if (themesService == null) {
			return getDefaultPageUrlByArticleResourcePath();
		}
		ICPage page = null;
		for (Iterator it = tree.values().iterator(); it.hasNext();) {
			o = it.next();
			if (o instanceof ICTreeNode) {
				pageID = ((ICTreeNode) o).getId();
				page = themesService.getICPage(Integer.valueOf(pageID));
				if (page != null) {
					if (page.isPage() && !page.getDeleted()) {
						moduleIds = builder.getModuleId(pageID, moduleClass);
						if (moduleIds == null) {
							return getDefaultPageUrlByArticleResourcePath();
						}
						for (int i = 0; i < moduleIds.size(); i++) {
							if (builder.isPropertyValueSet(pageID, moduleIds.get(i), propertyName, propertyValue)) {
								return ContentConstants.PAGES_START_URI + page.getDefaultPageURI();
							}
						}
					}
				}
			}
		}
		return getDefaultPageUrlByArticleResourcePath();
	}
	
	public boolean isCommentsEnabledInMainPage(IWContext iwc, String articleModuleClass, String commentsModuleClass) {
		if (iwc == null || articleModuleClass == null || commentsModuleClass == null) {
			return false;
		}
		BuilderService builder = null;
		try {
			builder = BuilderServiceFactory.getBuilderService(iwc.getApplicationContext());
		} catch (RemoteException e) {
			e.printStackTrace();
			return false;
		}
		if (builder == null) {
			return false;
		}
		String url = getPageUrlByArticleResourcePath(iwc, articleModuleClass);
		if (isUsedDefaultArticlePath()) {
			canEnableOrDisableComments = false;
			return false;
		}
		String pageKey = builder.getPageKeyByURI(url);
		if (pageKey == null) {
			return false;
		}
		
		List<String> moduleIds = builder.getModuleId(pageKey, commentsModuleClass);
		if (moduleIds == null) {
			return false;
		}
		String propertyName = "showCommentsForAllUsers";
		String propertyValue = "true";
		for (int i = 0; i < moduleIds.size(); i++) {
			boolean result = builder.isPropertyValueSet(pageKey, moduleIds.get(i), propertyName, propertyValue);
			System.out.println("founded result: " + result);
			return result;
		}
		return false;
	}
	
	private String getDefaultPageUrlByArticleResourcePath () {
		isUsedDefaultArticlePath = true;
		String realPath = contentItemResourcePath;
		if (realPath == null) {
			return ContentConstants.PAGES_START_URI;
		}
		realPath = realPath.substring(0, realPath.lastIndexOf(ContentConstants.SLASH));
		StringBuffer defaultPath = new StringBuffer("/idegaweb/action/preview/article");
		defaultPath.append(realPath);
		return defaultPath.toString();
	}
	
	private String getPathAppliedForSearch() {
		String realPath = contentItemResourcePath;
		if (realPath == null) {
			return null;
		}
		String[] pathParts = realPath.split(ContentConstants.SLASH);
		if (pathParts == null) {
			return contentItemResourcePath;
		}
		if (pathParts.length == 0) {
			return contentItemResourcePath;
		}
		StringBuffer appliedPath = new StringBuffer();
		for (int i = 0; i + 1 < pathParts.length; i++) {
			if (CoreConstants.CONTENT.indexOf(pathParts[i]) == -1) {
				appliedPath.append(ContentConstants.SLASH).append(pathParts[i]);
			}
		}
		return appliedPath.toString();
	}

	public boolean isUsedDefaultArticlePath() {
		return isUsedDefaultArticlePath;
	}

	public boolean isCanEnableOrDisableComments() {
		return canEnableOrDisableComments;
	}

}
