package com.idega.content.themes.business;

import java.rmi.RemoteException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.idega.business.IBOServiceBean;
import com.idega.content.business.ContentUtil;
import com.idega.content.themes.helpers.Setting;
import com.idega.content.themes.helpers.Theme;
import com.idega.content.themes.helpers.ThemesConstants;
import com.idega.content.themes.helpers.ThemesHelper;
import com.idega.core.builder.data.ICPage;
import com.idega.core.data.ICTreeNode;
import com.idega.idegaweb.IWMainApplication;
import com.idega.idegaweb.IWMainApplicationSettings;
import com.idega.presentation.IWContext;

public class ThemesEngineBean extends IBOServiceBean implements ThemesEngine {

	private static final long serialVersionUID = 5875353284352953688L;
	private static final Log log = LogFactory.getLog(ThemesEngineBean.class);
	
	private static final String PAGE_URI = "pageUri";
	private static final String PAGE_TITLE = "pageTitle";
	private static final String MINUS_ONE = "-1";
	private static final String PATH_TO_IMAGE_FOLDER = "/idegaweb/bundles/com.idega.content.bundle/resources/images/pageIcons/";
	
	private ThemesHelper helper = ThemesHelper.getInstance();

	/**
	 * Returns info about themes in slide
	 */
	public String getThemesPreviewsInfo() {		
		helper.searchForThemes(); // It is done in ThemesHelper's constructor, but it's possible to pass a paremeter to not search
		
		if (!helper.getThemesPropertiesExtractor().proceedFileExtractor()) {
			log.info("Error extracting theme's properties");
		}
		
		List <Theme> themes = new ArrayList<Theme>(helper.getThemesCollection());
		StringBuffer info = new StringBuffer();
		
		Theme theme = null;
		String webRoot = helper.getFullWebRoot();
		for (int i = 0; i < themes.size(); i++) {
			theme = themes.get(i);			
			if (theme.isPropertiesExtracted()) {
				if (theme.getChangedName() != null) {
					info.append(theme.getChangedName());
				}
				else {
					info.append(theme.getName());
				}
				info.append(ThemesConstants.AT);
				info.append(webRoot);
				info.append(theme.getLinkToBase());
				info.append(helper.encode(theme.getLinkToSmallPreview(), true));
				info.append(ThemesConstants.AT);
				info.append(webRoot);
				info.append(theme.getLinkToBase());
				if (theme.getLinkToDraftPreview() != null) {
					info.append(helper.encode(theme.getLinkToDraftPreview(), true));
				}
				else {
					info.append(helper.encode(theme.getLinkToThemePreview(), true));
				}
				info.append(ThemesConstants.AT);
				info.append(theme.getId());
				if (i + 1 < themes.size()) {
					info.append(ThemesConstants.SEMICOLON);
				}
			}
		}
		return info.toString();
	}
	
	/**
	 * 
	 */
	public String getThemeStyleVariations(String themeID) {
		return helper.getThemeStyleVariations().getThemeStyleVariations(themeID);
	}
	
	/**
	 * 
	 */
	public String changeTheme(String themeID, String styleGroupName, String styleMember, String themeName, boolean isRadio, boolean isChecked) {
		return helper.getThemeChanger().changeTheme(themeID, styleGroupName, styleMember, themeName, isRadio, isChecked);
	}
	
	/**
	 * 
	 */
	public boolean saveTheme(String themeID, String themeName) {
		return helper.getThemeChanger().saveTheme(themeID, themeName);
	}
	
	/**
	 * 
	 */
	public boolean setSelectedStyle(String themeID, String pageID, boolean applyToPage) {
		IWContext iwc = IWContext.getInstance();
		if (iwc == null) {
			return false;
		}
		
		if (themeID == null) {
			return false;
		}
		Theme theme = helper.getTheme(themeID);
		if (theme == null) {
			return false;
		}
		boolean result = true;
		
		helper.setLastUsedTheme(theme.getIBPageID());
		
		if (applyToPage) { // Apply style to selected page
			result = setPageStyle(pageID, theme.getIBPageID(), iwc);
		}
		else { // Apply style to all pages
			result = setSiteStyle(theme.getIBPageID(), iwc);
		}
		helper.getThemesService().getBuilderService().clearAllCachedPages();
		return result;
	}
	
	private boolean setPageStyle(String pageID, int templateID, IWContext iwc) {
		ICPage page = null;
		page = helper.getThemesService().getICPage(Integer.valueOf(pageID).intValue());
		if (page == null) {
			return false;
		}
		if (page.isPage()) {
			helper.getThemesService().getBuilderService().setTemplateId(pageID, String.valueOf(templateID));
			page.setTemplateId(templateID);
			page.store();
		}
		return true;
	}
	
	private boolean setSiteStyle(int templateID, IWContext iwc) {
		Map tree = helper.getThemesService().getBuilderService().getTree(iwc);
		if (tree == null) {
			return false;
		}
		Iterator it = tree.values().iterator();
		Object o = null;
		boolean result = true;
		while (it.hasNext()) {
			o = it.next();
			if (o instanceof ICTreeNode) {
				result = setPageStyle(((ICTreeNode) o).getId(), templateID, iwc);
			}
		}
		return result;
	}
	
	private boolean setPageTitle(String pageID, String title) {
		if (pageID == null || title == null) {
			return false;
		}
		if (MINUS_ONE.equals(pageID)) {
			return false;
		}
		
		IWContext iwc = IWContext.getInstance();
		if (iwc == null) {
			return false;
		}
		
		IWMainApplication appl = iwc.getIWMainApplication();
		if (appl == null) {
			return false;
		}
		
		String method = ":method:1:implied:void:setTitle:java.lang.String:";
		return helper.getThemesService().getBuilderService().setProperty(pageID, MINUS_ONE, method, new String[]{title}, appl);
	}
	
	public String changePageUri(String pageID, String pageTitle, boolean needSetPageTitle) {
		if (pageID == null || pageTitle == null) {
			return null;
		}
		if (MINUS_ONE.equals(pageID)) {
			return null;
		}
		
		ICPage page = helper.getThemesService().getICPage(Integer.valueOf(pageID).intValue());
		if (pageTitle.equals(page.getDefaultPageURI())) {
			return null;
		}
		
		ICTreeNode parentNode = page.getParentNode();
		String parentId = null;
		if (parentNode != null) {
			parentId = parentNode.getId();
		}
		
		if (needSetPageTitle) {
			setPageTitle(pageID, pageTitle);
		}
		
		if (helper.getThemesService().getBuilderService().changePageUriByTitle(parentId, page, pageTitle, -1)) {
			return page.getDefaultPageURI();
		}
		return null;
	}
	
	/**
	 * 
	 */
	public String savePageInfo(String pageID, String[] keywords, String[] values) {
		if (pageID == null || keywords == null || values == null) {
			return null;
		}
		if (keywords.length != values.length) {
			return null;
		}
		IWContext iwc = IWContext.getInstance();
		if (iwc == null) {
			return null;
		}
		IWMainApplication appl = iwc.getIWMainApplication();
		if (appl == null) {
			return null;
		}
		String changedPageUri = null;
		Setting s = null;
		Map <String, Setting> map = helper.getPageSettings();
		String[] currentValues = null;
		String[] newValues = null;
		boolean changedPageTitle = false;
		boolean needSetValue = true;
		for (int i = 0; i < keywords.length; i++) {
			s = map.get(keywords[i]);
			if (s != null) {
				currentValues = helper.getThemesService().getBuilderService().getPropertyValues(appl, pageID, MINUS_ONE, s.getMethod(), null, true);
				if (ThemesConstants.EMPTY.equals(values[i]) || values[i] == null) {
					if (currentValues != null) {
						helper.getThemesService().getBuilderService().removeProperty(appl, pageID, MINUS_ONE, s.getMethod(), currentValues);
					}
				}
				else {
					if (s.getCode().equals(PAGE_URI)) {
						if (!changedPageTitle) {
							changedPageUri = changePageUri(pageID, values[i], false);
						}
					}
					else {
						newValues = helper.getPageValues(s, values[i]);
						if (newValues == null) {
							needSetValue = false;
						}
						if (Arrays.deepEquals(newValues, currentValues)) {
							needSetValue = false;
						}
						if (needSetValue) {
							helper.getThemesService().getBuilderService().setProperty(pageID, MINUS_ONE, s.getMethod(), newValues, appl);
							if (s.getCode().equals(PAGE_TITLE)) {
								changedPageUri = changePageUri(pageID, values[i], false);
								helper.getThemesService().getBuilderService().changePageName(Integer.valueOf(pageID).intValue(), values[i]);
								changedPageTitle = true;
							}
						}
					}
				}
			}
		}
		return changedPageUri;
	}
	
	public String[] getPageInfoValues(String pageID, String[] keywords) {
		if (pageID == null || keywords == null) {
			return null;
		}
		if (MINUS_ONE.equals(pageID)) {
			return null;
		}
		IWContext iwc = IWContext.getInstance();
		if (iwc == null) {
			return null;
		}
		IWMainApplication appl = iwc.getIWMainApplication();
		if (appl == null) {
			return null;
		}
		Setting s = null;
		Map <String, Setting> map = helper.getPageSettings();
		String[] values = new String[keywords.length];
		String[] propValues = null;
		StringBuffer value = null;
		for (int i = 0; i < keywords.length; i++) {
			s = map.get(keywords[i]);
			value = new StringBuffer();
			if (s != null) {
				propValues = helper.getThemesService().getBuilderService().getPropertyValues(appl, pageID, MINUS_ONE, s.getMethod(), null, true);
				if (propValues != null) {
					for (int j = 0; j < propValues.length; j++) {
						if (!s.getDefaultValue().equals(propValues[j])) {
							value.append(propValues[j]);
							if (j + 1 < propValues.length) {
								value.append(ThemesConstants.COMMA);
							}
						}
					}
				}
				else {
					if (s.getCode().equals(PAGE_URI)) {
						ICPage page = helper.getThemesService().getICPage(Integer.valueOf(pageID).intValue());
						if (page != null) {
							value.append(page.getDefaultPageURI());
						}
					}
				}
			}
			values[i] = value.toString();
		}
		return values;
	}
	
	/**
	 * 
	 */
	public String[] getPageInfoElements() {
		Collection <Setting> c = helper.getPageSettings().values();
		if (c == null) {
			return null;
		}
		return getElements(c);
	}
	
	/**
	 * 
	 */
	public boolean restoreTheme(String themeID) {
		return helper.getThemeChanger().restoreTheme(themeID);
	}
	
	public String[] getSiteInfoElements() {
		Collection <Setting> c = helper.getThemeSettings().values();
		if (c == null) {
			return null;
		}
		return getElements(c);
	}
	
	public String[] getSiteInfoValues(String[] keywords, String language) {
		if (keywords == null || language == null) {
			return null;
		}
		Collection <Setting> c = helper.getThemeSettings().values();
		if (c == null) {
			return null;
		}
		String[] values = new String[keywords.length];
		IWMainApplicationSettings settings  = ContentUtil.getBundle().getApplication().getSettings();
		for (int i = 0; i < keywords.length; i++) {
			values[i] = settings.getProperty(ThemesConstants.THEMES_PROPERTY_START + keywords[i] + ThemesConstants.DOT + language);
		}
		return values;
	}
	
	private String[] getElements(Collection <Setting> c) {
		String[] elements = null;
		List <Setting> settings = new ArrayList<Setting>(c);
		elements = new String[settings.size()];
		for (int i = 0; i < settings.size(); i++) {
			elements[i] = settings.get(i).getCode();
		}
		return elements;
	}
	
	public boolean saveSiteInfo(String language, String[] keywords, String[] values) {
		if (language == null || keywords == null || values == null) {
			return false;
		}
		if (keywords.length != values.length) {
			return false;
		}
		IWMainApplicationSettings settings  = ContentUtil.getBundle().getApplication().getSettings();
		for (int i = 0; i < keywords.length; i++) {
			if (values[i] == null || values[i].equals(ThemesConstants.EMPTY)) {
				settings.removeProperty(ThemesConstants.THEMES_PROPERTY_START + keywords[i] + ThemesConstants.DOT + language);
			}
			else {
				settings.setProperty(ThemesConstants.THEMES_PROPERTY_START + keywords[i] + ThemesConstants.DOT + language, values[i]);
			}
		}
		return true;
	}
	
	public List <String> beforeCreatePage(List <String> struct){
		List <String> newIds = new ArrayList<String>();
		int id = -1;
		String prevId = null;
		String pageType = helper.getThemesService().getBuilderService().getPageKey();
		String format = helper.getThemesService().getBuilderService().getIBXMLFormat();
		boolean canCreate;
		String realID = null;
		String webDAVUri = null;
		String subType = null;
		for (int i = 0; i < struct.size(); i = i+5) {
			canCreate = true;
			prevId = struct.get(i);			
			try {
				subType = struct.get(i+3);
				webDAVUri = struct.get(i+4);
				id = createPage(struct.get(i+1), struct.get(i+2), pageType, null, null, subType, -1, format, null);
				realID = String.valueOf(id);
			} catch (IndexOutOfBoundsException e) {
				log.error(e);
				canCreate = false;
			}
			if (canCreate) {
				if (webDAVUri != null) {
					if (!webDAVUri.equals(ThemesConstants.EMPTY)) {
						String uriToPage = helper.loadPageToSlide(subType, id, webDAVUri);
						helper.createArticle(subType, id);
						if (uriToPage != null) {
							helper.getThemesService().updatePageWebDav(id, uriToPage);
						}
						String lastTheme = helper.getLastUsedTheme();
						if (lastTheme != null) {
							helper.getThemesService().getBuilderService().setTemplateId(realID, lastTheme);
						}
					}
				}
				
				for (int j = i; j < struct.size(); j = j+5) {
					try {
						if (struct.get(j + 1) != null) {
							if ((struct.get(j + 1)).equals(prevId)) {
								struct.set(j + 1, realID);
							}
						}
					} catch (IndexOutOfBoundsException e) {
						log.error(e);
					}
				}
				
				newIds.add(realID);
			}
		}

		return newIds;
	}
	
	public int createPage(String parentId, String name, String type, String templateId, String pageUri, String subType, int domainId, String format, String sourceMarkup) {
		int id = -1;
		if (pageUri != null) {
			if (pageUri.equals(ThemesConstants.EMPTY)) {
				pageUri = null;
			}
		}
		try {
			id = helper.getThemesService().createIBPage(parentId, name, type, templateId, pageUri, subType, domainId, format, sourceMarkup);
		} catch (RemoteException e) {
			log.error(e);
			return -1;
		}
		return id;
	}
	
	public boolean deletePage(String pageId, boolean deleteChildren) {
		if (pageId == null) {
			return false;
		}
		try {
			helper.getThemesService().deleteIBPage(pageId, deleteChildren);
		} catch (RemoteException e) {
			log.error(e);
			return false;
		}
		return true;
	}
	
	public String getPageId() {
		String id = null;
		id = helper.getLastVisitedPage();
		if (id != null) {
			return id;
		}
		
		id = String.valueOf(getRootPageId());
		helper.setLastVisitedPage(id);
		return id;
	}
	
	public boolean setPageId(String id) {
		if (id == null) {
			return false;
		}
		helper.setLastVisitedPage(id);
		return true;
	}
	
	public boolean movePage(int newParentId, int nodeId) {
		if (newParentId == 0 || newParentId == -1) {
			IWContext iwc = IWContext.getInstance();
			if (iwc == null) {
				return false;
			}
			return helper.getThemesService().getBuilderService().movePageToTopLevel(nodeId, iwc);
		}
		return helper.getThemesService().getBuilderService().movePage(newParentId, nodeId);
	}
	
	public String getPathToImageFolder(){
		return PATH_TO_IMAGE_FOLDER;
	}
	
	public boolean isStartPage(String pageID) {
		if (pageID == null) {
			return true; // Returning true to disable a button
		}
		int id = -1;
		try {
			id = Integer.valueOf(pageID).intValue();
		} catch (NumberFormatException e) {
			log.error(e);
			return true; // Returning true to disable a button
		}
		if (id <= 0) {
			return true; // Returning true to disable a button
		}
		if (id == getRootPageId()) {
			return true;
		}
		return false;
	}
	
	public boolean setAsStartPage(String pageID) {
		if (pageID == null) {
			return false;
		}
		int newRoot = -1;
		try {
			newRoot = Integer.valueOf(pageID).intValue();
		} catch (NumberFormatException e) {
			log.error(e);
			return false;
		}
		if (newRoot <= 0) {
			return false;
		}
		
		int currentRoot = getRootPageId();
		if (currentRoot == newRoot) {
			return true;
		}
		return true;
	}
	
	private int getRootPageId() {
		int id = 1;
		try {
			id = helper.getThemesService().getBuilderService().getRootPageId();
		} catch (Exception e) {
			log.error(e);
			return -1;
		}
		return id;
	}

}