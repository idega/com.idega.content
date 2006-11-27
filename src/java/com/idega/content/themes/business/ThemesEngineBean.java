package com.idega.content.themes.business;

import java.rmi.RemoteException;
import java.util.ArrayList;
import java.util.Collection;
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
import com.idega.idegaweb.IWMainApplicationSettings;
import com.idega.presentation.IWContext;

public class ThemesEngineBean extends IBOServiceBean implements ThemesEngine {

	private static final long serialVersionUID = 5875353284352953688L;
	
	private static final Log log = LogFactory.getLog(ThemesEngineBean.class);
	
	private ThemesHelper helper = ThemesHelper.getInstance();
	
	//private boolean extractingProperties;

	/**
	 * Returns info about themes in slide
	 */
	public String getThemesPreviewsInfo() {
		helper.searchForThemes(); // It is done in ThemesHelper's constructor, but it's possible to pass a paremeter to not search
		
//		if (!extractingProperties) {
//			extractingProperties = true;
			if (!helper.getThemesPropertiesExtractor().proceedFileExtractor()) {
				log.info("Error extracting theme's properties");
			}
//			extractingProperties = false;
//		}
		
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
	public String setSelectedStyle(String themeID, boolean applyToPage) {
		if (applyToPage) {
			return "page";
		}
		else {
			return "site";
		}
	}
	
	/**
	 * 
	 */
	public boolean savePageInfo(String pageID, String[] keywords, String[] values) {
		if (pageID == null || keywords == null || values == null) {
			return false;
		}
		if (keywords.length != values.length) {
			return false;
		}
		String instanceID = "-1";
		Setting s = null;
		Map <String, Setting> map = helper.getPageSettings();
		for (int i = 0; i < keywords.length; i++) {
			s = map.get(keywords[i]);
			if (s != null) {
				helper.getThemesService().getBuilderService().setProperty(pageID, instanceID, s.getMethod(), helper.getPageValues(s, values[i]), IWContext.getInstance().getIWMainApplication());
			}
		}
		return true;
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
			values[i] = settings.getProperty(ThemesConstants.THEMES_PROPERTY_START + keywords[i] + ThemesConstants.UNDER + language);
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
				settings.removeProperty(ThemesConstants.THEMES_PROPERTY_START + keywords[i] + ThemesConstants.UNDER + language);
			}
			else {
				settings.setProperty(ThemesConstants.THEMES_PROPERTY_START + keywords[i] + ThemesConstants.UNDER + language, values[i]);
			}
		}
		return true;
	}
	
	public int createPage(String parentId, String name, String type, String templateId, String pageUri, String subType, int domainId, String format, String sourceMarkup) {
		int id = -1;
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

}