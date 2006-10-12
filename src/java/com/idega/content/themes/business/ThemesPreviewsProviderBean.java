package com.idega.content.themes.business;

import java.util.ArrayList;
import java.util.List;

import com.idega.business.IBOServiceBean;
import com.idega.content.business.ContentUtil;
import com.idega.content.themes.helpers.ThemeInfo;
import com.idega.content.themes.helpers.ThemesConstants;
import com.idega.content.themes.helpers.ThemesHelper;

public class ThemesPreviewsProviderBean extends IBOServiceBean implements ThemesPreviewsProvider {

	private static final long serialVersionUID = 5875353284352953688L;

	private static final String THEME_NAME = ContentUtil.getBundle().getLocalizableStringDefaultValue("theme_name") + ": ";	
	
	private ThemesHelper helper = ThemesHelper.getInstance();

	/**
	 * Returns info about themes previews in slide
	 */
	public String getThemesPreviewsInfo() {
		helper.searchForThemes();
		
		helper.getThemesPropertiesExtractor().proceedFileExtractor();
		
		List <ThemeInfo> themes = new ArrayList <ThemeInfo> (helper.getThemesCollection());
		StringBuffer info = new StringBuffer();
		
		ThemeInfo theme = null;
		String webRoot = helper.getFullWebRoot();
		for (int i = 0; i < themes.size(); i++) {
			theme = themes.get(i);
			
			info.append(THEME_NAME);
			info.append(theme.getName());
			info.append(ThemesConstants.AT);
			info.append(webRoot + theme.getLinkToBase() + theme.getLinkToPreview());
			info.append(ThemesConstants.AT);
			info.append(theme.getThemeId());
			if (i + 1 < themes.size()) {
				info.append(";");
			}
		}
		return info.toString();
	}
	
	public String getThemeStyleVariations(String themeID) {
		return helper.getThemeStyleVariations().getThemeStyleVariations(themeID);
	}
	
	public String changeTheme(String themeID, String styleGroupName, String newStyleMember) {
		return helper.getThemeChanger().changeTheme(themeID, styleGroupName, newStyleMember);
	}
	
	public boolean saveTheme(String themeID) {
		return helper.getThemeChanger().saveTheme(themeID);
	}

}