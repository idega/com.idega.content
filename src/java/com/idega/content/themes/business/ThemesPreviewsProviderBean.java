package com.idega.content.themes.business;

import java.util.ArrayList;
import java.util.List;

import com.idega.business.IBOServiceBean;
import com.idega.content.themes.helpers.Theme;
import com.idega.content.themes.helpers.ThemesConstants;
import com.idega.content.themes.helpers.ThemesHelper;

public class ThemesPreviewsProviderBean extends IBOServiceBean implements ThemesPreviewsProvider {

	private static final long serialVersionUID = 5875353284352953688L;
	
	private ThemesHelper helper = ThemesHelper.getInstance();

	/**
	 * Returns info about themes previews in slide
	 */
	public String getThemesPreviewsInfo() {
		helper.getThemesPropertiesExtractor().proceedFileExtractor();
		
		List <Theme> themes = new ArrayList <Theme> (helper.getThemesCollection());
		StringBuffer info = new StringBuffer();
		
		Theme theme = null;
		String webRoot = helper.getFullWebRoot();
		for (int i = 0; i < themes.size(); i++) {
			theme = themes.get(i);
			
			if (theme.getChangedName() != null) {
				info.append(theme.getChangedName());
			}
			else {
				info.append(theme.getName());
			}
			info.append(ThemesConstants.AT);
			info.append(webRoot + theme.getLinkToBase());
			if (theme.getLinkToDraftPreview() != null) {
				info.append(helper.encode(theme.getLinkToDraftPreview(), true));
			}
			else {
				info.append(helper.encode(theme.getLinkToThemePreview(), true));
			}
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
	
	public String changeTheme(String themeID, String styleGroupName, String styleMember, String themeName, boolean radio, boolean checked) {
		return helper.getThemeChanger().changeTheme(themeID, styleGroupName, styleMember, themeName, radio, checked);
	}
	
	public boolean saveTheme(String themeID, String themeName) {
		return helper.getThemeChanger().saveTheme(themeID, themeName);
	}

}