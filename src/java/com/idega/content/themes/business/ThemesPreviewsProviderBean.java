package com.idega.content.themes.business;

import java.util.Iterator;

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
		
		Iterator <Theme> it = helper.getThemesCollection().iterator();
		StringBuffer info = new StringBuffer();
		
		Theme theme = null;
		String webRoot = helper.getFullWebRoot();
		while (it.hasNext()) {
			theme = it.next();
			
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
			if (it.hasNext()) {
				info.append(ThemesConstants.SEMICOLON);
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