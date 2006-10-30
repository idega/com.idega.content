package com.idega.content.themes.business;

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.idega.business.IBOServiceBean;
import com.idega.content.themes.helpers.Theme;
import com.idega.content.themes.helpers.ThemesConstants;
import com.idega.content.themes.helpers.ThemesHelper;

public class ThemesPreviewsProviderBean extends IBOServiceBean implements ThemesPreviewsProvider {

	private static final long serialVersionUID = 5875353284352953688L;
	
	private static final Log log = LogFactory.getLog(ThemesPreviewsProviderBean.class);
	
	private ThemesHelper helper = ThemesHelper.getInstance();
	
	private boolean extractingProperties;

	/**
	 * Returns info about themes in slide
	 */
	public String getThemesPreviewsInfo() {
		if (!extractingProperties) {
			extractingProperties = true;
			if (!helper.getThemesPropertiesExtractor().proceedFileExtractor()) {
				log.info("Error extracting theme's properties");
			}
			extractingProperties = false;
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
					info.append(ThemesConstants.SEMICOLON);
				}
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