package com.idega.content.themes.helpers;

import java.util.List;
import java.util.Map;

public class ThemeStyleVariations {
	
	public String getThemeStyleVariations(String themeID) {
		StringBuffer buffer = new StringBuffer();
		if (themeID == null) {
			return buffer.toString();
		}
		ThemeInfo theme = ThemesHelper.getInstance().getThemeInfo(themeID);
		buffer.append("<p>Theme variations for ");
		buffer.append(theme.getName());
		buffer.append(":</p>");
		buffer.append("<ul style='list-style-type: none;'>");
		
		String styleGroupName = null;
		List <String> styleGroups = theme.getStyleGroupsNames();
		for (int i = 0; i < styleGroups.size(); i++) {
			styleGroupName = styleGroups.get(i);
			buffer.append("<li>");
			buffer.append(styleGroupName);
			buffer.append(getStyleGroupMembers(theme, styleGroupName));
		}
		
		buffer.append("</ul>");
		return buffer.toString();
	}
	
	private String getStyleGroupMembers(ThemeInfo theme, String styleGroupName) {
		StringBuffer result = new StringBuffer();
		result.append("<ul style='list-style-type: none;'>");
		
		Map <String, ThemeStyleGroupMember> themeVariations = theme.getStyleGroupsMembers();
		ThemeStyleGroupMember member = null;
		int i = 0;
		while (themeVariations.get(styleGroupName + ThemesConstants.AT + i) != null) {
			member = themeVariations.get(styleGroupName + ThemesConstants.AT + i);
			result.append("<li>");
			result.append("<input type='radio' name='");
			result.append(styleGroupName);
			result.append("' value='");
			result.append(member.getName());
			result.append("' onclick=\"changeTheme('");
			result.append(theme.getThemeId());
			result.append("', '");
			result.append(styleGroupName);
			result.append("', '");
			result.append(member.getName());
			result.append("');\"");
			if (member.isEnabled()) {
				result.append("checked='true'");
			}
			result.append("/>");
			result.append(member.getName());
			i++;
		}
		
		result.append("</ul>");
		return result.toString();
	}

}