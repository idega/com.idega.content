package com.idega.content.themes.helpers;

import java.util.List;
import java.util.Map;

public class ThemeStyleVariations {

	private static final String TAG_LI_OPEN = "<li>";
	
	private static final String INPUT_TYPE = "<input type='";
	private static final String INPUT_NAME = "' name='";
	private static final String INPUT_VALUE = "' value='";
	private static final String INPUT_ONCLICK = "' onclick=\"changeTheme('";
	private static final String INPUT_CHECKED = "checked='true'";
	
	private static final String PARAM_CHECKED = "', this.checked";
	
	private static final String SEPERATOR = "', '";
	private static final String CLOSING_ONCLICK = ");\"";
	private static final String CLOSING_TAG = "/>";
	
	private static final String RADIO_INPUT = "radio";
	private static final String CHECKBOX_INPUT = "checkbox";
	
	public String getThemeStyleVariations(String themeID) {
		StringBuffer buffer = new StringBuffer();
		if (themeID == null) {
			return buffer.toString();
		}
		Theme theme = ThemesHelper.getInstance().getTheme(themeID);
		buffer.append("<p>Theme variations for ");
		buffer.append(theme.getName());
		buffer.append(":</p>");
		buffer.append("<ul style='list-style-type: none;'>");
		
		String styleGroupName = null;
		List <String> styleGroups = theme.getStyleGroupsNames();
		for (int i = 0; i < styleGroups.size(); i++) {
			styleGroupName = styleGroups.get(i);
			buffer.append(TAG_LI_OPEN);
			buffer.append(styleGroupName);
			buffer.append(getStyleGroupMembers(theme, styleGroupName));
		}
		
		buffer.append("</ul>");
		return buffer.toString();
	}
	
	private String getStyleGroupMembers(Theme theme, String styleGroupName) {
		StringBuffer result = new StringBuffer();
		result.append("<ul style='list-style-type: none;'>");
		
		Map <String, ThemeStyleGroupMember> themeVariations = theme.getStyleGroupsMembers();
		ThemeStyleGroupMember member = null;
		int i = 0;
		String type = null;
		while (themeVariations.get(styleGroupName + ThemesConstants.AT + i) != null) {
			member = themeVariations.get(styleGroupName + ThemesConstants.AT + i);
			result.append(TAG_LI_OPEN);
			result.append(INPUT_TYPE);
			if (member.isLimitedSelection()) {
				result.append(RADIO_INPUT);
				type = RADIO_INPUT;
			}
			else {
				result.append(CHECKBOX_INPUT);
				type = CHECKBOX_INPUT;
			}
			result.append(INPUT_NAME);
			result.append(styleGroupName);
			result.append(INPUT_VALUE);
			result.append(member.getName());
			result.append(INPUT_ONCLICK);
			result.append(theme.getId());
			result.append(SEPERATOR);
			result.append(styleGroupName);
			result.append(SEPERATOR);
			result.append(member.getName());
			result.append(SEPERATOR);
			result.append(type);
			result.append(PARAM_CHECKED);
			result.append(CLOSING_ONCLICK);
			if (member.isEnabled()) {
				result.append(INPUT_CHECKED);
			}
			result.append(CLOSING_TAG);
			result.append(member.getName());
			i++;
		}
		
		result.append("</ul>");
		return result.toString();
	}

}