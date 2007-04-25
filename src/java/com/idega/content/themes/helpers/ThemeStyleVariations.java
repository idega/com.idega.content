package com.idega.content.themes.helpers;

import java.util.List;
import java.util.Map;

public class ThemeStyleVariations {

	private static final String TAG_LI_OPEN = "<li>";
	
	private static final String INPUT_TYPE = "<input type='";
	private static final String INPUT_NAME = "' name='";
	private static final String INPUT_VALUE = "' value='";
	private static final String INPUT_ONCLICK = "' onclick=\"addThemeChange('";
	private static final String INPUT_CHECKED = "checked='true'";
	
	private static final String PARAM_CHECKED = "', this.checked";
	
	private static final String SEPERATOR = "', '";
	private static final String CLOSING_ONCLICK = ");\"";
	private static final String CLOSING_TAG = "/>";
	
	private static final String RADIO_INPUT = "radio";
	private static final String CHECKBOX_INPUT = "checkbox";
	
	private static final String DIV_OPENER = "<div class=\"";
	private static final String DIV_CLOSER = "</div>";
	private static final String VARIATION_GROUP_STYLE = "themeVariationGroup\">";
	private static final String VARIATION_GROUP_NAME_STYLE = "themeVariationGroupName\">";
	
	private ThemesHelper helper = ThemesHelper.getInstance();
	
	public String getThemeStyleVariations(String themeID) {
		StringBuffer buffer = new StringBuffer();
		if (themeID == null) {
			return buffer.toString();
		}
		Theme theme =helper.getTheme(themeID);
		if (theme == null) {
			return buffer.toString();
		}
		buffer.append(DIV_OPENER).append("allThemeVariations\">");
		buffer.append("<ul>");
		
		String styleGroupName = null;
		List <String> styleGroups = theme.getStyleGroupsNames();
		for (int i = 0; i < styleGroups.size(); i++) {
			styleGroupName = styleGroups.get(i);
			buffer.append(DIV_OPENER).append(VARIATION_GROUP_STYLE);
			buffer.append(TAG_LI_OPEN);
			buffer.append(DIV_OPENER).append(VARIATION_GROUP_NAME_STYLE);
			buffer.append(styleGroupName);
			buffer.append(DIV_CLOSER);
			buffer.append(getStyleGroupMembers(theme, styleGroupName));
			buffer.append(DIV_CLOSER);
		}
		
		buffer.append("</ul>");
		buffer.append(DIV_CLOSER);
		return buffer.toString();
	}
	
	private String getStyleGroupMembers(Theme theme, String styleGroupName) {
		StringBuffer result = new StringBuffer();
		result.append("<ul>");
		
		Map <String, ThemeStyleGroupMember> themeVariations = theme.getStyleGroupsMembers();
		ThemeStyleGroupMember member = null;
//		int i = 0;
		String type = null;
		for (int i = 0; themeVariations.get(styleGroupName + ThemesConstants.AT + i) != null; i++) {
			member = themeVariations.get(styleGroupName + ThemesConstants.AT + i);
			if (availableStyleMember(theme.getLinkToBase(), member)) {
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
	//			i++;
			}
		}
		
		result.append("</ul>");
		return result.toString();
	}
	
	private boolean availableStyleMember(String linkToBase, ThemeStyleGroupMember styleMember) {
		if (styleMember == null) {
			return false;
		}
		List<String> files = styleMember.getStyleFiles();
		if (files == null) {
			return false;
		}
		
		for (int i = 0; i < files.size(); i++) {
			if (!helper.existFileInSlide(new StringBuffer(linkToBase).append(files.get(i)).toString())) {
				return false;
			}
		}
		return true;
	}

}