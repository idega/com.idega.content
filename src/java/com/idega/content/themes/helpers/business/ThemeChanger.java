package com.idega.content.themes.helpers.business;

import java.util.List;
import java.util.Map;

import org.jdom.Document;
import org.jdom.output.XMLOutputter;

import com.idega.business.SpringBeanName;
import com.idega.content.themes.helpers.bean.Theme;
import com.idega.content.themes.helpers.bean.ThemeChange;
import com.idega.content.themes.helpers.bean.ThemeStyleGroupMember;

@SpringBeanName("themeChanger")
public interface ThemeChanger {
	
	public boolean restoreTheme(String themeID) throws Exception;

	public boolean saveTheme(String themeID, String themeName) throws Exception;
	
	public boolean setSelectedStyles(Theme theme, List<ThemeChange> enabledStyles) throws Exception;
	
	public String applyMultipleChangesToTheme(String themeID, List<ThemeChange> changes, String themeName) throws Exception;
	
	public String changeTheme(String themeID, String styleGroupName, String variation, String themeName, boolean radio, boolean checked) throws Exception;
	
	public List <ThemeStyleGroupMember> getEnabledStyles(Theme theme) throws Exception;
	
	public ThemeStyleGroupMember getMember(Map <String, ThemeStyleGroupMember> styleMembers, String styleGroupName, int index) throws Exception;
	
	public ThemeStyleGroupMember getStyleMember(Theme theme, String styleGroupName, String styleVariation) throws Exception;
	
	public XMLOutputter getXMLOutputter() throws Exception;
	
	public boolean prepareThemeForUsage(Theme theme) throws Exception;
	
	public boolean prepareThemeStyleFiles(Theme theme) throws Exception;
	
	public boolean uploadDocument(Document doc, String linkToBase, String fileName, Theme theme, boolean isTheme) throws Exception;
	
	public boolean reloadThemeProperties(String themeId, boolean checkConfig) throws Exception;
}