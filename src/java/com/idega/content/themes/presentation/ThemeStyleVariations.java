package com.idega.content.themes.presentation;

import java.util.List;
import java.util.Map;

import javax.faces.component.UIComponent;

import com.idega.content.business.ContentConstants;
import com.idega.content.themes.bean.ThemesManagerBean;
import com.idega.content.themes.helpers.bean.Theme;
import com.idega.content.themes.helpers.bean.ThemeStyleGroupMember;
import com.idega.content.themes.helpers.business.ThemesConstants;
import com.idega.content.themes.helpers.business.ThemesHelper;
import com.idega.idegaweb.IWResourceBundle;
import com.idega.presentation.Block;
import com.idega.presentation.IWContext;
import com.idega.presentation.Image;
import com.idega.presentation.Layer;
import com.idega.presentation.text.ListItem;
import com.idega.presentation.text.Lists;
import com.idega.presentation.text.Text;
import com.idega.presentation.ui.CheckBox;
import com.idega.presentation.ui.GenericInput;
import com.idega.presentation.ui.RadioButton;
import com.idega.webface.WFUtil;

public class ThemeStyleVariations extends Block {
	
	private static final String VARIATION_GROUP_STYLE = "themeVariationGroup";
	private static final String VARIATION_GROUP_NAME_STYLE = "themeVariationGroupName";
	
	private static final String ON_CLICK_ACTION = "addThemeChange('";
	private static final String SEPERATOR = "', '";
	private static final String INPUT_CHECKED = "', this.checked);";
	
	private static final String RADIO_INPUT = "radio";
	private static final String CHECKBOX_INPUT = "checkbox";
	private static final String INPUT_CHECKED_ATTRIBUTE = "checked";
	
	private ThemesHelper helper = ThemesHelper.getInstance();
	
	public ThemeStyleVariations() {
		setCacheable(getCacheKey());
	}
	
	public String getCacheKey() {
		return ThemesConstants.THEME_STYLE_VARIATIONS_CACHE_KEY;
	}
	
	protected String getCacheState(IWContext iwc, String cacheStatePrefix) {
		String themeID = getThemeId();
		if (themeID == null) {
			return cacheStatePrefix;
		}
		String cacheKey = new StringBuffer(cacheStatePrefix).append(themeID).toString();
		Theme theme = helper.getTheme(themeID);
		if (theme != null) {
			theme.addStyleVariationsCacheKey(new StringBuffer(getCacheKey()).append(cacheKey).toString());
		}
		return cacheKey;
	}
	
	private String getThemeId() {
		Object themeID = WFUtil.invoke(ThemesManagerBean.THEMES_MANAGER_BEAN_ID, "getThemeId");
		if (themeID == null) {
			return null;
		}
		return themeID.toString();
	}
	
	public void main(IWContext iwc) throws Exception {
		Theme theme = helper.getTheme(getThemeId());
		if (theme == null) {
			return;
		}
		
		Layer container = new Layer();
		container.setStyleClass("allThemeVariations");
		
		addRehreshThemeBlock(iwc, theme, container);
		
		Lists styles = new Lists();
		addVariations(theme, styles);
		
		container.add(styles);
		this.add(container);
	}
	
	private void addRehreshThemeBlock(IWContext iwc, Theme theme, Layer container) {
		if (!iwc.isSuperAdmin()) {
			return;
		}
		Layer refresh = new Layer();
		container.add(refresh);
		
		IWResourceBundle iwrb = getResourceBundle(iwc);
		StringBuffer name = new StringBuffer(iwrb.getLocalizedString("properties", "Properties")).append(" :: ");
		name.append(iwrb.getLocalizedString("reload_properties_for_theme", "Reload properties for this theme"));
		Image reload = new Image(getBundle(iwc).getVirtualPathWithFileNameString("images/reload.png"), name.toString(), 24, 24);
		reload.setStyleClass("reload_properties_for_theme");
		reload.setMarkupAttribute("current_theme_id", theme.getId());
		
		refresh.add(reload);
	}
	
	private void addVariations(Theme theme, Lists styles) {
		String styleGroupName = null;
		List <String> styleGroups = theme.getStyleGroupsNames();
		
		Layer groupContainer = null;
		Layer nameContainer = null;
		Lists groupVariationsContainer = null;
		ListItem groupVariations = null;
		UIComponent groupVariation = null;
		for (int i = 0; i < styleGroups.size(); i++) {
			styleGroupName = styleGroups.get(i);
			
			groupVariation = getGroupVariations(theme, styleGroupName);
			if (groupVariation != null) {
			
				groupContainer = new Layer();
				groupContainer.setStyleClass(VARIATION_GROUP_STYLE);
				groupVariationsContainer = new Lists();
				groupVariations = new ListItem();
				groupVariationsContainer.add(groupVariations);
				groupContainer.add(groupVariationsContainer);
				
				nameContainer = new Layer();
				nameContainer.setStyleClass(VARIATION_GROUP_NAME_STYLE);
				nameContainer.add(new Text(styleGroupName));
				groupVariations.add(nameContainer);
			
				groupVariations.add(groupVariation);
				
				styles.add(groupContainer);
			}
		}
	}
	
	private Lists getGroupVariations(Theme theme, String groupName) {
		Lists variations = new Lists();
		
		ListItem variationContainer = null;
		Map <String, ThemeStyleGroupMember> allVariations = theme.getStyleGroupsMembers();
		if (allVariations.values() == null) {
			return null;
		}
		if (allVariations.values().size() == 0) {
			return null;
		}
		
		ThemeStyleGroupMember variation = null;
		String type = null;
		String nameInMap = new StringBuffer(groupName).append(ThemesConstants.AT).toString();
		GenericInput input = null;
		StringBuffer action = null;
		for (int i = 0; allVariations.get(new StringBuffer(nameInMap).append(i).toString()) != null; i++) {
			variation = allVariations.get(new StringBuffer(nameInMap).append(i).toString());
			if (variation.isStylesheet()) {
				if (availableStyleMember(theme.getLinkToBase(), variation)) {
					variationContainer = new ListItem();
					if (variation.isLimitedSelection()) {
						input = new RadioButton();
						type = RADIO_INPUT;
					}
					else {
						input = new CheckBox();
						type = CHECKBOX_INPUT;
					}
					input.setName(groupName);
					input.setValue(variation.getName());
					action = new StringBuffer(ON_CLICK_ACTION).append(theme.getId()).append(SEPERATOR).append(groupName).append(SEPERATOR);
					action.append(variation.getName()).append(SEPERATOR).append(type).append(INPUT_CHECKED);
					input.setOnClick(action.toString());
					if (variation.isEnabled()) {
						input.setMarkupAttribute(INPUT_CHECKED_ATTRIBUTE, true);
					}
					variationContainer.add(input);
					variationContainer.add(new Text(variation.getName()));
					variations.add(variationContainer);
				}
			}
			else {
				return null;	//	TODO: handle it
			}
		}
		
		return variations;
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
	
	public String getBundleIdentifier()	{
		return ContentConstants.IW_BUNDLE_IDENTIFIER;
	}

}
