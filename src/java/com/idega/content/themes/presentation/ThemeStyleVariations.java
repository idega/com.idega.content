package com.idega.content.themes.presentation;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import javax.faces.component.UIComponent;

import org.springframework.beans.factory.annotation.Autowired;

import com.idega.content.business.ContentConstants;
import com.idega.content.themes.bean.ThemesManagerBean;
import com.idega.content.themes.helpers.bean.BuiltInThemeStyle;
import com.idega.content.themes.helpers.bean.Theme;
import com.idega.content.themes.helpers.bean.ThemeStyleGroupMember;
import com.idega.content.themes.helpers.business.ThemesConstants;
import com.idega.content.themes.helpers.business.ThemesHelper;
import com.idega.idegaweb.IWResourceBundle;
import com.idega.presentation.Block;
import com.idega.presentation.IWContext;
import com.idega.presentation.Image;
import com.idega.presentation.Layer;
import com.idega.presentation.Span;
import com.idega.presentation.text.ListItem;
import com.idega.presentation.text.Lists;
import com.idega.presentation.text.Text;
import com.idega.presentation.ui.CheckBox;
import com.idega.presentation.ui.GenericInput;
import com.idega.presentation.ui.RadioButton;
import com.idega.util.expression.ELUtil;
import com.idega.webface.WFUtil;

public class ThemeStyleVariations extends Block {

	private static final String VARIATION_GROUP_STYLE = "themeVariationGroup";
	private static final String VARIATION_GROUP_NAME_STYLE = "themeVariationGroupName";

	private static final String ON_CLICK_ACTION = "addThemeChangeByThemesManager('";
	private static final String SEPERATOR = "', '";

	private static final String RADIO_INPUT = "radio";
	private static final String CHECKBOX_INPUT = "checkbox";
	private static final String INPUT_CHECKED_ATTRIBUTE = "checked";

	private static final String COLOUR_CHOOSER_ACTION = "showColorChooser(";

	@Autowired
	private ThemesHelper helper;

	public ThemeStyleVariations() {
		setCacheable(getCacheKey());
	}

	@Override
	public String getCacheKey() {
		return ThemesConstants.THEME_STYLE_VARIATIONS_CACHE_KEY;
	}

	@Override
	protected String getCacheState(IWContext iwc, String cacheStatePrefix) {
		String themeID = getThemeId();
		if (themeID == null) {
			return cacheStatePrefix;
		}
		String cacheKey = new StringBuffer(cacheStatePrefix).append(themeID).toString();
		Theme theme = getHelper().getTheme(themeID);
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

	@Override
	public void main(IWContext iwc) throws Exception {
		Theme theme = getHelper().getTheme(getThemeId());
		if (theme == null) {
			return;
		}

		Layer container = new Layer();
		container.setStyleClass("allThemeVariations");

		addRehreshThemeBlock(iwc, theme, container);

		Lists styles = new Lists();
		addVariations(theme, styles);

		addBuiltInStyles(theme, styles, iwc);

		container.add(styles);
		this.add(container);
	}

	private void addBuiltInStyles(Theme theme, Lists styles, IWContext iwc) {
		List<BuiltInThemeStyle> builtInStyles = new ArrayList<BuiltInThemeStyle>(theme.getBuiltInThemeStyles());
		if (builtInStyles.isEmpty()) {
			return;
		}

		IWResourceBundle iwrb = getResourceBundle(iwc);

		BuiltInThemeStyle defaultStyle = new BuiltInThemeStyle(ThemesConstants.DEFAULT_THEME_STYLE_ID);
		defaultStyle.setName(iwrb.getLocalizedString("theme_default", "Theme Default"));
		if (!builtInStyles.contains(defaultStyle)) {
			builtInStyles.add(0, defaultStyle);
		}

		ListItem builtStyleContainer = new ListItem();
		styles.add(builtStyleContainer);

		Layer container = new Layer();
		builtStyleContainer.add(container);
		container.setStyleClass(VARIATION_GROUP_STYLE);

		Lists allBuiltInStyles = new Lists();
		container.add(allBuiltInStyles);
		ListItem allBuiltInStylesContainer = new ListItem();
		allBuiltInStyles.add(allBuiltInStylesContainer);

		Layer labelContainer = new Layer();
		allBuiltInStylesContainer.add(labelContainer);
		labelContainer.setStyleClass(VARIATION_GROUP_NAME_STYLE);
		labelContainer.add(new Text(iwrb.getLocalizedString("built_in_style", "Built-in Styles")));

		Lists realStylesContainer = new Lists();
		allBuiltInStylesContainer.add(realStylesContainer);
		ListItem builtInStyleContainer = null;
		String groupName = "themeBuiltInStylesGroup";
		RadioButton styleSelection = null;
		String themeId = theme.getId();
		String currentlyUsedStyle = theme.getCurrentlyUsedBuiltInStyleUri();
		for (BuiltInThemeStyle style: builtInStyles) {
			builtInStyleContainer = new ListItem();
			realStylesContainer.add(builtInStyleContainer);

			styleSelection = new RadioButton(groupName, style.getName());
			styleSelection.setOnClick(new StringBuilder("setBuiltInStyle('").append(themeId).append("', '").append(style.getId()).append("');").toString());
			if (currentlyUsedStyle != null && !ThemesConstants.MINUS_ONE.equals(currentlyUsedStyle) && currentlyUsedStyle.equals(style.getUri())) {
				styleSelection.setSelected();
			}
			builtInStyleContainer.add(styleSelection);
			builtInStyleContainer.add(new Text(style.getName()));
		}
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
		ListItem colorVariation = null;
		Span colourContainer = null;
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
					action.append(variation.getName()).append(SEPERATOR).append(type).append(SEPERATOR).append(input.getId()).append("', null);");
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
				colorVariation = new ListItem();

				colourContainer = new Span();
				colourContainer.add("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;");
				colorVariation.add(colourContainer);
				colourContainer.setStyleClass("themeVariationColourContainerStyle");

				String color = theme.getStyleVariableValue(variation.getVariable());
				if (color == null) {
					color = variation.getColour();
				}
				if (color != null) {
					variations.add(colorVariation);

					colourContainer.setStyleAttribute("background-color", color);
					colourContainer.setMarkupAttributeMultivalued("onclick", new StringBuffer(COLOUR_CHOOSER_ACTION).append("'").append(colourContainer.getId()).append(SEPERATOR)
							.append(variation.getVariable()).append(SEPERATOR).append(variation.getGroupName())
							.append("');").toString());

					colorVariation.add(variation.getName());
				}
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
			if (!getHelper().existFileInRepository(new StringBuffer(linkToBase).append(files.get(i)).toString())) {
				return false;
			}
		}
		return true;
	}

	@Override
	public String getBundleIdentifier()	{
		return ContentConstants.IW_BUNDLE_IDENTIFIER;
	}

	public ThemesHelper getHelper() {
		if (helper == null) {
			ELUtil.getInstance().autowire(this);
		}
		return helper;
	}

	public void setHelper(ThemesHelper helper) {
		this.helper = helper;
	}

}
