package com.idega.content.themes.presentation;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import javax.faces.component.UIComponent;

import com.idega.content.business.ContentUtil;
import com.idega.content.themes.business.ThemesEngine;
import com.idega.content.themes.helpers.Setting;
import com.idega.content.themes.helpers.ThemesConstants;
import com.idega.content.themes.helpers.ThemesHelper;
import com.idega.core.builder.data.ICDomain;
import com.idega.core.localisation.presentation.LocalePresentationUtil;
import com.idega.presentation.Block;
import com.idega.presentation.IWContext;
import com.idega.presentation.Layer;
import com.idega.presentation.ui.DropdownMenu;
import com.idega.presentation.ui.Form;
import com.idega.presentation.ui.InterfaceObject;
import com.idega.presentation.ui.Label;
import com.idega.presentation.ui.SubmitButton;
import com.idega.presentation.ui.TextInput;
import com.idega.util.CoreConstants;

public class SiteInfo extends Block {
	
	private static final String REGION_VALUE = "region_value";
	private static final String IW_LOCALES = "iw_locales";
	private static final String ACTION_PARAMETER = "theme_regions";
	private static final String PARAMETER_CLASS_NAME = "iwcontent_class_name";
	private static final String SAVE_PARAMETER = "save_site_info";
	private static final String SAVE_ACTION = "save";
	private static final String TYPE_TEXT = "text";
	
	private String locale = null;
	
	public SiteInfo() {
		super();
	}
	
	@Override
	public void main(IWContext iwc) throws Exception {
		addForm(iwc);
	}
	
	protected DropdownMenu getLocales(IWContext iwc, boolean submit, String onChnageAction) {
		locale = iwc.getParameter(IW_LOCALES);
		DropdownMenu locales = LocalePresentationUtil.getAvailableLocalesDropdown(iwc.getIWMainApplication(), IW_LOCALES);
		locales.keepStatusOnAction();
		locales.setToSubmit(submit);
		if (onChnageAction != null) {
			locales.setOnChange(onChnageAction);
		}
		if (locale == null) {
			locale = ThemesHelper.getInstance().getCurrentLanguage(iwc);
			locales.setSelectedElement(locale);
		}
		return locales;
	}
	
	protected void createContents(UIComponent layer, IWContext iwc, boolean boldText, boolean addKeyPressAction) {
		Setting setting = null;
		List <Setting> settings = new ArrayList <Setting> (ThemesHelper.getInstance().getThemeSettings().values());
		if (settings == null) {
			return;
		}
		if (locale == null) {
			return;
		}

		ThemesEngine engine = ThemesHelper.getInstance().getThemesEngine();
		ICDomain domain = iwc.getIWMainApplication().getIWApplicationContext().getDomain();
		String keyPressAction = "return saveSiteInfoWithEnter(event)";
		for (int i = 0; i < settings.size(); i++) {
			setting = settings.get(i);
			
			Layer formItem = new Layer();
			formItem.setStyleClass("webfaceFormItem");
			if (TYPE_TEXT.equals(setting.getType())) {
				TextInput regionValue = new TextInput(ThemesConstants.THEMES_PROPERTY_START + setting.getCode() + CoreConstants.DOT + REGION_VALUE);
				if (addKeyPressAction) {
					regionValue.setOnKeyPress(keyPressAction);
				}
				regionValue.setId(setting.getCode());
				regionValue.setValue(engine.getSiteInfoValue(setting.getCode(), locale, iwc.getApplicationSettings(), domain));
				
				formItem.add(getLabel(setting.getLabel(), regionValue));
				formItem.add(regionValue);
				layer.getChildren().add(formItem);
			}
		}
	}
	
	private void addForm(IWContext iwc) {
		Form form = new Form();
		form.maintainParameter(ACTION_PARAMETER);
		form.maintainParameter(PARAMETER_CLASS_NAME);
		form.setID("siteInfoForm");
		
		Layer layer = new Layer();
		layer.setStyleClass("webfaceFormSection");
		form.add(layer);
		
		doBusiness(iwc, ThemesHelper.getInstance().getThemeSettings().values());
		createContents(layer, iwc, true, false);
		
		DropdownMenu locales = getLocales(iwc, true, null);
		
		Layer formItem = new Layer();
		formItem.setStyleClass("webfaceFormItem");
		formItem.add(getLabel(ContentUtil.getBundle().getLocalizedString("locale"), locales));
		formItem.add(locales);
		layer.add(formItem);
		
		Layer buttonLayer = new Layer();
		buttonLayer.setStyleClass("webfaceButtonLayer");
		form.add(buttonLayer);

		SubmitButton save = new SubmitButton(ContentUtil.getBundle().getLocalizedString("save"), SAVE_PARAMETER, SAVE_ACTION);
		save.setStyleClass("button");
		save.setID(SAVE_ACTION);
		buttonLayer.add(save);
		
		add(form);
	}
	
	protected void doBusiness(IWContext iwc, Collection <Setting> c) {
		if (!SAVE_ACTION.equals(iwc.getParameter(SAVE_PARAMETER))) {
			return;
		}
		if (locale == null || c == null) {
			return;
		}
		
		Setting setting = null;
		List <Setting> l = new ArrayList <Setting> (c);
		String[] keywords = new String[l.size()];
		String[] values = new String[l.size()];
		for (int i = 0; i < l.size(); i++) {
			setting = l.get(i);
			keywords[i] = setting.getCode();
			values[i] = iwc.getParameter(ThemesConstants.THEMES_PROPERTY_START + setting.getCode() + CoreConstants.DOT +
					REGION_VALUE);
		}
		ThemesHelper.getInstance().getThemesEngine().saveSiteInfo(locale, keywords, values);
	}
	
	protected Label getLabel(String text, InterfaceObject component) {
		Label label = new Label(text, component);
		return label;
	}
	
}