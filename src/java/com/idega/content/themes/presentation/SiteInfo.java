package com.idega.content.themes.presentation;

import java.util.List;

import javax.faces.component.UIComponent;

import org.springframework.beans.factory.annotation.Autowired;

import com.idega.content.business.ContentUtil;
import com.idega.content.lucid.business.LucidEngine;
import com.idega.content.themes.helpers.bean.Setting;
import com.idega.content.themes.helpers.business.ThemesConstants;
import com.idega.content.themes.helpers.business.ThemesHelper;
import com.idega.core.builder.data.ICDomain;
import com.idega.core.localisation.business.ICLocaleBusiness;
import com.idega.idegaweb.IWResourceBundle;
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
import com.idega.util.expression.ELUtil;

public class SiteInfo extends Block {
	
	private static final String REGION_VALUE = "region_value";
	private static final String IW_LOCALES = "iw_locales";
	private static final String ACTION_PARAMETER = "theme_regions";
	private static final String PARAMETER_CLASS_NAME = "iwcontent_class_name";
	private static final String SAVE_PARAMETER = "save_site_info";
	private static final String SAVE_ACTION = "save";
	private static final String TYPE_TEXT = "text";
	
	private String locale = null;
	
	@Autowired
	private LucidEngine lucidEngine;
	
	public SiteInfo() {
		super();
		
		try {
			ELUtil.getInstance().autowire(this);
		} catch(Exception e) {
			e.printStackTrace();
		}
	}
	
	@Override
	public void main(IWContext iwc) throws Exception {
		addForm(iwc);
	}
	
	protected DropdownMenu getLocales(IWContext iwc, boolean submit, String onChnageAction) {
		locale = iwc.getParameter(IW_LOCALES);
		DropdownMenu locales = ICLocaleBusiness.getAvailableLocalesDropdownStringKeyed(iwc.getIWMainApplication(), IW_LOCALES, true);
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
		List<Setting> settings = ThemesHelper.getInstance().getThemeSettings();
		if (settings == null) {
			return;
		}
		if (locale == null || lucidEngine == null) {
			return;
		}
		
		IWResourceBundle iwrb = ContentUtil.getBundle().getResourceBundle(iwc);
		ICDomain domain = iwc.getIWMainApplication().getIWApplicationContext().getDomain();
		String keyPressAction = "return saveSiteInfoWithEnter(event)";
		for (Setting setting: settings) {
			Layer formItem = new Layer();
			formItem.setStyleClass("webfaceFormItem");
			if (TYPE_TEXT.equals(setting.getType())) {
				TextInput regionValue = new TextInput(new StringBuilder(ThemesConstants.THEMES_PROPERTY_START).append(setting.getCode()).append(CoreConstants.DOT)
														.append(REGION_VALUE).toString());
				if (addKeyPressAction) {
					regionValue.setOnKeyPress(keyPressAction);
				}
				regionValue.setId(new StringBuilder("id").append(setting.getCode()).toString());
				regionValue.setValue(lucidEngine.getSiteInfoValue(setting.getCode(), locale, iwc.getApplicationSettings(), domain));
				
				formItem.add(getLabel(iwrb.getLocalizedString(new StringBuilder("site_info.").append(setting.getCode()).toString(), setting.getLabel()),
																															regionValue));
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
		
		doBusiness(iwc, ThemesHelper.getInstance().getThemeSettings());
		
		DropdownMenu locales = getLocales(iwc, true, null);
		createContents(layer, iwc, true, false);
		
		IWResourceBundle iwrb = ContentUtil.getBundle().getResourceBundle(iwc);
		
		Layer formItem = new Layer();
		formItem.setStyleClass("webfaceFormItem");
		formItem.add(getLabel(iwrb.getLocalizedString("site_info.locale", "Locale"), locales));
		formItem.add(locales);
		layer.add(formItem);
		
		Layer buttonLayer = new Layer();
		buttonLayer.setStyleClass("webfaceButtonLayer");
		form.add(buttonLayer);

		SubmitButton save = new SubmitButton(iwrb.getLocalizedString("save", "Save"), SAVE_PARAMETER, SAVE_ACTION);
		save.setStyleClass("button");
		save.setID(SAVE_ACTION);
		buttonLayer.add(save);
		
		add(form);
	}
	
	protected void doBusiness(IWContext iwc, List <Setting> settings) {
		if (!SAVE_ACTION.equals(iwc.getParameter(SAVE_PARAMETER))) {
			return;
		}
		if (locale == null || settings == null) {
			return;
		}
		
		Setting setting = null;
		String[] keywords = new String[settings.size()];
		String[] values = new String[settings.size()];
		for (int i = 0; i < settings.size(); i++) {
			setting = settings.get(i);
			keywords[i] = setting.getCode();
			values[i] = iwc.getParameter(new StringBuilder(ThemesConstants.THEMES_PROPERTY_START).append(setting.getCode()).append(CoreConstants.DOT)
											.append(REGION_VALUE).toString());
		}
		
		try {
			lucidEngine.saveSiteInfo(locale, keywords, values);
		} catch(Exception e) {
			e.printStackTrace();
		}
	}
	
	protected Label getLabel(String text, InterfaceObject component) {
		Label label = new Label(text, component);
		return label;
	}

	public LucidEngine getLucidEngine() {
		return lucidEngine;
	}

	public void setLucidEngine(LucidEngine lucidEngine) {
		this.lucidEngine = lucidEngine;
	}
	
}