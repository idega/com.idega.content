package com.idega.content.themes.presentation;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import com.idega.content.business.ContentUtil;
import com.idega.content.themes.business.ThemesEngine;
import com.idega.content.themes.helpers.Setting;
import com.idega.content.themes.helpers.ThemesConstants;
import com.idega.content.themes.helpers.ThemesHelper;
import com.idega.core.builder.data.ICDomain;
import com.idega.core.localisation.presentation.LocalePresentationUtil;
import com.idega.presentation.Block;
import com.idega.presentation.IWContext;
import com.idega.presentation.Table2;
import com.idega.presentation.TableCell2;
import com.idega.presentation.TableRow;
import com.idega.presentation.TableRowGroup;
import com.idega.presentation.text.Text;
import com.idega.presentation.ui.DropdownMenu;
import com.idega.presentation.ui.Form;
import com.idega.presentation.ui.SubmitButton;
import com.idega.presentation.ui.TextInput;

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
	
	protected void createTableBody(TableRowGroup group, IWContext iwc, boolean boldText, boolean addKeyPressAction) {
		TableRow row = null;
		TableCell2 cell = null;
		
		Setting setting = null;
		String value = null;
		TextInput regionValue = null;
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
			row = group.createRow();
			cell = row.createCell();
			cell.add(getText(setting.getLabel(), boldText));
			
			cell = row.createCell();
			if (TYPE_TEXT.equals(setting.getType())) {
				regionValue = new TextInput(ThemesConstants.THEMES_PROPERTY_START + setting.getCode() + ThemesConstants.DOT +
						REGION_VALUE);
				if (addKeyPressAction) {
					regionValue.setOnKeyPress(keyPressAction);
				}
				regionValue.setId(setting.getCode());
				value = engine.getSiteInfoValue(setting.getCode(), locale, iwc.getApplicationSettings(), domain);
				regionValue.setValue(value);
				cell.add(regionValue);
			}
		}
	}
	
	private void addForm(IWContext iwc) {
		Form form = new Form();
		form.maintainParameter(ACTION_PARAMETER);
		form.maintainParameter(PARAMETER_CLASS_NAME);
		
		Table2 table = new Table2();
		table.setCellpadding(0);
		form.add(table);
		
		TableRowGroup group = table.createHeaderRowGroup();
		TableRow row = group.createRow();
		form.add(getText(ContentUtil.getBundle().getLocalizedString("locale") + ": ", true));
		form.add(getLocales(iwc, true, null));

		TableCell2 cell = row.createHeaderCell();
		cell.add(new Text(ContentUtil.getBundle().getLocalizedString("region")));

		cell = row.createHeaderCell();
		cell.add(new Text(ContentUtil.getBundle().getLocalizedString("value")));
		
		doBusiness(iwc, ThemesHelper.getInstance().getThemeSettings().values());
		createTableBody(table.createBodyRowGroup(), iwc, true, false);
		
		SubmitButton save = new SubmitButton(ContentUtil.getBundle().getLocalizedString("save"), SAVE_PARAMETER, SAVE_ACTION);
		save.setStyleClass("button");
		save.setID(SAVE_ACTION);
		form.add(save);		
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
			values[i] = iwc.getParameter(ThemesConstants.THEMES_PROPERTY_START + setting.getCode() + ThemesConstants.DOT +
					REGION_VALUE);
		}
		ThemesHelper.getInstance().getThemesEngine().saveSiteInfo(locale, keywords, values);
	}
	
	protected Text getText(String text, boolean bold) {
		Text T = new Text(text);
		T.setBold(bold);
		T.setFontFace(Text.FONT_FACE_VERDANA);
		return T;
	}
	
}