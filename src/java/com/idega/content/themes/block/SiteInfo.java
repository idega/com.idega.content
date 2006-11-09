package com.idega.content.themes.block;

import java.util.Iterator;

import com.idega.content.themes.helpers.ThemeSettings;
import com.idega.content.themes.helpers.ThemesConstants;
import com.idega.content.themes.helpers.ThemesHelper;
import com.idega.core.localisation.presentation.LocalePresentationUtil;
import com.idega.idegaweb.IWMainApplicationSettings;
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
	private static final String UNDER = "_";
	private static final String SAVE_ACTION = "save";
	private static final String TYPE_TEXT = "text";
	
	private String locale = null;
	
	public SiteInfo() {
		super();
	}
	
	public void main(IWContext iwc) throws Exception {
		locale = iwc.getParameter(IW_LOCALES);
		DropdownMenu locales = LocalePresentationUtil.getAvailableLocalesDropdown(iwc.getIWMainApplication(), IW_LOCALES);
		locales.keepStatusOnAction();
		locales.setToSubmit();
		if (locale == null) {
			locale = iwc.getCurrentLocale().getLanguage();
			locales.setSelectedElement(locale);
		}
		doBusiness(iwc, ThemesHelper.getInstance().getSettings().values().iterator());
		
		IWMainApplicationSettings applicationSettings  = iwc.getApplicationSettings();
		
		Form form = new Form();
		form.maintainParameter(ACTION_PARAMETER);
		form.maintainParameter(PARAMETER_CLASS_NAME);

		Table2 table = new Table2();
		table.setCellpadding(0);
		form.add(table);
		
		TableRowGroup group = table.createHeaderRowGroup();
		TableRow row = group.createRow();
		
		form.add(getText("Locale: "));
		form.add(locales);
		
		locales.getIndex();

		TableCell2 cell = row.createHeaderCell();
		cell.add(new Text("Region"));

		cell = row.createHeaderCell();
		cell.add(new Text("Value"));
		
		group = table.createBodyRowGroup();
		
		ThemeSettings setting = null;
		String value = null;
		TextInput regionValue = null;
		Iterator <ThemeSettings> it = ThemesHelper.getInstance().getSettings().values().iterator();
		while(it.hasNext()) {
			setting = it.next();
			row = group.createRow();
			cell = row.createCell();
			cell.add(getText(setting.getLabel()));
			
			cell = row.createCell();
			if (TYPE_TEXT.equals(setting.getType())) {
				regionValue = new TextInput(ThemesConstants.THEMES_PROPERTY_START + setting.getCode() + UNDER + REGION_VALUE);
				value = applicationSettings.getProperty(ThemesConstants.THEMES_PROPERTY_START + setting.getCode() + UNDER + locale);
				regionValue.setValue(value);
				cell.add(regionValue);
			}
		}

		SubmitButton save = new SubmitButton("Save", SAVE_PARAMETER, SAVE_ACTION);
		save.setStyleClass("button");
		save.setID(SAVE_ACTION);
		form.add(save);		
		add(form);
	}
	
	private void doBusiness(IWContext iwc, Iterator <ThemeSettings> it) {
		if (!SAVE_ACTION.equals(iwc.getParameter(SAVE_PARAMETER))) {
			return;
		}
		ThemeSettings setting = null;
		IWMainApplicationSettings applicationSettings  = iwc.getApplicationSettings();
		String value = null;
		while (it.hasNext()) {
			setting = it.next();
			value = iwc.getParameter(ThemesConstants.THEMES_PROPERTY_START + setting.getCode() + UNDER + REGION_VALUE);
			if (value != null) {
				applicationSettings.setProperty(ThemesConstants.THEMES_PROPERTY_START + setting.getCode() + UNDER + locale, value);
			}
		}
	}
	
	private Text getText(String text) {
		Text T = new Text(text);
		T.setBold();
		T.setFontFace(Text.FONT_FACE_VERDANA);
		return T;
	}
	
}