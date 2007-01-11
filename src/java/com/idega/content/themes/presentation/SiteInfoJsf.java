package com.idega.content.themes.presentation;

import com.idega.content.business.ContentUtil;
import com.idega.content.themes.helpers.ThemesHelper;
import com.idega.presentation.IWContext;
import com.idega.presentation.Table2;
import com.idega.presentation.ui.DropdownMenu;
import com.idega.presentation.ui.GenericButton;
import com.idega.webface.WFBlock;
import com.idega.webface.WFTitlebar;

public class SiteInfoJsf extends SiteInfo {
	
	private String styleClass;
	
	public SiteInfoJsf() {
		super();
	}
	
	public void main(IWContext iwc) throws Exception {
		WFBlock siteInfo = new WFBlock();
		if (styleClass != null) {
			siteInfo.setStyleClass(getStyleClass());
		}
		
		WFTitlebar bar = new WFTitlebar();
		bar.addTitleText(ContentUtil.getBundle().getLocalizedText("site_info"));
		siteInfo.setTitlebar(bar);
		
		DropdownMenu locales = getLocales(iwc, false, "getValues(this)");
		locales.setId("siteInfoLocale");
		
		Table2 table = new Table2();
		table.setCellpadding(0);
		createTableBody(table.createBodyRowGroup(), iwc, false, true);
		siteInfo.add(table);
		
		siteInfo.add(getText(ContentUtil.getBundle().getLocalizedString("locale") + ": ", false));
		siteInfo.add(locales);
		
		GenericButton save = new GenericButton(ContentUtil.getBundle().getLocalizedString("save"));
		save.setStyleClass("button");
		save.setId("saveButton");
		save.setOnClick("saveSiteInfo()");
		siteInfo.add(save);
		
		getChildren().add(siteInfo);
		
		doBusiness(iwc, ThemesHelper.getInstance().getThemeSettings().values());
	}

	public String getStyleClass() {
		return styleClass;
	}

	public void setStyleClass(String styleClass) {
		this.styleClass = styleClass;
	}

}
