package com.idega.content.themes.presentation;

import com.idega.content.business.ContentUtil;
import com.idega.presentation.IWContext;
import com.idega.presentation.Layer;
import com.idega.presentation.ui.DropdownMenu;
import com.idega.presentation.ui.GenericButton;
import com.idega.webface.WFBlock;
import com.idega.webface.WFTitlebar;

public class SiteInfoJsf extends SiteInfo {
	
	private String styleClass;
	
	public SiteInfoJsf() {
		super();
	}
	
	@Override
	public void main(IWContext iwc) throws Exception {
		WFBlock siteInfo = new WFBlock();
		if (styleClass != null) {
			siteInfo.setStyleClass(getStyleClass());
		}
		
		WFTitlebar bar = new WFTitlebar();
		bar.addTitleText(ContentUtil.getBundle().getLocalizedString("site_info", "Site Info"));
		siteInfo.setTitlebar(bar);
		
		DropdownMenu locales = getLocales(iwc, false, "getValues(this)");
		locales.setId("siteInfoLocale");
		
		createContents(siteInfo, iwc, false, true);
		
		Layer formItem = new Layer();
		formItem.setStyleClass("webfaceFormItem");
		formItem.add(getLabel(ContentUtil.getBundle().getLocalizedString("site_info.locale", "Locale"), locales));
		formItem.add(locales);
		siteInfo.getChildren().add(formItem);
		
		Layer buttonLayer = new Layer();
		buttonLayer.setStyleClass("webfaceButtonLayer");
		siteInfo.getChildren().add(buttonLayer);

		GenericButton save = new GenericButton(ContentUtil.getBundle().getLocalizedString("save", "Save"));
		save.setStyleClass("button");
		save.setId("saveSiteInfoButton");
		save.setOnClick("saveSiteInfo()");
		buttonLayer.add(save);
		
		add(siteInfo);
		
		doBusiness(iwc, getThemesHelper().getThemeSettings());
	}

	@Override
	public String getStyleClass() {
		return styleClass;
	}

	@Override
	public void setStyleClass(String styleClass) {
		this.styleClass = styleClass;
	}

}
