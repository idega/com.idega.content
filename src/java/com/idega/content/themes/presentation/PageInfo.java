package com.idega.content.themes.presentation;

import java.util.Iterator;

import javax.faces.component.html.HtmlInputText;
import javax.faces.context.FacesContext;

import com.idega.content.business.ContentUtil;
import com.idega.content.presentation.ContentBlock;
import com.idega.content.themes.helpers.Setting;
import com.idega.content.themes.helpers.ThemesConstants;
import com.idega.content.themes.helpers.ThemesHelper;
import com.idega.presentation.Layer;
import com.idega.presentation.ui.GenericButton;
import com.idega.presentation.ui.Label;

public class PageInfo extends ContentBlock {

	private String styleClass = null;

	@Override
	protected void initializeComponent(FacesContext context) {
		Layer pageInfo = new Layer();
		if (styleClass != null) {
			pageInfo.setStyleClass(getStyleClass());
		}

		ThemesHelper.getInstance().loadPageSettings(ThemesHelper.getInstance().getWebRootWithoutContent() + ThemesConstants.PAGE_SETTINGS);
		Iterator<Setting> pageSettings = ThemesHelper.getInstance().getPageSettings().values().iterator();
		Setting s = null;

		Layer layer = new Layer();
		layer.setStyleClass("webfaceFormSection");
		pageInfo.add(layer);
		
		String keyPressAction = "return savePageInfoWithEnter(event)";
		for (Iterator<Setting> it = pageSettings; it.hasNext();) {
			s = it.next();
			
			HtmlInputText input = new HtmlInputText();
			input.setOnkeypress(keyPressAction);
			input.setId(s.getCode());

			Label label = new Label(s.getLabel(), input);

			Layer formItem = new Layer();
			formItem.setStyleClass("webfaceFormItem");
			formItem.add(label);
			formItem.add(input);
			layer.add(formItem);
		}

		Layer buttonContainer = new Layer();
		buttonContainer.setStyleClass("webfaceButtonLayer");
		
		GenericButton save = new GenericButton("saveButton", ContentUtil.getBundle().getLocalizedString("save"));
		save.setInputType("button");
		save.setId("saveButton");
		save.setStyleClass("saveButtonStyleClass");
		buttonContainer.add(save);

		pageInfo.add(buttonContainer);

		add(pageInfo);
	}

	public String getStyleClass() {
		return styleClass;
	}

	public void setStyleClass(String styleClass) {
		this.styleClass = styleClass;
	}

}