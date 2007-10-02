package com.idega.content.themes.presentation;

import java.util.Iterator;

import javax.faces.component.html.HtmlInputText;
import javax.faces.component.html.HtmlOutputText;
import javax.faces.context.FacesContext;

import com.idega.content.business.ContentUtil;
import com.idega.content.presentation.ContentBlock;
import com.idega.content.themes.helpers.Setting;
import com.idega.content.themes.helpers.ThemesConstants;
import com.idega.content.themes.helpers.ThemesHelper;
import com.idega.presentation.Layer;
import com.idega.presentation.Table2;
import com.idega.presentation.TableCell2;
import com.idega.presentation.TableRow;
import com.idega.presentation.TableRowGroup;
import com.idega.presentation.ui.GenericButton;

public class PageInfo extends ContentBlock {
	
	private String styleClass = null;

	@Override
	protected void initializeComponent(FacesContext context) {		
		Layer pageInfo = new Layer();
		if (styleClass != null) {
			pageInfo.setStyleClass(getStyleClass());
		}
		
		ThemesHelper.getInstance().loadPageSettings(ThemesHelper.getInstance().getWebRootWithoutContent() + ThemesConstants.PAGE_SETTINGS);
		Iterator <Setting> pageSettings = ThemesHelper.getInstance().getPageSettings().values().iterator();
		Setting s = null;
		HtmlOutputText label = null;
		HtmlInputText input = null;
		Table2 table = new Table2();
		table.setCellpadding(0);
		TableRowGroup group = table.createBodyRowGroup();
		TableRow row = null;
		TableCell2 cell = null;
		String keyPressAction = "return savePageInfoWithEnter(event)";
		for (Iterator<Setting> it = pageSettings; it.hasNext();) {
			s = it.next();
			label = new HtmlOutputText();
			label.setValue(s.getLabel());
			
			input = new HtmlInputText();
			input.setOnkeypress(keyPressAction);
			input.setId(s.getCode());
			
			row = group.createRow();
			cell = row.createCell();
			cell.add(label);
			cell = row.createCell();
			cell.add(input);
		}

		pageInfo.add(table);
		
		Layer buttonContainer = new Layer();
		buttonContainer.setStyleAttribute("float", "right");
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