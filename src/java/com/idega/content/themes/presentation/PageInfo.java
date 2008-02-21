package com.idega.content.themes.presentation;

import java.util.Iterator;

import javax.faces.context.FacesContext;

import com.idega.content.business.ContentConstants;
import com.idega.content.business.ContentUtil;
import com.idega.content.presentation.ContentBlock;
import com.idega.content.themes.helpers.bean.Setting;
import com.idega.content.themes.helpers.business.ThemesConstants;
import com.idega.content.themes.helpers.business.ThemesHelper;
import com.idega.core.accesscontrol.business.StandardRoles;
import com.idega.core.builder.data.ICPage;
import com.idega.idegaweb.IWResourceBundle;
import com.idega.presentation.IWContext;
import com.idega.presentation.Layer;
import com.idega.presentation.ui.GenericButton;
import com.idega.presentation.ui.HiddenInput;
import com.idega.presentation.ui.Label;
import com.idega.presentation.ui.RadioButton;
import com.idega.presentation.ui.TextInput;
import com.idega.util.CoreConstants;

public class PageInfo extends ContentBlock {

	private String styleClass = null;
	private String pageKey = null;
	
	private Boolean hasEditorRole = null;
	private Boolean pageIsPublished = null;

	@Override
	protected void initializeComponent(FacesContext context) {
		IWContext iwc = IWContext.getIWContext(context);
		if (!needAddPageInfo(iwc)) {
			return;
		}
		
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
		
		IWResourceBundle iwrb = getIWResourceBundle(context, ContentConstants.IW_BUNDLE_IDENTIFIER);
		String localizedLabel = null;
		for (Iterator<Setting> it = pageSettings; it.hasNext();) {
			s = it.next();
			
			if (canAddSection(iwc, s.getCode())) {
				Layer container = new Layer();
				container.setStyleClass("webfaceFormItem");
				
				Label label = new Label();
				localizedLabel = iwrb.getLocalizedString(s.getCode());
				if (localizedLabel == null) {
					localizedLabel = s.getLabel();
				}
				label.setLabel(localizedLabel);
				container.add(label);
				
				if (ContentConstants.BOOLEAN_TYPE.equals(s.getType())) {
					Layer buttons = new Layer();
					
					HiddenInput hiddenInput = new HiddenInput();
					hiddenInput.setID(s.getCode());
					buttons.add(hiddenInput);
					
					RadioButton yes = getRadioButton(Boolean.TRUE.toString(), s.getCode());
					Label yesLabel = new Label(iwrb.getLocalizedString("yes", "Yes"), yes);
					yesLabel.setStyleAttribute("width", "20px");
					
					RadioButton no = getRadioButton(Boolean.FALSE.toString(), s.getCode());
					Label noLabel = new Label(iwrb.getLocalizedString("no", "No"), no);
					noLabel.setStyleAttribute("width", "20px");
					
					buttons.add(yesLabel);
					buttons.add(yes);
					
					buttons.add(noLabel);
					buttons.add(no);
					
					label.setFor(buttons.getID());
					container.add(buttons);
				}
				else {
					TextInput input = new TextInput();
					input.setOnKeyPress(keyPressAction);
					input.setId(s.getCode());
		
					label.setFor(input.getId());
					container.add(input);
				}
				
				layer.add(container);
			}
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
	
	private boolean needAddPageInfo(IWContext iwc) {
		if (iwc == null) {
			return false;
		}
		
		if (hasEditorRole == null) {
			hasEditorRole = iwc.hasRole(StandardRoles.ROLE_KEY_EDITOR);
		}
		if (hasEditorRole) {
			return true;
		}
		
		if (pageIsPublished == null) {
			pageIsPublished = isPagePublished(iwc);
		}
		if (pageIsPublished) {
			//	Role is less than editor and page is published - user can do nothing
			return false;
		}
		
		return true;
	}
	
	private boolean canAddSection(IWContext iwc, String code) {
		if (iwc == null || code == null) {
			return false;
		}
		
		if (!needAddPageInfo(iwc)) {
			return false;
		}
		
		//	Page is not published
		//	"Less" role than content_editor
		if (code.equals(ContentConstants.PUBLISH_PAGE_IN_LUCID_CODE) && !hasEditorRole) {
			//	Publish/unpublish can only user with content_editor role
			return false;
		}
		
		return true;
	}
	
	private boolean isPagePublished(IWContext iwc) {
		if (pageKey == null) {
			pageKey = String.valueOf(iwc.getCurrentIBPageID());
		}
		
		ICPage page = ThemesHelper.getInstance(false).getThemesService().getICPage(pageKey);
		
		return page == null ? true : page.isPublished();
	}
	
	private RadioButton getRadioButton(String value, String code) {
		RadioButton button = new RadioButton(code, value);
		button.setMarkupAttribute("radioButtonCode", code);
		button.setStyleAttribute("width", "13px");
		button.setId(new StringBuffer(code).append(CoreConstants.UNDER).append(value).toString());
		
		button.setOnClick(new StringBuffer("savePageInfoWithRadioButtonValue('").append(button.getId()).append("');").toString());
		
		return button;
	}

	public String getStyleClass() {
		return styleClass;
	}

	public void setStyleClass(String styleClass) {
		this.styleClass = styleClass;
	}

	public void setPageKey(String pageKey) {
		this.pageKey = pageKey;
	}

}