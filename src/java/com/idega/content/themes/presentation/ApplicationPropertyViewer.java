package com.idega.content.themes.presentation;

import java.rmi.RemoteException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import javax.faces.context.FacesContext;

import com.idega.block.web2.business.Web2Business;
import com.idega.content.business.ContentConstants;
import com.idega.content.business.ContentUtil;
import com.idega.content.themes.helpers.bean.Setting;
import com.idega.content.themes.helpers.business.ThemesConstants;
import com.idega.content.themes.helpers.business.ThemesHelper;
import com.idega.idegaweb.IWBundle;
import com.idega.idegaweb.IWResourceBundle;
import com.idega.idegaweb.IWUserContext;
import com.idega.presentation.Block;
import com.idega.presentation.IWContext;
import com.idega.presentation.Image;
import com.idega.presentation.PresentationObject;
import com.idega.presentation.text.Text;
import com.idega.util.CoreConstants;
import com.idega.util.PresentationUtil;
import com.idega.util.StringUtil;
import com.idega.webface.WFUtil;

/**
 * This class displays the current value of an application property, localized if available.
 * You have to use the method setApplicationPropertyKey to make it display anything.
 * You can also use setDefaultValue to initialize a key that has not been set.
 * 
 * @author valdas
 *
 */
public class ApplicationPropertyViewer extends Block {
	
	private static final String STYLE_CLASS = "applicationPropertyStyleClass";
	private static final String FIFTEEN_SPACE = "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;";
	private static final String TEMP_IMAGE = "images/spacer.gif";
	
	private String applicationPropertyKey = null;
	
	public ApplicationPropertyViewer() {
		setUseBuilderObjectControl(false);	//	We don't need 'wrappers' in Builder for this PO
	}
	
	@Override
	public void main(IWContext iwc) {
		if (applicationPropertyKey == null) {
			return;
		}
		
		String settingKey = applicationPropertyKey;
		String key = null;
		if (applicationPropertyKey.indexOf(ThemesConstants.THEMES_PROPERTY_START) == -1) {
			applicationPropertyKey = new StringBuffer(ThemesConstants.THEMES_PROPERTY_START).append(applicationPropertyKey).toString();
		}
		
		String value = null;
		String language = ThemesHelper.getInstance().getCurrentLanguage(iwc);
		
		key = new StringBuffer(applicationPropertyKey).append(CoreConstants.DOT).append(language).toString();
		value = iwc.getApplicationSettings().getProperty(key);
		if (value == null) {
			//	Didn't find localized value, getting default value
			key = new StringBuffer(applicationPropertyKey).append(ThemesConstants.THEMES_PROPERTY_END).toString();
			value = iwc.getApplicationSettings().getProperty(key);
		}
		
		if (value == null) {
			return;
		}
		
		if (!ContentUtil.hasContentEditorRoles(iwc)) {
			if (StringUtil.isEmpty(value) || FIFTEEN_SPACE.equals(value) || value.length() == 15) {	// Nothing to display
				return;
			}
		}
		
		if (key.indexOf(getCheckKey(ThemesConstants.LOGO)) != -1) {
			IWBundle bundle = ContentUtil.getBundle();
			
			String siteLogo = "site_logo";
			String name = bundle.getResourceBundle(iwc).getLocalizedString(siteLogo);
			if (value.equals(ContentConstants.EMPTY)) {
				name = FIFTEEN_SPACE;
				value = bundle.getVirtualPathWithFileNameString(TEMP_IMAGE);

			}
			Image image = new Image(value, name);
			addPropertyEditAction(iwc, image, key, settingKey, true);
			this.add(image);
			return;
		}
		
		if (value.equals(ContentConstants.EMPTY)) {
			value = FIFTEEN_SPACE;
		}
		
		if (key.indexOf(getCheckKey(ThemesConstants.SITE_TITLE)) != -1) {
			Text h1 = new Text(value);
			addPropertyEditAction(iwc, h1, key, settingKey, false);
			this.add(h1);
			return;
		}
		
		if (key.indexOf(getCheckKey(ThemesConstants.SITE_SLOGAN)) != -1) {
			Text h2 = new Text(value);
			addPropertyEditAction(iwc, h2, key, settingKey, false);
			this.add(h2);
			return;
		}
		
		Text text = new Text(value); // Simple text
		addPropertyEditAction(iwc, text, key, settingKey, false);
		text.setStyleClass(STYLE_CLASS);
		this.add(text);
	}
	
	public void setApplicationPropertyKey(String key){
		this.applicationPropertyKey = key;
	}
	
	@Override
	public void restoreState(FacesContext context, Object state) {
		Object values[] = (Object[]) state;
		super.restoreState(context, values[0]);
		this.applicationPropertyKey = (String) values[1];
	}

	@Override
	public Object saveState(FacesContext context) {
		Object values[] = new Object[2];
		values[0] = super.saveState(context);
		values[1] = this.applicationPropertyKey;
		return values;
	}
	
	@SuppressWarnings("unchecked")
	private void addPropertyEditAction(IWContext iwc, PresentationObject component, String key, String settingKey, boolean needsReload) {
		if (iwc == null || component == null || key == null) {
			return;
		}
		if (!ContentUtil.hasContentEditorRoles(iwc)) {
			return;
		}
		
		IWBundle bundle = ContentUtil.getBundle();
		IWResourceBundle iwrb = bundle.getResourceBundle(iwc);
		Web2Business web2 = (Web2Business) WFUtil.getBeanInstance(Web2Business.SPRING_BEAN_IDENTIFIER);
		
		//	Adding script files
		List<String> scripts = new ArrayList<String>();
		scripts.add(CoreConstants.DWR_ENGINE_SCRIPT);
		scripts.add("/dwr/interface/ThemesEngine.js");
		scripts.add(bundle.getVirtualPathWithFileNameString("javascript/ApplicationPropertyHelper.js"));
		try {
			scripts.add(web2.getBundleURIToMootoolsLib());
		} catch (RemoteException e) {
			e.printStackTrace();
		}
		
		String property = ThemesHelper.getInstance().extractValueFromString(key, key.indexOf(CoreConstants.DOT) + 1, key.lastIndexOf(CoreConstants.DOT));
		
		Map<String, Setting> settings = ThemesHelper.getInstance().getThemeSettings();
		Setting s = settings.get(settingKey);
		String title = ContentConstants.EMPTY;
		if (s != null) {
			title = s.getLabel();
		}
		
		String id = new StringBuffer(property).append(ThemesConstants.ADD_FOR_PROPERTY_CHANGE).append(component.getId()).toString();
		component.setID(id);
		if (component.attributes == null) {
			component.attributes = new HashMap();
		}
		StringBuffer onDblClickAction = new StringBuffer();
		onDblClickAction.append("changeSiteInfo('").append(id).append("', '").append(iwrb.getLocalizedString("saving", "Saving..."));
		onDblClickAction.append("', ").append(needsReload).append(")");
		component.attributes.put("ondblclick", PresentationUtil.getJavaScriptLinesLoadedLazily(scripts, onDblClickAction.toString()));
		
		component.setStyleClass(STYLE_CLASS);
		
		String localizedText = iwrb.getLocalizedString("double_click_to_edit", "Double click to edit");
		component.setToolTip(new StringBuffer(title).append(": ").append(localizedText).toString());
	}
	
	@Override
	public String getBuilderName(IWUserContext iwuc) {
		String name = ContentUtil.getBundle().getComponentName(ApplicationPropertyViewer.class);
		if (name == null || ThemesConstants.EMPTY.equals(name)) {
			return ApplicationPropertyViewer.class.getSimpleName();
		}
		return name;
	}
	
	private String getCheckKey(String keyWord) {
		return new StringBuffer(ThemesConstants.THEMES_PROPERTY_START).append(keyWord).append(CoreConstants.DOT).toString();
	}
}
