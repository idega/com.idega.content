package com.idega.content.themes.presentation;

import java.util.Locale;

import javax.faces.context.FacesContext;

import com.idega.content.themes.helpers.ThemesConstants;
import com.idega.content.themes.helpers.ThemesHelper;
import com.idega.core.builder.data.ICPage;
import com.idega.presentation.Block;
import com.idega.presentation.IWContext;
import com.idega.presentation.text.Link;
import com.idega.presentation.text.ListItem;
import com.idega.presentation.text.Lists;
import com.idega.presentation.text.Text;

/**
 * This class diplays the current value of an application property, localized if available.
 * You have to use the method setApplicationPropertyKey to make it display anything.
 * You can also use setDefaultValue to initilize a key that has not been set.
 * 
 * @author valdas
 *
 */
public class ApplicationPropertyViewer extends Block {
	
	private static final String SIDEBAR = "sidebar";
	private static final String TOOLBAR = "toolbar";
	private static final String BREADCRUMB = "breadcrumb";
	private static final String LIST_STYLE = "list-style-type: none; width: 100%";

	private String applicationPropertyKey = null;
	
	public ApplicationPropertyViewer() {}
	
	public void main(IWContext iwc) {
		if(applicationPropertyKey != null){
			String key = null;
			if (applicationPropertyKey.indexOf(ThemesConstants.THEMES_PROPERTY_START) == -1) {
				applicationPropertyKey = ThemesConstants.THEMES_PROPERTY_START + applicationPropertyKey;
			}
			
			String value = null;
			Locale locale = iwc.getCurrentLocale();
			if (locale != null) { // Getting value, based on Locale
				key = applicationPropertyKey + ThemesConstants.DOT + locale.getLanguage();
				value = iwc.getApplicationSettings().getProperty(key);
			}
			if (value == null) { // Didn't find localized value, getting default value
				key = applicationPropertyKey + ThemesConstants.THEMES_PROPERTY_END;
				value = iwc.getApplicationSettings().getProperty(key);
			}
			
			if (value == null) {
				return;
			}
			
			if (key.indexOf(ThemesConstants.THEMES_PROPERTY_START + SIDEBAR + ThemesConstants.DOT) != -1) { // Sidebar
				ICPage page = ThemesHelper.getInstance().getThemesService().getICPage(iwc.getCurrentIBPageID());
				if (page != null) {
					if (page.getWebDavUri() != null) {
						return;
					}
				}
				String[] values = value.split(ThemesConstants.COMMA);
				Lists list = new Lists();
				list.setStyleAttribute(LIST_STYLE);
				ListItem item = null;
				for (int i = 0; i < values.length; i++) {
					item = new ListItem();
					item.addText(values[i].trim());
					list.add(item);
				}
				this.add(list);
				return;
			}
			
			if (key.indexOf(ThemesConstants.THEMES_PROPERTY_START + TOOLBAR + ThemesConstants.DOT) != -1) { // Toolbar
				Link l = new Link();
				l.setId("current");
				l.setText(value);
				this.add(l);
				return;
			}
			
			if (key.indexOf(ThemesConstants.THEMES_PROPERTY_START + BREADCRUMB + ThemesConstants.DOT) != -1) { // Breadcrumb
				Link l = new Link();
				l.setText(value);
				this.add(l);
				return;
			}

			Text text = new Text(value); // Simple text
			this.add(text);
		}	
	}
	
	
	public void setApplicationPropertyKey(String key){
		this.applicationPropertyKey = key;
	}
	
	public void restoreState(FacesContext context, Object state) {
		Object values[] = (Object[]) state;
		super.restoreState(context, values[0]);
		this.applicationPropertyKey = (String) values[1];
	}

	public Object saveState(FacesContext context) {
		Object values[] = new Object[2];
		values[0] = super.saveState(context);
		values[1] = this.applicationPropertyKey;
		return values;
	}
}
