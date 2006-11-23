package com.idega.content.themes.presentation;

import java.util.Locale;

import com.idega.presentation.Block;
import com.idega.presentation.IWContext;

/**
 * This class diplays the current value of an application property, localized if available.
 * You have to use the method setApplicationPropertyKey to make it display anything.
 * You can also use setDefaultValue to initilize a key that has not been set.
 * 
 * @author valdas
 *
 */
public class ApplicationPropertyViewer extends Block {

	private String applicationPropertyKey = null;
	
	public ApplicationPropertyViewer(){}
	
	public void main(IWContext iwc){
		
		if(applicationPropertyKey!=null){
			Locale locale = iwc.getCurrentLocale();
			
			//also need the current local....
			Object value = iwc.getApplicationSettings().getProperty(applicationPropertyKey+"."+locale.getLanguage());
			
			this.add(value);
			
			
			
		}
		
	}
	
	
	public void setApplicationPropertyKey(String key){
		this.applicationPropertyKey = key;
	}
	
	//todo implement store and restore for jsf compatability
	
	
	
	
}
