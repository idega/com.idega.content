package com.idega.content.lucid.business;

import java.io.Serializable;
import java.util.List;

import javax.faces.model.SelectItem;

public interface LucidEngine extends Serializable {

	public String getJavaScriptResources();
	
	public String getStyleSheetResources();
	
	public List<String> getPermissionWindowResources();
	
	public List<String> getPropertiesWindowResources();
	
	public boolean isContentEditor();
	
	public boolean setLocale(String locale);
	
	public List<SelectItem> getAvailableLocales();
	
	public String getCurrentLocaleValue();
	
}
