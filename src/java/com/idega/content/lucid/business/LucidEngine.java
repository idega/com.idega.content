package com.idega.content.lucid.business;

import java.io.Serializable;
import java.util.List;

public interface LucidEngine extends Serializable {

	public String getJavaScriptResources();
	
	public String getStyleSheetResources();
	
	public List<String> getPermissionWindowResources();
	
	public List<String> getPropertiesWindowResources();
	
	public boolean isContentEditor();
	
}
