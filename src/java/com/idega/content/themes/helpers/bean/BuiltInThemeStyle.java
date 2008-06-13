package com.idega.content.themes.helpers.bean;

import java.util.HashMap;
import java.util.Map;

public class BuiltInThemeStyle {
	
	private String id = null;
	private String name = null;
	private String uri = null;
	
	private Map<String, String> colours = null;
	private Map<String, String> variations = null;
	
	public BuiltInThemeStyle(String id) {
		this.id = id;
	}
	
	public BuiltInThemeStyle(String id, String name, String uri) {
		this(id);
		
		this.name = name;
		this.uri = uri;
	}
	
	public BuiltInThemeStyle(String id, BuiltInThemeStyle style) {
		this(id, style.getName(), style.getUri());
		
		Map<String, String> colours = style.getColours();
		this.colours = new HashMap<String, String>();
		if (colours != null && !colours.isEmpty()) {
			this.colours.putAll(colours);
		}
		
		Map<String, String> variations = style.getVariations();
		this.variations = new HashMap<String, String>();
		if (variations != null && !variations.isEmpty()) {
			this.variations.putAll(variations);
		}
	}
	
	public String getId() {
		return id;
	}
	
	public String getName() {
		return name;
	}
	
	public void setName(String name) {
		this.name = name;
	}
	
	public String getUri() {
		return uri;
	}

	public void setUri(String uri) {
		this.uri = uri;
	}

	public Map<String, String> getColours() {
		return colours;
	}
	
	public void setColours(Map<String, String> colours) {
		this.colours = colours;
	}
	
	public Map<String, String> getVariations() {
		return variations;
	}
	
	public void setVariations(Map<String, String> variations) {
		this.variations = variations;
	}

}
