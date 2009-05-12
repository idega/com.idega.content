package com.idega.content.themes.presentation;

import javax.faces.component.UIComponent;
import javax.faces.webapp.UIComponentELTag;

public class ThemesSliderViewerTag extends UIComponentELTag {

	private String mainId = "themesSliderContainer";
	private String mainStyleClass = "themesSlider";
	
	private boolean hiddenOnLoad = false;
	
	@Override
	public String getComponentType() {
		return "ThemesSliderViewer";
	}

	@Override
	public String getRendererType() {
		return null;
	}
	
	@Override
	protected void setProperties(UIComponent component) {
		if (component instanceof ThemesSliderViewer) {
			super.setProperties(component);
			
			ThemesSliderViewer viewer = (ThemesSliderViewer) component;
			viewer.setMainId(getMainId());
			viewer.setMainStyleClass(getMainStyleClass());
			viewer.setHiddenOnLoad(isHiddenOnLoad());
		}
	}

	public boolean isHiddenOnLoad() {
		return hiddenOnLoad;
	}

	public void setHiddenOnLoad(boolean hiddenOnLoad) {
		this.hiddenOnLoad = hiddenOnLoad;
	}

	public String getMainId() {
		return mainId;
	}

	public void setMainId(String mainId) {
		this.mainId = mainId;
	}

	public String getMainStyleClass() {
		return mainStyleClass;
	}

	public void setMainStyleClass(String mainStyleClass) {
		this.mainStyleClass = mainStyleClass;
	}

}
