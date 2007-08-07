package com.idega.content.themes.presentation;

import com.idega.content.business.ContentConstants;
import com.idega.idegaweb.IWBundle;
import com.idega.idegaweb.IWResourceBundle;
import com.idega.presentation.Block;
import com.idega.presentation.IWContext;
import com.idega.presentation.Image;
import com.idega.presentation.Layer;

public class ThemesSliderViewer extends Block {
	
	private String mainId = "themesSliderContainer";
	private String mainStyleClass = "themesSlider";
	
	private boolean hiddenOnLoad = false;
	
	public void main(IWContext iwc) {
		IWBundle bundle = getBundle(iwc);
		IWResourceBundle iwrb = getResourceBundle(iwc);
		
		//	Main container
		Layer container = new Layer();
		container.setId(getMainId());
		container.setStyleClass(getMainStyleClass());
		if (hiddenOnLoad) {
			container.setStyleAttribute("display", "none");
		}
		
		//	Left scroller
		Layer leftScroller = new Layer();
		leftScroller.setId("leftScrollerContainer");
		leftScroller.setStyleClass("themeScroller");
		leftScroller.add(getScrollImage(bundle.getVirtualPathWithFileNameString("images/left.gif"),
				iwrb.getLocalizedString("scroll_left", "Scroll to left"), "leftScroller"));
		container.add(leftScroller);
		
		//	Ticker
		Layer tickerContainer = new Layer();
		tickerContainer.setId("themesTickerContainer");
		tickerContainer.setStyleClass("themesTicker");
		Layer ticker = new Layer();
		ticker.setId("themes");
		ticker.setStyleClass("multiImageGallery");
		ticker.setStyleAttribute("left", "0px");
		tickerContainer.add(ticker);
		container.add(tickerContainer);
		
		//	Right scroller
		Layer rightScroller = new Layer();
		rightScroller.setId("rightScrollerContainer");
		rightScroller.setStyleClass("themeScroller rightThemeScroller");
		rightScroller.add(getScrollImage(bundle.getVirtualPathWithFileNameString("images/right.gif"),
				iwrb.getLocalizedString("scroll_right", "Scroll to right"), "rightScroller"));
		container.add(rightScroller);
		
		add(container);
	}
	
	private Image getScrollImage(String uri, String name, String id) {
		Image image = new Image(uri, name);
		image.setId(id);
		image.setOnClick(new StringBuffer("scroll('").append(image.getId()).append("');").toString());
		return image;
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
	
	public boolean isHiddenOnLoad() {
		return hiddenOnLoad;
	}

	public void setHiddenOnLoad(boolean hiddenOnLoad) {
		this.hiddenOnLoad = hiddenOnLoad;
	}

	public String getBundleIdentifier() {
		return ContentConstants.IW_BUNDLE_IDENTIFIER;
	}
}
