package com.idega.content.themes.presentation;

import java.rmi.RemoteException;
import java.util.Arrays;

import com.idega.block.web2.business.Web2Business;
import com.idega.content.business.ContentConstants;
import com.idega.idegaweb.IWBundle;
import com.idega.idegaweb.IWResourceBundle;
import com.idega.presentation.Block;
import com.idega.presentation.IWContext;
import com.idega.presentation.Image;
import com.idega.presentation.Layer;
import com.idega.util.CoreConstants;
import com.idega.util.CoreUtil;
import com.idega.util.PresentationUtil;
import com.idega.util.StringUtil;
import com.idega.util.expression.ELUtil;

public class ThemesSliderViewer extends Block {
	
	private String mainId = "themesSliderContainer";
	private String mainStyleClass = "themesSlider";
	
	private boolean hiddenOnLoad = false;
	
	private String initAction;
	
	@Override
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
		leftScroller.add(getScrollImage(bundle.getVirtualPathWithFileNameString("images/left.png"),
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
		rightScroller.add(getScrollImage(bundle.getVirtualPathWithFileNameString("images/right.png"),
				iwrb.getLocalizedString("scroll_right", "Scroll to right"), "rightScroller"));
		container.add(rightScroller);
		
		//	Resources
		Web2Business web2 = ELUtil.getInstance().getBean(Web2Business.SPRING_BEAN_IDENTIFIER);
		try {
			PresentationUtil.addJavaScriptSourcesLinesToHeader(iwc, Arrays.asList(
					web2.getBundleURIToMootoolsLib(),
					web2.getReflectionForMootoolsScriptFilePath(),
					
					web2.getBundleURIToJQueryLib(),
					web2.getBundleUriToContextMenuScript(),
					
					CoreConstants.DWR_ENGINE_SCRIPT,
					"/dwr/interface/ThemesEngine.js",
					"/dwr/interface/LucidEngine.js",
					
					bundle.getVirtualPathWithFileNameString("javascript/ThemesHelper.js"),
					bundle.getVirtualPathWithFileNameString("javascript/ThemesManagerHelper.js"),
					bundle.getVirtualPathWithFileNameString("javascript/ThemesSliderHelper.js")
			));
		} catch (RemoteException e) {
			e.printStackTrace();
		}
		
		PresentationUtil.addStyleSheetToHeader(iwc, bundle.getVirtualPathWithFileNameString("style/content.css"));
		
		if (!StringUtil.isEmpty(initAction)) {
			String action = getInitAction();
			if (!CoreUtil.isSingleComponentRenderingProcess(iwc)) {
				action = new StringBuilder("jQuery(window).load(function() {").append(action).append("});").toString();
			}
			PresentationUtil.addJavaScriptActionToBody(iwc, action);
		}
		
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
	
	public String getInitAction() {
		return initAction;
	}

	public void setInitAction(String initAction) {
		this.initAction = initAction;
	}

	@Override
	public String getBundleIdentifier() {
		return ContentConstants.IW_BUNDLE_IDENTIFIER;
	}
}
