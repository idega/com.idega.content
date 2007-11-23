var THEME_ID = null;

var SCROLLER_IMAGE_WIDTH = 23;
var FRAME_CHANGE = 158;
var RESERVED_WIDTH = 280;
var SHOW_ELEMENT_TRANSITION_DURATION = 500;
var SET_DISPLAY_PROPERTY_ID = 0;
var GET_THEMES_ID = 0;

var SLIDER_SHOWED_FIRST_TIME = true;
var MODULES_SHOWN = false;
var IS_USER_ADMIN = false;

var KEYWORDS = null;

var IB_SOURCE_VIEW_CLASS = 'com.idega.builder.presentation.IBSourceView';

function setThemeForStyle(ID) {
	THEME_ID = ID;
}

function getThemeForStyle() {
	return THEME_ID;
}

function getScrollerImageWidth() {
	return SCROLLER_IMAGE_WIDTH;
}

function savePageInfo() {
	showLoadingMessage(getThemeSavingText());
	if (KEYWORDS == null) {
		ThemesEngine.getPageInfoElements(getPageInfoElementsCallback);
	}
	else {
		getPageInfoElementsCallback(KEYWORDS);
	}
}

function getPageInfoElementsCallback(allKeywords) {
	var pageId = getPageID();
	if (pageId == null || allKeywords == null) {
		closeLoadingMessage();
		return;
	}
	
	KEYWORDS = allKeywords;
	var keywords = new Array();
	var values = new Array();
	var element = null;
	var treeNode = null;
	for (var i = 0; i < allKeywords.length; i++) {
		element = $(allKeywords[i]);
		if (element != null) {
			keywords.push(allKeywords[i]);
			values.push(element.value);
			if (allKeywords[i] == 'pageTitle' && element.value != '') {
				treeNode = $(pageId + 'a');
				if (treeNode != null) {
					removeChildren(treeNode);
					treeNode.appendChild(document.createTextNode(element.value));
				}
			}
		}
	}
	ThemesEngine.savePageInfo(pageId, keywords, values, savePageInfoCallback);
}

function savePageInfoCallback(result) {
	if (result != null) {
		var pageUri = $('pageUri');
		if (pageUri != null) {
			pageUri.value = result;
		}
	}
	closeLoadingMessage();
	getPrewUrl(getPageID());
}

function showSlider(container) {
	resizeSlider();
	
	container = $(container);
	container.setStyle('position', 'absolute');
	container.setStyle('bottom', '5px');
	container.setStyle('left', RESERVED_WIDTH + 'px');
	container.setStyle('right', '5px');
	
	var showSlider = new Fx.Style(container, 'opacity', {duration: 1000});
	showSlider.start(0, 1);
	SET_DISPLAY_PROPERTY_ID = window.setTimeout("setDisplayPropertyToElement('"+container.id+"', 'block', null)", SHOW_ELEMENT_TRANSITION_DURATION);
	GET_THEMES_ID = window.setTimeout('getThemesSlider()', SHOW_ELEMENT_TRANSITION_DURATION + 50);
}

function getThemesSlider() {
	var slideToDefault = SLIDER_SHOWED_FIRST_TIME;
	SLIDER_SHOWED_FIRST_TIME = false;
	getThemes(null, true, slideToDefault);
	
	window.clearTimeout(GET_THEMES_ID);
}

function manageSlider(buttonID) {
	var container = $('themesSliderContainer');
	if (container == null) {
		return;
	}
	var button = $(buttonID);
	if (button == null) {
		return;
	}
	if (container.style.display == 'none') {
		button.setText(getHideThemesText());
		changeFrameHeight(-FRAME_CHANGE);
		showSlider(container);
	}
	else {
		hideThemesSliderInPages(container, button);
	}
}

function hideThemesSliderInPages(container, button) {
	if (container == null) {
		return false;
	}
	
	if ($(container).getStyle('display') != 'none') {
		removeStyleOptions();
		if (button != null) {
			button.setText(getShowThemesText());
		}
		var hideSlider = new Fx.Style(container, 'opacity', {duration: SHOW_ELEMENT_TRANSITION_DURATION});
		hideSlider.start(1, 0);
		SET_DISPLAY_PROPERTY_ID = window.setTimeout("setDisplayPropertyToElement('"+container.id+"', 'none', "+FRAME_CHANGE+")", SHOW_ELEMENT_TRANSITION_DURATION);
	}
}

function setDisplayPropertyToElement(id, property, frameChange) {
	if (id == null || property == null) {
		return false;
	}
	var element = $(id);
	if (element == null) {
		return false;;
	}
	
	element.setStyle('display', property);
	window.clearTimeout(SET_DISPLAY_PROPERTY_ID);
	
	if (frameChange != null) {
		changeFrameHeight(frameChange);
	}
}

function chooseStyle(themeID) {
	if (themeID == null) {
		return;
	}
	var theme = getTheme(themeID);
	if (theme != null) {
		theme.applyStyle = true;
		applyThemeForPage(themeID);
	}
}

function recallStyle(themeID) {
	if (themeID == null) {
		return;
	}
	var theme = getTheme(themeID);
	if (theme != null) {
		theme.applyStyle = false;
	}
}

function applyThemeForPage(themeID) {
	if (themeID == null) {
		return;
	}
	var theme = getTheme(themeID);
	if (theme != null) {
		if (theme.applyStyle) {
			theme.applyStyle = false;
			chooseOption(themeID);
		}
	}
}

function chooseOption(themeID) {
	var leftPosition = (getAbsoluteLeft(themeID + '_container') + 3);
	if (getTotalWidth() - (leftPosition + getImageWidth()) < 0) {
		return; // There is not enough space
	}
	
	setThemeForStyle(themeID);
	
	var div = $('chooseStyleLayer');
	var pageSpan = null;
	var siteSpan = null;
	if (div == null) {
		div = new Element('div');
		div.style.display = 'none';
		div.setAttribute('id', 'chooseStyleLayer');
		div.className = 'themeChooseStyle';
		
		var divp = new Element('div');
		divp.className = 'themeChooseStyleText';
		divp.setAttribute('title', getStyleForCurrentPage());
		divp.setAttribute('alt', getStyleForCurrentPage());
		pageSpan = new Element('span');
		pageSpan.setAttribute('id', 'pageStyle');
		pageSpan.appendChild(document.createTextNode(getChooseStyleForPage()));
		divp.appendChild(pageSpan);
	
		var divs = new Element('div');
		divs.className = 'themeChooseStyleText';
		divs.setAttribute('title', getStyleForSite());
		divs.setAttribute('alt', getStyleForSite());
		siteSpan = new Element('span');
		siteSpan.setAttribute('id', 'siteStyle');
		siteSpan.appendChild(document.createTextNode(getChooseStyleForSite()));
		divs.appendChild(siteSpan);
		
		div.appendChild(divp);
		div.appendChild(divs);
		document.body.appendChild(div);
		
		var setStyleForPageFunction = function() {
			setStyle(true);
		};
		var setStyleForSiteFunction = function() {
			setStyle(false);
		};
		pageSpan.addEvent('click', setStyleForPageFunction);
		siteSpan.addEvent('click', setStyleForSiteFunction);
		div.addEvent('click', removeStyleOptions);
	}
	div.style.left = leftPosition + 'px';
	div.style.top = (getAbsoluteTop(themeID + '_container') + 3) + 'px';
	div.style.display = 'block';
}

function setStyle(isPage) {
	removeStyleOptions();
	if (getThemeForStyle() == null) {
		return;
	}
	
	var pageId = getPageID();
	
	if (isPage && pageId == null) {
		return;
	}
	showLoadingMessage(getApplyingStyleText());
	setNewStyleToElements('usedThemeName', 'themeName');
	setNewStyleForSelectedElement(getThemeForStyle() + '_themeNameContainer', 'themeName usedThemeName');
	ThemesEngine.setSelectedStyle(getThemeForStyle(), pageId, isPage, {
		callback: function(result) {
			setStyleCallback(result, pageId);
		}
	});
}

function setStyleCallback(result, pageId) {
	closeLoadingMessage();
	getPrewUrl(pageId);
}

function resizeSlider() {
	var themesTicker = $('themesTickerContainer');
	var container = $('themesSliderContainer');
	if (themesTicker == null || container == null) {
		return;
	}

	var available = getTotalWidth() - RESERVED_WIDTH - 6;
	if (available > 0) {
		container.setStyle('width', available + 'px');
		themesTicker.setStyle('width', (available - 50) + 'px');
	}
}

function setButtonText(id, text) {
	var button = $(id);
	if (button != null) {
		button.setText(text);
	}
}

function newPages(containerId, buttonId, buttonText, positionFromLeft) {
	var container = $(containerId);
	if (container == null) {
		return false;
	}
	
	var containerIsOpened = false;
	var displayValue = container.getStyle('display');
	if (displayValue != null) {
		containerIsOpened = displayValue == 'block';
	}
	if (containerIsOpened) {
		closeNewPage(container, buttonId, buttonText);
	}
	else {
		container.setStyle('left', positionFromLeft);
		container.setStyle('visibility', 'hidden');
		container.setStyle('display', 'block');
		var showNewPage = new Fx.Style(container, 'opacity', {duration: SHOW_ELEMENT_TRANSITION_DURATION});
		showNewPage.start(0, 1);
	}
}

function changeFrameHeight(change) {
	var container = $('pagePreviewContainer');
	if (container == null) {
		return;
	}
	var current = container.getStyle('height');
	if (current == null) {
		return;
	}
	
	var temp = current.split('px');
	if (temp == null) {
		return;
	}
	
	var oldHeight = temp[0];
	oldHeight++;
	oldHeight--;
	var newHeight = oldHeight + change;
	
	var changeSize = new Fx.Style(container, 'height', {duration: SHOW_ELEMENT_TRANSITION_DURATION}, {wait:true});
	changeSize.start(oldHeight, newHeight);
}

function resizeFrame() {
	var container = $('pagePreviewContainer');
	if (container == null) {
		return;
	}
	
	//	Width
	var availableWidth = getTotalWidth() - RESERVED_WIDTH - 6; 
	if (availableWidth > 0) {
		container.setStyle('width', availableWidth + 'px');
	}
	
	//	Height
	var availableHeight = getTotalHeight() - 148;
	if (availableHeight > 0) {
		container.setStyle('height', availableHeight + 'px');
	}
}

function getPageInfoValues() {
	if (KEYWORDS == null) {
		ThemesEngine.getPageInfoElements(getAvailableElements);
	}
	else {
		getAvailableElements(KEYWORDS);
	}
}


function getAvailableElements(allKeywords) {
	if (allKeywords == null) {
		return;
	}
	KEYWORDS = allKeywords;
	ThemesEngine.getPageInfoValues(getPageID(), allKeywords, showPageInfoValues);
}

function showPageInfoValues(values) {
	if (values == null) {
		if (KEYWORDS != null) {
			for (var i = 0; i < KEYWORDS.length; i++) {
				element = $(KEYWORDS[i]);
				if (element != null) {
					element.value = '';
				}
			}
		}
		
		return false;
	}
	if (KEYWORDS.length != values.length) {
		return;
	}
	var element = null;
	for (var i = 0; i < KEYWORDS.length; i++) {
		element = $(KEYWORDS[i]);
		if (element != null) {
			element.value = values[i];
		}
	}
}

function isStartPage(pageID) {
	ThemesEngine.isStartPage(pageID, isStartPageCallback);
}

function isStartPageCallback(isStart) {
	var button = $('makeStartPage');
	if (button == null) {
		return;
	}
	button.disabled = isStart;
	if (isStart) {
		button.value = getStartPageText();
	}
	else {
		button.value = getMakeStartPageText();
		button.title = getMakeThisPageStartPageText();
	}
}

function makePageAsStartPage() {
	showLoadingMessage(getChangingStructureText());
	ThemesEngine.setAsStartPage(getPageID(), setAsStartPageCallback);
}

function setAsStartPageCallback(result) {
	closeAllLoadingMessages();
}

function closeNewPage(container, buttonId, buttonText) {
	setButtonText(buttonId, buttonText);
	
	if (container != null) {
		var hideNewPage = new Fx.Style(container, 'opacity', {duration: SHOW_ELEMENT_TRANSITION_DURATION});
		hideNewPage.start(1, 0);
		SET_DISPLAY_PROPERTY_ID = window.setTimeout("setDisplayPropertyToElement('"+container.id+"', 'none', null)", SHOW_ELEMENT_TRANSITION_DURATION);
	}
}

function managePageInfoComponents() {
	removeStyleOptions();
}

function initializePages() {
	initScript(true, false, false);
	getGlobalPageId();

	resizeFrame();
	isStartPage(getPageID());
	checkIfNotEmptySiteTree(ALL_CURRENT_SITE_STRUCTURE_TREE_ID);
	document.addEvent('click', managePageInfoComponents);
	
	resizeTreeContainerInThemes(RESERVED_HEIGHT_FOR_PAGES);
	
	ThemesEngine.isUserAdmin({
		callback: function(isAdmin) {
			IS_USER_ADMIN = isAdmin;
		}
	});
	
	BuilderService.getClassNameForSourceView({
		callback: function(className) {
			IB_SOURCE_VIEW_CLASS = className;
		}
	});
}

function registerPageInfoActions() {
	$$('a.newPageButtonStyleClass').each(
		function(element) {
			setHrefToVoidFunction(element);
			
			element.addEvent('click', function() {
				newPages('newPageContainer', 'newPageButton', getNewPageText(), element.getLeft());
			});
    	}
    );
    
    $$('a.newPagesButtonStyleClass').each(
		function(element) {
			setHrefToVoidFunction(element);
			
			element.addEvent('click', function() {
				newPages('newPagesContainer', 'newPagesButton', NEW_PAGES_TEXT, element.getLeft());
			});
    	}
    );
    
    $$('input.saveButtonStyleClass').each(
    	function(element) {
			element.addEvent('click', savePageInfo);
    	}
    );
    
	$$('a.showThemesButtonStyleClass').each(
		function(element) {
			setHrefToVoidFunction(element);
			
			var manageSliderFunction = function() {
				manageSlider(element.id);
			};
			element.addEvent('click', function() {
				if (!SHOW_SOURCE_PAGES) {
					manageSliderFunction();
				}
				
				return false;
			});
	   	}
	);
	
	$$('a.showPageModulesStyleClass').each(
		function(element) {
			setHrefToVoidFunction(element);
			
			var manageModulesBackgroundFunction = function() {
				manageModulesBackground(element);
			};
			element.addEvent('click', function() {
				if (SHOW_EDIT_PAGES) {
					manageModulesBackgroundFunction();
				}
			});
		}
	);
	
	$$('a.showEditPagesButtonStyleClass').each(
		function(button) {
			setHrefToVoidFunction(button);
			
			button.addEvent('click', function() {
				SHOW_SOURCE_PAGES = false;
				SHOW_EDIT_PAGES = true;
				
				$('showPreviewPagesButton').removeClass('activeButtonInPages');
				$('showSourcePagesButton').removeClass('activeButtonInPages');
				
				if (!button.hasClass('activeButtonInPages')) {
					button.addClass('activeButtonInPages');
				}
				
				getPrewUrl(getPageID());
			});
		}
	);
	
	$$('a.showPreviewPagesButtonStyleClass').each(
		function(button) {
			setHrefToVoidFunction(button);
			
			button.addEvent('click', function() {
				SHOW_SOURCE_PAGES = false;
				SHOW_EDIT_PAGES = false;
				
				if (SHOW_SOURCE_PAGES) {
					hideThemesSliderInPages($('themesSliderContainer'), $('showThemesButton'));
				}
				
				$('showEditPagesButton').removeClass('activeButtonInPages');
				$('showSourcePagesButton').removeClass('activeButtonInPages');
				
				if (!button.hasClass('activeButtonInPages')) {
					button.addClass('activeButtonInPages');
				}
				
				getPrewUrl(getPageID());
			});
		}
	);
	
	$$('a.showSourcePagesButtonStyleClass').each(
		function(button) {
			setHrefToVoidFunction(button);
			
			button.addEvent('click', function() {
				if (IS_USER_ADMIN) {
					SHOW_SOURCE_PAGES = true;
					SHOW_EDIT_PAGES = false;
					
					if (SHOW_SOURCE_PAGES) {
						hideThemesSliderInPages($('themesSliderContainer'), $('showThemesButton'));
					}
					
					$('showEditPagesButton').removeClass('activeButtonInPages');
					$('showPreviewPagesButton').removeClass('activeButtonInPages');
					
					if (!button.hasClass('activeButtonInPages')) {
						button.addClass('activeButtonInPages');
					}
					
					getPrewUrl(getPageID());
				}
			});
		}
	);
	
	$$('img.closeNewPageOrPagesStyle').each(
		function(image) {
			image.addEvent('click', function() {
				if (image.getProperty('id') == 'closeNewPagesContainer') {
					closeNewPage($('newPagesContainer'), 'newPagesButton', NEW_PAGES_TEXT);
				}
				else {
					closeNewPage($('newPageContainer'), 'newPageButton', getNewPageText());
				}
			});
		}
	);
	
	registerActionsForSiteTree();
	boldCurrentTreeElement();
}

function setHrefToVoidFunction(element) {
	element.setProperty('href', 'javascript:void(0)');
}

function manageModulesBackground(element) {
	if (element == null) {
		return;
	}
	var frameObject = window.frames['treePages'];
	if (frameObject == null) {
		element.disabled = true;
		return;
	}
	var frameDocument = frameObject.document;
	if (frameDocument == null) {
		element.disabled = true;
		return;
	}
	element.disabled = false;
	
	if (MODULES_SHOWN) {
		hideOldLabels(frameDocument);
	}
	
	var elements = getElementsByClassName(frameDocument, 'div', 'moduleContainer');
	if (elements == null) {
		return;
	}
	var module = null;
	for (var i = 0; i < elements.length; i++) {
		module = elements[i];
		if (MODULES_SHOWN) {
			module.removeAttribute('style');
		}
		else {
			module.setAttribute('style', 'background-color: #FFFF99;');
			showAllComponentsLabels(module);
		}
	}
	
	if (MODULES_SHOWN) {
		element.value = getShowModuleText();
		MODULES_SHOWN = false;
	}
	else {
		element.value = getHideModulesText();
		MODULES_SHOWN = true;
	}
}