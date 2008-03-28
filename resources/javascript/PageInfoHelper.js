var THEME_ID = null;

var SCROLLER_IMAGE_WIDTH = 23;
var FRAME_CHANGE = 158;
var RESERVED_WIDTH = 280;
var SHOW_ELEMENT_TRANSITION_DURATION = 500;
var SET_DISPLAY_PROPERTY_ID = 0;
var GET_THEMES_ID = 0;

var SLIDER_SHOWED_FIRST_TIME = true;
var MODULES_SHOWN = true;

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

function setValueToHiddentPageInfoElement(input) {
	var hiddenInputId = input.getProperty('radioButtonCode');
	if (hiddenInputId == null) {
		return false;
	}
	
	var hiddenInput = $(hiddenInputId);
	if (hiddenInput == null) {
		return false;
	}
	
	var inputValue = input.getProperty('value');
	if (inputValue == null || inputValue == '') {
		inputValue = '-1';
	}
	
	hiddenInput.setProperty('value', inputValue);
}

function savePageInfoWithRadioButtonValue(id) {
	var radio = $(id);
	if (radio == null) {
		return false;
	}
	
	setValueToHiddentPageInfoElement(radio);
	
	savePageInfo();
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
	var needReload = false;
	for (var i = 0; i < allKeywords.length; i++) {
		element = $(allKeywords[i]);
		if (element != null) {
			keywords.push(allKeywords[i]);
			var pageInfoValue = element.getProperty('value');
			values.push(pageInfoValue);
			if (allKeywords[i] == 'pageTitle' && pageInfoValue != '') {
				treeNode = $(pageId);
				if (treeNode == null) {
					needReload = true;
				}
				else {
					var nodeLinks = treeNode.getElements('a[class=pageTreeNames]');
					if (nodeLinks == null || nodeLinks.length == 0) {
						needReload = true;
					}
					else {
						var nodeLink = null;
						for (var j = 0; j < nodeLinks.length; j++) {
							nodeLink = nodeLinks[j];
							
							nodeLink.empty();
							nodeLink.setText(pageInfoValue);
						}
					}
				}
			}
		}
	}
	ThemesEngine.savePageInfo(pageId, keywords, values, {
		callback: function(result) {
			if (needReload) {
				reloadPage();
				return;
			}
			
			if (result != null) {
				var pageUri = $('pageUri');
				if (pageUri != null) {
					pageUri.value = result;
				}
			}
			
			closeLoadingMessage();
			getPrewUrl(getPageID());
		}
	});
}

function showSlider(container) {
	resizeSlider();
	
	container = $(container);
	container.setStyle('position', 'absolute');
	container.setStyle('bottom', '5px');
	container.setStyle('left', RESERVED_WIDTH + 'px');
	container.setStyle('right', '5px');
	
	var showSliderEffect = new Fx.Style(container, 'opacity', {duration: 1000, onComplete: function() {
		setDisplayPropertyToElement(container.id, 'block', null);
		getThemesSlider();
	}});
	showSliderEffect.start(0, 1);
}

function getThemesSlider() {
	var slideToDefault = SLIDER_SHOWED_FIRST_TIME;
	SLIDER_SHOWED_FIRST_TIME = false;
	getThemes(null, true, slideToDefault);
	
	if (GET_THEMES_ID != null) {
		window.clearTimeout(GET_THEMES_ID);
		GET_THEMES_ID = null;
	}
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
	if (container.getStyle('display') == 'none') {
		button.addClass('active');
		changeFrameHeight(-FRAME_CHANGE);
		showSlider(container);
	}
	else {
		button.removeClass('active');
		hideThemesSliderInPages(container, button);
	}
}

function hideThemesSliderInPages(container, button) {
	if (container == null) {
		return false;
	}
	
	if ($(container).getStyle('display') != 'none') {
		removeStyleOptions(null);
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
	if (SET_DISPLAY_PROPERTY_ID != null) {
		window.clearTimeout(SET_DISPLAY_PROPERTY_ID);
		SET_DISPLAY_PROPERTY_ID = null;
	}
	
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
	var pageAndChildrenSpan = null;
	if (div == null) {
		div = new Element('div');
		div.setStyle('opacity', '0');
		div.setProperty('id', 'chooseStyleLayer');
		div.addClass('themeChooseStyle');
		
		var container = new Element('div');
		container.addClass('themesButtonContainer');
		
		var left = new Element('div');
		left.addClass('left');
		container.appendChild(left);
		
		var right = new Element('div');
		right.addClass('right');
		container.appendChild(right);
		
		var divp = new Element('div');
		divp.addClass('themeChooseStyleText');
		divp.addClass('applyPage');
		divp.setProperty('title', getStyleForCurrentPage());
		divp.setProperty('alt', getStyleForCurrentPage());
		pageSpan = new Element('span');
		pageSpan.setProperty('id', 'pageStyle');
		divp.appendChild(pageSpan);
	
		var divs = new Element('div');
		divs.addClass('themeChooseStyleText');
		divs.addClass('applySite');
		divs.setProperty('title', getStyleForSite());
		divs.setProperty('alt', getStyleForSite());
		siteSpan = new Element('span');
		siteSpan.setProperty('id', 'siteStyle');
		divs.appendChild(siteSpan);
		
		var divd = new Element('div');
		divd.addClass('themeChooseStyleText');
		divd.addClass('applyPageAndChildren');
		divd.setProperty('title', getStyleForPageAndChildren());
		divd.setProperty('alt', getStyleForPageAndChildren());
		pageAndChildrenSpan = new Element('span');
		pageAndChildrenSpan.setProperty('id', 'pageAndChildrenStyle');
		divd.appendChild(pageAndChildrenSpan);
		
		var themeChildrenTemplatesContainer = new Element('div');
		themeChildrenTemplatesContainer.setProperty('id', 'themeTemplateChildrenContainer');
		themeChildrenTemplatesContainer.addClass('themeTemplateChildrenContainerAsStackStyle');
		document.body.appendChild(themeChildrenTemplatesContainer);
		
		right.appendChild(divp);
		right.appendChild(divd);
		right.appendChild(divs);
		
		div.appendChild(container);
		document.body.appendChild(div);
		
		var setStyleForPageFunction = function() {
			TEMPLATE_ID = null;
			setTemplateForPageOrPages(true, 0);
		};
		var setStyleForPageAndChildren = function() {
			TEMPLATE_ID = null;
			setTemplateForPageOrPages(true, 1);
		}
		var setStyleForSiteFunction = function() {
			TEMPLATE_ID = null;
			setTemplateForPageOrPages(false, 2);
		};
		
		pageSpan.addEvent('click', setStyleForPageFunction);
		siteSpan.addEvent('click', setStyleForSiteFunction);
		pageAndChildrenSpan.addEvent('click', setStyleForPageAndChildren);
		div.addEvent('click', function(e) {
			e = new Event(e);
			removeStyleOptions(e);
		});
	}
	
	var topPosition = getAbsoluteTop(themeID + '_container') - 37;
	
	$('themeTemplateChildrenContainer').setStyles({
		opacity: '0',
		left: leftPosition + 'px'
	});
	$('themeTemplateChildrenContainer').setProperty('themeid', themeID);
	$('themeTemplateChildrenContainer').setProperty('initialtopposition', topPosition);
	
	var theme = getTheme(themeID);
	if (theme != null && theme.children != null && theme.children.length > 0) {
		getChildTemplatesForThisTheme();
	}
	
	div.setStyle('left', leftPosition + 'px');
	div.setStyle('top', topPosition + 'px');
	var showSelectStyle = new Fx.Style(div, 'opacity', {duration: 250});
	showSelectStyle.start(0, 1);
}

function getChildTemplatesForThisTheme() {
	var stackContainer = $('themeTemplateChildrenContainer');
	if (stackContainer == null) {
		return false;
	}
	
	var theme = getTheme(stackContainer.getProperty('themeid'));
	if (theme == null) {
		return false;
	}
	
	stackContainer.empty();
	
	var listInStackContainer = new Element('ul');
	listInStackContainer.addClass('templatesListInStackContainer');
	listInStackContainer.injectInside(stackContainer);
	
	var allChildren = theme.children;
	var templateId = null;
	for (var i = 0; i < allChildren.length; i++) {
		templateId = allChildren[i].id;
		
		var childTemplateContainer = new Element('li');
		childTemplateContainer.addClass('themeChildInStackContainerStyle');
		if (i == 0) {
			childTemplateContainer.addClass('firstChild');
		}
		else if (i + 1 == allChildren.length) {
			childTemplateContainer.addClass('lastChild');
		}
		var span = new Element('span');
		span.appendText(allChildren[i].name);
		span.injectInside(childTemplateContainer);
		
		var container = new Element('div');
		container.addClass('container');
		container.injectInside(childTemplateContainer);
		
		var applyStyleToPageLink = new Element('a');
		applyStyleToPageLink.setProperty('href', 'javascript:void(0)');
		applyStyleToPageLink.setProperty('templateid', templateId);
		applyStyleToPageLink.addClass('applyPage');
		applyStyleToPageLink.setProperty('title', getStyleForCurrentPage());
		applyStyleToPageLink.addEvent('click', function() {
			TEMPLATE_ID = $(this).getProperty('templateid');
			setTemplateForPageOrPages(true, 0);
		});
		applyStyleToPageLink.injectInside(container);
		
		var applyStyleToPageAndChildrenLink = new Element('a');
		applyStyleToPageAndChildrenLink.setProperty('href', 'javascript:void(0)');
		applyStyleToPageAndChildrenLink.setProperty('templateid', templateId);
		applyStyleToPageAndChildrenLink.addClass('applyPageAndChildren');
		applyStyleToPageAndChildrenLink.setProperty('title', getStyleForPageAndChildren());
		applyStyleToPageAndChildrenLink.addEvent('click', function() {
			TEMPLATE_ID = $(this).getProperty('templateid');
			setTemplateForPageOrPages(true, 1);
		});
		applyStyleToPageAndChildrenLink.injectInside(container);
		
		var applyStyleToSiteLink = new Element('a');
		applyStyleToSiteLink.setProperty('href', 'javascript:void(0)');
		applyStyleToSiteLink.setProperty('templateid', templateId);
		applyStyleToSiteLink.addClass('applySite');
		applyStyleToSiteLink.setProperty('title', getStyleForSite());
		applyStyleToSiteLink.addEvent('click', function() {
			TEMPLATE_ID = $(this).getProperty('templateid');
			setTemplateForPageOrPages(false, 2);
		});
		applyStyleToSiteLink.injectInside(container);
		
		childTemplateContainer.injectInside(listInStackContainer);
	}
	
	var initialTopPosition = stackContainer.getProperty('initialtopposition');
	var currentSize = stackContainer.getSize().size.y;
	stackContainer.setStyle('top', (initialTopPosition - currentSize) + 'px');
	
	var showStackContainer = new Fx.Style(stackContainer, 'opacity', {duration: 250});
	showStackContainer.start(0, 1);
}

function setTemplateForPageOrPages(isPage, type) {
	removeStyleOptions(null);
	if (getThemeForStyle() == null) {
		return;
	}
	
	var pageId = getPageID();
	if (isPage) {
		if (pageId == null) {
			return;
		}
	}
	else {
		pageId = null;
	}
	
	if (type > 0) {
		var confirmed = window.confirm(ARE_YOU_SURE_YOU_WANT_APPLY_THIS_TEMPLATE);
		if (!confirmed) {
			return false;
		}
	}
	
	showLoadingMessage(getApplyingStyleText());
	ThemesEngine.setSelectedStyle(getThemeForStyle(), pageId, type, TEMPLATE_ID, {
		callback: function(result) {
			closeAllLoadingMessages();
			
			//	Error
			if (!result) {
				if (!USER_IS_CONTENT_EDITOR) {
					alert(INSUFFICIENT_RIGHTS_FOR_ACTION_IN_LUCID);
				}
				return false;
			}
			
			//	OK
			setNewStyleToElements('usedThemeName', 'themeName');
			setNewStyleForSelectedElement(getThemeForStyle() + '_themeNameContainer', 'themeName usedThemeName');
			
			WORKING_WITH_TEMPLATE = false;
			getPrewUrl(getPageID());
		}
	});
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
	
	var pageId = getPageID();
	ThemesEngine.getRenderedPageInfo(pageId, 'customizePage', 'pageInfoStyle_accordion', {
		callback: function(component) {
			var container = $('pageInfoToggle');
			container.empty();
			
			insertNodesToContainer(component, container);
			registerPageInfoActions();
			
			ThemesEngine.getPageInfoValues(pageId, allKeywords, showPageInfoValues);	
		}
	});
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
		if (element == null || element.getProperty('type') == 'hidden') {
			element = $(KEYWORDS[i] + '_' + values[i]);
			if (element != null && values[i] != null && values[i] != '') {
				element.setProperty('checked', true);
				element.checked = true;
			}
		}
		else {
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

function managePageInfoComponents(e) {
	removeStyleOptions(e);
}

function initializePages() {
	initScript(true, false, false);
	getGlobalPageId();

	resizeFrame();
	isStartPage(getPageID());
	checkIfNotEmptySiteTree(ALL_CURRENT_SITE_STRUCTURE_TREE_ID);
	document.addEvent('click', function(e) {
		e = new Event(e);
		managePageInfoComponents(e);
	});
	
	resizeTreeContainerInThemes(RESERVED_HEIGHT_FOR_PAGES);
		
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
			
			element.removeEvents('click');
			element.addEvent('click', function() {
				newPages('newPageContainer', 'newPageButton', getNewPageText(), element.getLeft());
			});
    	}
    );
    
    $$('a.newPagesButtonStyleClass').each(
		function(element) {
			setHrefToVoidFunction(element);
			
			element.removeEvents('click');
			element.addEvent('click', function() {
				newPages('newPagesContainer', 'newPagesButton', NEW_PAGES_TEXT, element.getLeft());
			});
    	}
    );
    
    $$('input.saveButtonStyleClass').each(
    	function(element) {
    		element.removeEvents('click');
			element.addEvent('click', savePageInfo);
    	}
    );
    
	$$('a.showThemesButtonStyleClass').each(
		function(element) {
			setHrefToVoidFunction(element);
			
			element.removeEvents('click');
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
			
			element.removeEvents('click');
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
			
			button.removeEvents('click');
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
			
			button.removeEvents('click');
			button.addEvent('click', function() {
				SHOW_SOURCE_PAGES = false;
				SHOW_EDIT_PAGES = false;
				
				if (SHOW_SOURCE_PAGES) {
					hideThemesSliderInPages($('themesSliderContainer'), $('showThemesButton'));
				}
				
				MODULES_SHOWN = true;
				$('showPageModules').removeClass('active');
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
			
			button.removeEvents('click');
			button.addEvent('click', function() {
				SHOW_SOURCE_PAGES = true;
				SHOW_EDIT_PAGES = false;
				
				if (SHOW_SOURCE_PAGES) {
					hideThemesSliderInPages($('themesSliderContainer'), $('showThemesButton'));
				}
				
				MODULES_SHOWN = true;
				$('showPageModules').removeClass('active');
				$('showThemesButton').removeClass('active');
				$('showEditPagesButton').removeClass('activeButtonInPages');
				$('showPreviewPagesButton').removeClass('activeButtonInPages');
			
				if (!button.hasClass('activeButtonInPages')) {
					button.addClass('activeButtonInPages');
				}
				
				getPrewUrl(getPageID());
			});
		}
	);
	
	$$('img.closeNewPageOrPagesStyle').each(
		function(image) {
			image.removeEvents('click');
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
	
	MODULES_SHOWN = !MODULES_SHOWN;
	
	if (MODULES_SHOWN) {
		showAllComponentsLabels(frameDocument);
	}
	else {
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
			showModuleContainerTop($(module));
			showAllComponentsLabels(module);
			manageComponentInfoImageVisibility($(module), 'visible');
		}
		else {
			hideModuleContainerTop($(module));
			manageComponentInfoImageVisibility($(module), 'hidden');
		}
	}
	
	if (MODULES_SHOWN) {
		element.removeClass('active');
	}
	else {
		element.addClass('active');
	}
}

function registerActionsForTemplatesInLucid() {
	$$('a.templateNameInLucidTemplatesTreeStyle').each(
		function(element) {
			element.removeEvents('click');
			
			element.addEvent('click', function() {
				var templateId = element.getProperty('templateid');
				if (templateId == null) {
					return false;
				}
				
				WORKING_WITH_TEMPLATE = true;
				
				var allTemplates = $$('a.templateNameInLucidTemplatesTreeStyle');
				var template = null;
				for (var i = 0; i < allTemplates.length; i++) {
					template = allTemplates[i];
					if (template != element) {
						template.setStyle('font-weight', 'normal');
					}
				}
				element.setStyle('font-weight', 'bold');
				
				TEMPLATE_ID = templateId;
				getPrewUrl(templateId);
			});
		}
	);
	
	$$('input.createChildTemplateForCurrentTemplateButtonInLucidStyle').each(
		function(element) {
			element.removeEvents('click');
		
			element.addEvent('click', function() {
				if (TEMPLATE_ID == null) {
					alert(SELECT_TEMPLATE_FIRST_TEXT);
					return false;
				}
				
				WORKING_WITH_TEMPLATE = true;
				
				showLoadingMessage(CREATING_TEXT);
				ThemesEngine.createChildTemplateForThisTemplate(TEMPLATE_ID, {
					callback: function(newTemplateId) {
						closeAllLoadingMessages();
						if (newTemplateId == null) {
							return false;
						}
						
						TEMPLATE_ID = newTemplateId;
						
						var newTemplateElement = $(document.body).getElement('a[templateid='+newTemplateId+']');
						if (newTemplateElement != null) {
							newTemplateElement.setStyle('font-weight', 'bold');
						}
						
						getPrewUrl(newTemplateId);
					}
				});
			});
		}
	);
}

function updateSiteTemplatesTree(tree) {
	if (tree == null) {
		return false;
	}
	
	var container = $('templatesTreeToggle');
	if (container == null) {
		return false;
	}
	
	container.empty();
	
	insertNodesToContainer(tree, container);
	
	registerActionsForTemplatesInLucid();
	
	getThemes(null, true, SLIDER_SHOWED_FIRST_TIME);
}