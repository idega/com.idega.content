var PAGE_ID = null;
var TEMPLATE_ID = null;

var RESERVED_HEIGHT = 82;
var RESERVED_HEIGHT_FOR_PAGES = RESERVED_HEIGHT + 197;
var RESERVED_HEIGHT_FOR_SITE = RESERVED_HEIGHT + 94;

var IS_SITE_MAP = false;
var NEED_RELOAD_BUILDER_PAGE = false;
var NEED_REDIRECT = false;

var EMPTY_SITE_TREE_TEXT_CONTAINER_ID = 'emptySiteTreeTextContainer';
var CURRENT_SITE_STRUCTURE_TREE_ID = 'current_structure_tree';
var ALL_CURRENT_SITE_STRUCTURE_TREE_ID = 'div_id_' + CURRENT_SITE_STRUCTURE_TREE_ID;

var WORKING_WITH_TEMPLATE = false;

var USER_IS_CONTENT_EDITOR = false;

function markIfUserIsContentEditor(userIsContentEditor) {
	USER_IS_CONTENT_EDITOR = userIsContentEditor;
}

function setNeedRedirect(redirect) {
	NEED_REDIRECT = redirect;
}

function isNeedRedirect() {
	return NEED_REDIRECT;
}

function isSiteMap() {
	return IS_SITE_MAP;
}

function setIsSiteMap(isSiteMap) {
	IS_SITE_MAP = isSiteMap;
}

function isChangingSiteMap() {
	if (isSiteMap()) {
		return false;
	}
	else {
		getPageInfoValues();
	}
}

function getPageID() {
	return PAGE_ID;
}

function setPageID(ID) {
	PAGE_ID = ID;
	ThemesEngine.setPageId(ID, nothingToDo);
}

function nothingToDo(parameter) {
}

function getGlobalPageId() {
	if (getPageID() == null) {
		ThemesEngine.getPageId(setGlobalPageId);
	}
	else {
		return getPageID();
	}
}

function setGlobalPageId(ID) {
	setPageID(ID);
	getPrewUrl(ID);
	
	boldCurrentTreeElementWithPageId(ID);
	getPageInfoValues();
}

function changePageTitleCallback(result) {
	if (result == null) {
		return;
	}
	var pageUri = document.getElementById('pageUri');
	if (pageUri != null) {
		pageUri.value = result;
	}
	if (!isSiteMap()) {
		if (getPageID() != null) {
			if (getPageID() != -1) {
				setTimeout("getPrewUrl('"+getPageID()+"')", 1000);
			}
		}
	}
}

function changePageTitleInPageInfo(title) {
	if (title == null) {
		return;
	}
	var element = document.getElementById('pageTitle');
	if (element == null) {
		return;
	}
	element.value = title;
}

function getTotalWidth() {
	return window.getWidth();
}

function getTotalHeight() {
	return window.getHeight();
}

function getRealContainerByStyle(containerID, styleClass) {
	var container = document.getElementById(containerID);
	if (container == null) {
		return;
	}
	var children = container.childNodes;
	if (children == null) {
		return;
	}
	var realContainer = null;
	var found = false;
	for (var i = 0; (i < children.length && !found); i++) {
		realContainer = children[i];
		if (realContainer != null) {
			if (realContainer.className == styleClass) {
				found = true;
			}
		}
	}
	return realContainer;
}

function resizeContainer(containerID, styleClass, usedSpace, changeHeight) {
	var realContainer = getRealContainerByStyle(containerID, styleClass);
	if (realContainer != null) {
		if (changeHeight) {
			realContainer.style.height = (getTotalHeight() - usedSpace) + 'px';
		}
		else {
			realContainer.style.width = (getTotalWidth() - usedSpace) + 'px';
		} 
	}
}

function checkIfNotEmptySiteTree(id) {
	if (id == null) {
		return false;
	}
	var treeContainer = document.getElementById(id);
	if (treeContainer == null) {
		return false;
	}
	if (treeContainer.childNodes != null) {
		if (treeContainer.childNodes.length != 0) {
			return false;
		}
	}
	
	//	No pages created
	var button = document.getElementById('makeStartPage');
	if (button != null) {
		button.disabled = true;
		button.value = getNoPageExistsText();
	}
	
	drawTableForEmptySite(id);
}

function drawTableForEmptySite(id) {
	var container = $(id);
	if (container == null) {
		return false;
	}
	
	container.empty();
	
	var rootUl = new Element('ul');
	rootUl.setProperty('id', 'rootUl');
	rootUl.injectInside(container);
	
	var emptyTextContainer = new Element('div');
	emptyTextContainer.setProperty('id', EMPTY_SITE_TREE_TEXT_CONTAINER_ID);
	emptyTextContainer.addClass('emptySiteTreeContainerStyleClass');
	emptyTextContainer.addEvent('mouseenter', function() {
		try {
			treeObj.prepareToSetTopPage();
		} catch(e) {}
	});	
	emptyTextContainer.addEvent('mouseleave', function() {
		try {
			treeObj.topPageNotSet();
		} catch(e) {}
	});
	emptyTextContainer.injectInside(container);
	
	var stretcher = new Element('div');
	stretcher.setStyle('height', '40%');
	var clone = stretcher.clone();
	stretcher.injectInside(emptyTextContainer);
	
	var textContainer = new Element('div');
	textContainer.appendText(getDropTemplatesHereText());
	textContainer.addClass('emptySiteTreeTextContainerStyleClass');
	textContainer.injectInside(emptyTextContainer);
	
	clone.injectInside(emptyTextContainer);
	
	PAGE_ID = null;
	getPageInfoValues();	//	Reloading page info
}

function setNeedReloadBuilderPage(needReload) {
	NEED_RELOAD_BUILDER_PAGE = needReload;
}

function isNeedRelaodBuilderPage() {
	return NEED_RELOAD_BUILDER_PAGE;
}

function redirectAction(uri, timeOut) {
	if (uri != null) {
		setTimeout("redirectFromSiteMap('"+uri+"')", timeOut);
	}
}

function redirectFromSiteMap(uri) {
	if (uri == null) {
		return;
	}
	var frame = null;
	
	var allFrames = null;
	var parentWindow = window.parent;
	if (parentWindow != null) {
		allFrames = parentWindow.document.getElementsByTagName('iframe');
	}
	else {
		allFrames = document.getElementsByTagName('iframe');
	}
	
	if (allFrames != null) {
		frame = allFrames[0];
	}
	
	if (frame == null) {
		if (parentWindow != null) {
			parentWindow.location.href = uri;
		}
		else {
			closeLoadingMessage();
			return;
		}
	}
	
	frame.src = uri;
}

function savePageInfoWithEnter(event) {
	if (isEnterEvent(event)) {
		savePageInfo();
	}
}

function saveSiteInfoWithEnter(event) {
	if (isEnterEvent(event)) { 
		saveSiteInfo();
		return false;
	}
}

function applyThemeForSite(themeId) {
	setThemeForStyle(themeId);
	setStyle(false);
}

function insertStyleFile() {
	var style = document.createElement('link');
	style.setAttribute('type', 'text/css');
	style.setAttribute('href', '/idegaweb/bundles/com.idega.content.bundle/resources/style/themes_manager.css');
	style.setAttribute('rel', 'stylesheet');
	document.getElementsByTagName('head')[0].appendChild(style);
}

// Localized text
var UPLOADING_THEME = 'Uploading...';
var CHANGING_THEME = 'Changing...';
var SAVING_THEME = 'Saving...';
var GENERATING_PREVIEW = 'Generating preview...';
var RESTORING_THEME = 'Restoring...';
var HIDE_THEMES = 'Hide Themes';
var SHOW_THEMES = 'Show Themes';
var STYLE_FOR_CURRENT_PAGE = 'Select style for current page';
var STYLE_FOR_SITE = 'Select style for all pages';
var STYLE_FOR_PAGE_AND_CHILDREN = 'Select style for current page and all children';
var APPLYING_STYLE = 'Applying style...';
var CLOSE_TEXT = 'Close';
var START_PAGE_TEXT = 'Start Page';
var MAKE_START_PAGE_TEXT = 'Start Page';
var MAKE_THIS_PAGE_START_PAGE_TEXT='Make This Page As Start Page';
var CHANGING_STRUCTURE_TEXT = 'Changing structure...';
var NEW_PAGE_TEXT = 'New Page';
var NEW_PAGES_TEXT = 'New Pages';
var MOVING = 'Moving...';
var ARE_YOU_SURE_TEXT = 'Are you sure?';
var DELETING_TEXT = 'Deleting...';
var CHOOSE_STYLE_FOR_PAGE = 'Page';
var CHOOSE_STYLE_FOR_SITE = 'Site';
var CHOOSE_STYLE_FOR_PAGE_AND_CHILDREN = 'Page*'
var DROP_TEMPLATES_HERE = 'Drop templates here';
var NO_PAGE_EXISTS_TEXT = 'No page exist';
var LOADING_TEXT = 'Loading...';
var RELOADING_TEXT = 'Reloading...';
var SHOW_MODULES_TEXT = 'Show Modules';
var HIDE_MODULES_TEXT = 'Hide Modules';
var REDIRECTING_TEXT = 'Redirecting...';
var CREATING_TEXT = 'Creating...';
var PREPARING_THEME_TEXT = 'Preparing...';
var SELECT_TEMPLATE_FIRST_TEXT = 'Select template first!';
var ARE_YOU_SURE_YOU_WANT_APPLY_THIS_TEMPLATE = 'Are you sure you want to apply this template?';
var INSUFFICIENT_RIGHTS_FOR_ACTION_IN_LUCID = 'Sorry, you have insufficient rights for this action!';

function getLocalizedTextForThemes() {
	ThemesEngine.getLocalizedText(getLocalizedTextForThemesCallback);
}

function getLocalizedTextForThemesCallback(list) {
	if (list == null) {
		return false;
	}
	if (list.length != 36) {
		return false;
	}
	
	UPLOADING_THEME = list[0];
	CHANGING_THEME = list[1];
	SAVING_THEME = list[2];
	GENERATING_PREVIEW = list[3];
	RESTORING_THEME = list[4];
	HIDE_THEMES = list[5];
	SHOW_THEMES = list[6];
	STYLE_FOR_CURRENT_PAGE = list[7];
	STYLE_FOR_SITE = list[8];
	APPLYING_STYLE = list[9];
	CLOSE_TEXT = list[10];
	START_PAGE_TEXT = list[11];
	MAKE_START_PAGE_TEXT = list[12];
	CHANGING_STRUCTURE_TEXT = list[13];
	NEW_PAGE_TEXT = list[14];
	MOVING = list[15];
	ARE_YOU_SURE_TEXT = list[16];
	DELETING_TEXT = list[17];
	CHOOSE_STYLE_FOR_PAGE = list[18];
	CHOOSE_STYLE_FOR_SITE = list[19];
	DROP_TEMPLATES_HERE = list[20];
	NO_PAGE_EXISTS_TEXT = list[21];
	LOADING_TEXT = list[22];
	MAKE_THIS_PAGE_START_PAGE_TEXT = list[23];
	RELOADING_TEXT = list[24];
	SHOW_MODULES_TEXT = list[25];
	HIDE_MODULES_TEXT = list[26];
	REDIRECTING_TEXT = list[27];
	CREATING_TEXT = list[28];
	NEW_PAGES_TEXT  = list[29];
	PREPARING_THEME_TEXT = list[30];
	STYLE_FOR_PAGE_AND_CHILDREN = list[31];
	CHOOSE_STYLE_FOR_PAGE_AND_CHILDREN = list[32];
	SELECT_TEMPLATE_FIRST_TEXT = list[33];
	ARE_YOU_SURE_YOU_WANT_APPLY_THIS_TEMPLATE = list[34];
	INSUFFICIENT_RIGHTS_FOR_ACTION_IN_LUCID = list[35];
	
}

function getUploadingThemeText() {
	return UPLOADING_THEME;
}

function getThemeChangingText() {
	return CHANGING_THEME;
}

function getThemeSavingText() {
	return SAVING_THEME;
}

function getGeneratingPreviewText() {
	return GENERATING_PREVIEW;
}

function getRestoringThemeText() {
	return RESTORING_THEME;
}

function getHideThemesText() {
	return HIDE_THEMES;
}

function getShowThemesText() {
	return SHOW_THEMES;
}

function getStyleForCurrentPage() {
	return STYLE_FOR_CURRENT_PAGE;
}

function getStyleForSite() {
	return STYLE_FOR_SITE;
}

function getStyleForPageAndChildren() {
	return STYLE_FOR_PAGE_AND_CHILDREN;
}

function getApplyingStyleText() {
	return APPLYING_STYLE;
}

function getCloseText() {
	return CLOSE_TEXT;
}

function getStartPageText() {
	return START_PAGE_TEXT;
}

function getMakeStartPageText() {
	return MAKE_START_PAGE_TEXT;
}

function getChangingStructureText() {
	return CHANGING_STRUCTURE_TEXT;
}

function getNewPageText() {
	return NEW_PAGE_TEXT;
}

function getMovingText() {
	return MOVING;
}

function getAreYouSureText() {
	return ARE_YOU_SURE_TEXT;
}

function getDeletingText() {
	return DELETING_TEXT;
}

function getChooseStyleForPage() {
	return CHOOSE_STYLE_FOR_PAGE;
}

function getChooseStyleForSite() {
	return CHOOSE_STYLE_FOR_SITE;
}

function getChooseStyleForPageAndChildren() {
	return CHOOSE_STYLE_FOR_PAGE_AND_CHILDREN;
}

function getDropTemplatesHereText() {
	return DROP_TEMPLATES_HERE;
}

function getNoPageExistsText() {
	return NO_PAGE_EXISTS_TEXT;
}

function getLoadingText() {
	return LOADING_TEXT;
}

function getMakeThisPageStartPageText() {
	return MAKE_THIS_PAGE_START_PAGE_TEXT;
}

function getReloadingText() {
	return RELOADING_TEXT;
}

function getShowModuleText() {
	return SHOW_MODULES_TEXT;
}

function getHideModulesText() {
	return HIDE_MODULES_TEXT;
}
// Localized text ends

function setNewStyleToElements(oldClassName, newClassName) {
	if (oldClassName == null || newClassName == null) {
		return;
	}
	var elements = getElementsByClassName(document, '*', oldClassName);
	if (elements == null) {
		return;
	}
	for (var i = 0; i < elements.length; i++) {
		elements[i].className = newClassName;
	}
}

function setNewStyleForSelectedElement(id, newClassName) {
	if (id == null) {
		return;
	}
	var element = document.getElementById(id);
	if (element == null) {
		return;
	}
	element.className = newClassName;
}

function boldCurrentTreeElement() {
	var pageId = getPageID();
	if (pageId == null) {
		ThemesEngine.getPageId(boldCurrentTreeElementWithPageId);
	}
	else {
		boldCurrentTreeElementWithPageId(pageId);
	}
}

function boldCurrentTreeElementWithPageId(pageId) {
	if (pageId == null) {
		return false;
	}
	
	if (getPageID() == null) {
		setPageID(pageId);
	}
	
	var liElement = $(pageId);
	if (liElement == null) {
		return false;
	}
	
	var children = getElementsByClassName(liElement, 'a', 'pageTreeNames');
	if (children == null) {
		return false;
	}
	
	var element = null;
	for (var i = 0; i < children.length; i++) {
		boldSelectedTreeElement(children[i]);
		return false;
	}
	
	return false;
}

function boldSelectedTreeElement(element) {
	if (element == null) {
		return;
	}
	
	// Unbolding
	var list = $$('a.pageTreeNames');
	if (list != null) {
		for (var i = 0; i < list.length; i++) {
			list[i].setStyle('font-weight', 'normal');
		}
	}
	
	// Bold
	$(element).setStyle('font-weight', 'bold');
}

function startBuilderApplication() {
	ThemesEngine.startBuilderApplication(startBuilderApplicationCallback);
}

function startBuilderApplicationCallback(result) {
}

function roundThemesSliderCorners() {
	//$('themesSliderContainer').makeRounded({radius: 30});
}

function registerActionsForSiteTree() {
	$$('a.pageTreeNames').each(
		function(element) {
			registerActionsOnSiteTreeElement(element);
	   	}
	);
}

function registerActionsOnSiteTreeElement(element) {
	element.removeEvents('click');
	element.removeEvents('dblclick');
	
	element.addEvent('dblclick', function() {
		WORKING_WITH_TEMPLATE = false;
		
		executeOnDblClick(element);
		markAsExecutingDoubleClick(element.id);
		return false;
	});
	element.addEvent('click', function() {
		WORKING_WITH_TEMPLATE = false;
		
		handleSiteTreeNodeClick(element);
		return false;
	});
}

var DOUBLE_CLICKE_TIME = 250;		//	Waiting time for other clicks on the same element
var CLICKS_ON_ELEMENT = new Array();

function ClickOnSiteTreeElement(id, count) {
	this.id = id;
	this.count = count;
	this.timeOutId = null;
	this.executingAction = false;
}

function markAsExecutingDoubleClick(id) {
	var click = getClicksNumberOnElement(id);
	click.count = 2;
	click.executingAction = true;
}

function getClicksNumberOnElement(id) {
	var click = null;
	for (var i = 0; i < CLICKS_ON_ELEMENT.length; i++) {
		click = CLICKS_ON_ELEMENT[i];
		if (click.id == id) {
			return click;
		}
	}
	click = new ClickOnSiteTreeElement(id, 0);
	CLICKS_ON_ELEMENT.push(click);
	return click;
}

function resetOtherClicksForSiteTree(id) {
	var click = null;
	for (var i = 0; i < CLICKS_ON_ELEMENT.length; i++) {
		click = CLICKS_ON_ELEMENT[i];
		if (click.id != id) {
			click.executingAction = false;
			if (click.timeOutId != null) {
				window.clearTimeout(click.timeOutId);
				click.timeOutId = null;
			}
			click.count = 0;
		}
	}
}

function clearAllClicksForSiteTree() {
	var click = null;
	for (var i = 0; i < CLICKS_ON_ELEMENT.length; i++) {
		click = CLICKS_ON_ELEMENT[i];
		click.executingAction = false;
		if (click.timeOutId != null) {
			window.clearTimeout(click.timeOutId);
			click.timeOutId = null;
		}
		click.count = 0;
	}
}

function handleSiteTreeNodeClick(element) {
	var id = element.id;
	
	resetOtherClicksForSiteTree(id);
	
	var click = getClicksNumberOnElement(id);
	click.count++;
	if (click.timeOutId == null) {
		var action = function() {
			executeSiteTreeActionAfterTimeOut(id);
		}
		click.timeOutId = window.setTimeout(action, DOUBLE_CLICKE_TIME);
	}
}

function executeSiteTreeActionAfterTimeOut(id) {
	var click = getClicksNumberOnElement(id);	
	if (click == null) {
		executeOnClick(id);
	}
	
	var count = click.count;
	var executingAction = click.executingAction;
	
	clearAllClicksForSiteTree();
	
	if (executingAction) {
		return false;
	}
	
	if (count <= 1) {
		executeOnClick(id)
	}
	else {
		executeOnDblClick($(id));
	}
	
	return false;
}

function executeOnClick(id) {
	var element = $(id);
	if (element == null) {
		return false;
	}
	
	var newPageId = element.parentNode.id;
	boldSelectedTreeElement(element);
	setPageID(newPageId);
	if (!IS_SITE_MAP) {
		getPrewUrl(newPageId);
		getPageInfoValues();
		isStartPage(newPageId);
	}
}

function executeOnDblClick(element) {
	initEditLabel(element);
}

var SET_REVERSE_AJAX_IN_THEMES = false;
var ENABLE_REVERSE_AJAX_TIME_OUT_ID_IN_THEMES = 0;
function enableReverseAjaxInThemes() {
	if (!SET_REVERSE_AJAX_IN_THEMES) {
		if (isSafariBrowser()) {
			ENABLE_REVERSE_AJAX_TIME_OUT_ID_IN_THEMES = window.setTimeout(setReverseAjaxInThemes, 3000);
		}
		else {
			setReverseAjaxInThemes();
		}
	}
}

function setReverseAjaxInThemes() {
	if (!SET_REVERSE_AJAX_IN_THEMES) {
		SET_REVERSE_AJAX_IN_THEMES = true;
		DWREngine.setActiveReverseAjax(true);
	}
	if (ENABLE_REVERSE_AJAX_TIME_OUT_ID_IN_THEMES != 0) {
		window.clearTimeout(ENABLE_REVERSE_AJAX_TIME_OUT_ID_IN_THEMES);
		ENABLE_REVERSE_AJAX_TIME_OUT_ID_IN_THEMES = 0;
	}
}

var LOADING_LAYER_ABOVE_TREE = null;
var TIME_OUT_ID_FOR_ELEMENT_LOADING_LAYER = null;
function updateSiteTree(updatedTree) {	
	if (updatedTree == null) {
		return false;
	}

	var container = $(CURRENT_SITE_STRUCTURE_TREE_ID).getParent();
	if (container == null) {
		return false;
	}
	
	LOADING_LAYER_ABOVE_TREE = $(setLoadingLayerForElement(CURRENT_SITE_STRUCTURE_TREE_ID, false, container.getSize(), container.getPosition()));

	container.empty();	
	insertNodesToContainer(updatedTree, container);
	
	try {
		var treeObj = new JSDragDropTree();
		treeObj.setTreeId(CURRENT_SITE_STRUCTURE_TREE_ID);
		treeObj.initTree();
		treeObj.checkIfOverTree(CURRENT_SITE_STRUCTURE_TREE_ID);
		treeObj.getNodeOrders();
		treeObj.expandAll();
	} catch(e) {
		reloadPage();
	}
	
	if (isSiteMap()) {
		resizeTreeContainerInThemes(RESERVED_HEIGHT_FOR_SITE);
	}
	else {
		resizeTreeContainerInThemes(RESERVED_HEIGHT_FOR_PAGES);
		getPrewUrl(getPageID());
	}
	
	checkIfNotEmptySiteTree(ALL_CURRENT_SITE_STRUCTURE_TREE_ID);
	
	registerActionsForSiteTree();
	boldCurrentTreeElement();
	
	if (LOADING_LAYER_ABOVE_TREE) {
		TIME_OUT_ID_FOR_ELEMENT_LOADING_LAYER = window.setTimeout(showLayerAndResumeUpdatingTree, 500);
	}
}

function showLayerAndResumeUpdatingTree() {
	if (TIME_OUT_ID_FOR_ELEMENT_LOADING_LAYER != null) {
		window.clearTimeout(TIME_OUT_ID_FOR_ELEMENT_LOADING_LAYER);
		TIME_OUT_ID_FOR_ELEMENT_LOADING_LAYER = null;
	}
	
	LOADING_LAYER_ABOVE_TREE.remove();
	LOADING_LAYER_ABOVE_TREE = null;
}

function resizeTreeContainerInThemes(reservedHeight) {
	var siteTreeContainer = $(ALL_CURRENT_SITE_STRUCTURE_TREE_ID);
	if (siteTreeContainer) {
		var totalHeight = getTotalHeight();
		var height = totalHeight - reservedHeight;
		if (height > 0) {
			siteTreeContainer.setStyle('height', height + 'px');
		}

		var heightForAccordion = totalHeight - 225;
		if (heightForAccordion > 0) {
			$$('.selectedElement').setStyle('height', heightForAccordion + 'px');
		}
	}
	
	if (!isSiteMap()) {
		resizeFrame();
		resizeSlider();
	}
}

function controlLucidAppWindow() {
	if (isSiteMap()) {
		resizeTreeContainerInThemes(RESERVED_HEIGHT_FOR_SITE);
	}
	else {
		resizeTreeContainerInThemes(RESERVED_HEIGHT_FOR_PAGES);
	}
}

function createAccordionForLucid() {
	var accordion = new Accordion('span.atStart', 'div.atStart', {
		opacity: false,
		display: 0,
		height: false,
		transition: Fx.Transitions.quadOut,
		onActive: function(toggler, element){
			toggler.addClass('selectedToggler');

			element.removeClass('hiddenElement');
			element.addClass('selectedElement');

			var heightForAccordion = getTotalHeight() - 225;
			if (heightForAccordion > 0) {
				element.setStyle('height', heightForAccordion + 'px');
			}
		},
 
		onBackground: function(toggler, element){
			toggler.removeClass('selectedToggler');

			element.removeClass('selectedElement');
			element.addClass('hiddenElement');
			element.setStyle('height', '0px');
		}
	}, $('accordion'));
}

function getUpdatedSiteTreeFromServer() {
	ThemesEngine.getUpdatedSiteTree({
		callback: function(tree) {
			updateSiteTree(tree);
		}
	});
}

function getUpdatedSiteTemplatesTreeFromServer() {
	ThemesEngine.getUpdatedSiteTemplatesTree({
		callback: function(templatesTree) {
			updateSiteTemplatesTree(templatesTree);
		}
	});
}

function updateSiteInfoBoxWithNewValues() {
	try {
		setActiveLanguage();
		getValues(null);
	} catch(e) {};
}