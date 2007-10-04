var PAGE_ID = null;

var TOTAL_WIDTH = 0;
var TOTAL_HEIGHT = 0;

var IS_SITE_MAP = false;
var NEED_RELOAD_BUILDER_PAGE = false;
var NEED_REDIRECT = false;

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
	if (TOTAL_WIDTH != 0) {
		return TOTAL_WIDTH;
	}
	if (typeof(window.innerWidth) == 'number') {
		TOTAL_WIDTH = window.innerWidth; // Non-IE
	} else if(document.documentElement && document.documentElement.clientWidth) {
		TOTAL_WIDTH = document.documentElement.clientWidth; // IE 6+ in 'standards compliant mode'
	} else if(document.body && document.body.clientWidth) {
		TOTAL_WIDTH = document.body.clientWidth; // IE 4 compatible
	}
	return TOTAL_WIDTH;
}

function getTotalHeight() {
	if (TOTAL_HEIGHT != 0) {
		return TOTAL_HEIGHT;
	}
	if (typeof(window.innerHeight) == 'number') {
		TOTAL_HEIGHT = window.innerHeight; // Non-IE
	} else if(document.documentElement && document.documentElement.clientHeight) {
		TOTAL_HEIGHT = document.documentElement.clientHeight; // IE 6+ in 'standards compliant mode'
	} else if(document.body && document.body.clientHeight) {
		TOTAL_HEIGHT = document.body.clientHeight; // IE 4 compatible
	}
	return TOTAL_HEIGHT;
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
		return;
	}
	var treeContainer = document.getElementById(id);
	if (treeContainer == null) {
		return;
	}
	if (treeContainer.childNodes != null) {
		if (treeContainer.childNodes.length != 0) {
			return;
		}
	}
	// No pages created
	var button = document.getElementById('makeStartPage');
	if (button != null) {
		button.disabled = true;
		button.value = getNoPageExistsText();
	}
	
	var rootUl = document.createElement('ul');
	rootUl.setAttribute('id','rootUl');
	var tempTable = document.createElement('table');
	tempTable.setAttribute('id','temporaryTable');
	tempTable.setAttribute('onmouseover','treeObj.prepareToSetTopPage();');	
	tempTable.setAttribute('onmouseout','treeObj.topPageNotSet();');	
	tempTable.style.border='1px  solid';
	tempTable.style.margin='5px';
	var tr=document.createElement('tr');
  	var td=document.createElement('td');
  	var tdText=document.createTextNode(getDropTemplatesHereText()); 
  	td.appendChild(tdText);
  	tr.appendChild(td);
  	tempTable.appendChild(tr); 	
  	rootUl.appendChild(tempTable);
	treeContainer.appendChild(rootUl);
}

function setNeedRelaodBuilderPage(needReload) {
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
var APPLYING_STYLE = 'Applying style...';
var CLOSE_TEXT = 'Close';
var START_PAGE_TEXT = 'Start Page';
var MAKE_START_PAGE_TEXT = 'Start Page';
var MAKE_THIS_PAGE_START_PAGE_TEXT='Make This Page As Start Page';
var CHANGING_STRUCTURE_TEXT = 'Changing structure...';
var NEW_PAGE_TEXT = 'New Page';
var MOVING = 'Moving...';
var ARE_YOU_SURE_TEXT = 'Are you sure?';
var DELETING_TEXT = 'Deleting...';
var CHOOSE_STYLE_FOR_PAGE = 'Page';
var CHOOSE_STYLE_FOR_SITE = 'Site';
var DROP_TEMPLATES_HERE = 'Drop templates here';
var NO_PAGE_EXISTS_TEXT = 'No page exist';
var LOADING_TEXT = 'Loading...';
var RELOADING_TEXT = 'Reloading...';
var SHOW_MODULES_TEXT = 'Show Modules';
var HIDE_MODULES_TEXT = 'Hide Modules';
var REDIRECTING_TEXT = 'Redirecting...';
var CREATING_TEXT = 'Creating...';

function getLocalizedTextForThemes() {
	ThemesEngine.getLocalizedText(getLocalizedTextForThemesCallback);
}

function getLocalizedTextForThemesCallback(list) {
	if (list == null) {
		return;
	}
	if (list.length != 29) {
		return;
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
	var liElement = document.getElementById(getPageID());
	if (liElement == null) {
		return;
	}
	var children = liElement.childNodes;
	if (children == null) {
		return;
	}
	var element = null;
	for (var i = 0; i < children.length; i++) {
		if (children[i].tagName == 'a' || children[i].tagName == 'A') {
			boldSelectedTreeElement(children[i]);
			return;
		}
	}
	return false;
}

function boldSelectedTreeElement(element) {
	if (element == null) {
		return;
	}
	// Unbolding
	var list = getElementsByClassName(document, '*', 'pageTreeNames');
	if (list != null) {
		for (var i = 0; i < list.length; i++) {
			list[i].style.fontWeight = 'normal';
		}
	}
	
	// Bold
	element.style.fontWeight = 'bold';
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
	element.addEvent('dblclick', function(e) {
		handleSiteTreeNodeClick(new Event(e), element);
	});
	element.addEvent('click', function(e) {
		handleSiteTreeNodeClick(new Event(e), element);
	});
}

var DOUBLE_CLICKE_TIME = 250;		// Doubleclick time
var DOUBLE_CLICK_DELAY = 100;		// No clicks after doubleclick
var DOUBLE_CLICK_OCCURED_AT = 0;	// Time of doubleclick
var CLICK_OCCURED_AT = 0;			// Save time of click event.
var TIME_OUT_ID_FOR_CLICKS = null;	// Handle of click setTimeOut
function handleSiteTreeNodeClick(e, element) {
	var eventType = e.type;
	switch (eventType) {
		case 'click':
			// If we've just had a doubleclick then ignore it
			if (hadSiteTreeNodeDoubleClick()) {
				return false;
			}
			
			// Otherwise set timer to act.  It may be preempted by a doubleclick.
			var d = new Date();
			CLICK_OCCURED_AT = d.getTime();
			TIME_OUT_ID_FOR_CLICKS = setTimeout("executeOnClick('"+element.id+"')", DOUBLE_CLICKE_TIME);
			break;
		case 'dblclick':
			executeOnDblClick(element);
			break;
     default:
   }
}

function hadSiteTreeNodeDoubleClick() {
	var d = new Date();
	var now = d.getTime();
	if ((now - DOUBLE_CLICK_OCCURED_AT) < DOUBLE_CLICK_DELAY) {
		return true;
	}
	return false;
}

function executeOnClick(id) {
	if (CLICK_OCCURED_AT - DOUBLE_CLICK_OCCURED_AT <= 0) {
		return false;
	}
	
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
	var d = new Date();
	DOUBLE_CLICK_OCCURED_AT = d.getTime();
	if (TIME_OUT_ID_FOR_CLICKS != null) {
		clearTimeout(TIME_OUT_ID_FOR_CLICKS);	// Clear pending Click  
		TIME_OUT_ID_FOR_CLICKS = null;
	}
	
	initEditLabel(element);
}