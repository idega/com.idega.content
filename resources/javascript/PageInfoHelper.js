var THEME_ID = null;

var SCROLLER_IMAGE_WIDTH = 23;
var SPACE_FROM_LEFT = 290;
var FRAME_CHANGE = 155;

var RESERVED_HEIGHT = 255;
var RESERVED_WIDTH = 500;

var CLICKED_CREATE = false;

var KEYWORDS = null;

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
	if (getPageID() == null || allKeywords == null) {
		closeLoadingMessage();
		return;
	}
	KEYWORDS = allKeywords;
	var keywords = new Array();
	var values = new Array();
	var element = null;
	var treeNode = null;
	for (var i = 0; i < allKeywords.length; i++) {
		element = document.getElementById(allKeywords[i]);
		if (element != null) {
			keywords.push(allKeywords[i]);
			values.push(element.value);
			if (allKeywords[i] == "pageTitle" && element.value != "") {
				treeNode = document.getElementById(getPageID()+ "a");
				if (treeNode != null) {
					var children = treeNode.childNodes;
					if (children != null) {
						for (var j = 0; j < children.length; j++) {
							treeNode.removeChild(children[j]);
						}
					}
					treeNode.appendChild(document.createTextNode(element.value));
				}
			}
		}
	}
	ThemesEngine.savePageInfo(getPageID(), keywords, values, savePageInfoCallback);
}

function savePageInfoCallback(result) {
	if (result != null) {
		var pageUri = document.getElementById("pageUri");
		if (pageUri != null) {
			pageUri.value = result;
		}
	}
	closeLoadingMessage();
	getPrewUrl(getPageID());
}

function showSlider(container) {
	resizeSlider();
	container.style.position = "absolute";
	container.style.bottom = "24px";
	container.style.left = SPACE_FROM_LEFT + "px";
	container.className = "theme_slider";
	new Effect.Appear(container);
	getThemes(null, true);
}

function manageSlider(buttonID) {
	var container = document.getElementById("themesSliderContainer");
	if (container == null) {
		return;
	}
	var button = document.getElementById(buttonID);
	if (button == null) {
		return;
	}
	if (container.style.display == "none") {
		button.value = getHideThemesText();
		showSlider(container);
		changeFrameHeight(-FRAME_CHANGE);
	}
	else {
		removeStyleOptions();
		new Effect.Fade(container);
		button.value = getShowThemesText();
		changeFrameHeight(FRAME_CHANGE);
	}
}

function chooseStyle(themeID) {
	if (themeID == null) {
		return;
	}
	var theme = getTheme(themeID);
	if (theme != null) {
		theme.applyStyle = true;
		setTimeout("sleepAndApply('"+themeID+"')", waitForStyle);
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

function sleepAndApply(themeID) {
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
	var leftPosition = (getAbsoluteLeft(themeID + "_container") + 3);
	if (getTotalWidth() - (leftPosition + getImageWidth())< 0) {
		return; // There is not enough space
	}
	
	setThemeForStyle(themeID);
	
	var div = document.getElementById("chooseStyleLayer");
	var pageSpan = null;
	var siteSpan = null;
	if (div == null) {
		div = document.createElement("div");
		div.style.display = "none";
		div.setAttribute("id", "chooseStyleLayer");
		div.className = "themeChooseStyle";
		
		var divp = document.createElement("div");
		divp.className = "themeChooseStyleText";
		divp.setAttribute("title", getStyleForCurrentPage());
		divp.setAttribute("alt", getStyleForCurrentPage());
		pageSpan = document.createElement("span");
		pageSpan.setAttribute("id", "pageStyle");
		pageSpan.appendChild(document.createTextNode(getChooseStyleForPage()));
		divp.appendChild(pageSpan);
	
		var divs = document.createElement("div");
		divs.className = "themeChooseStyleText";
		divs.setAttribute("title", getStyleForSite());
		divs.setAttribute("alt", getStyleForSite());
		siteSpan = document.createElement("span");
		siteSpan.setAttribute("id", "siteStyle");
		siteSpan.appendChild(document.createTextNode(getChooseStyleForSite()));
		divs.appendChild(siteSpan);
		
		if (typeof div.attachEvent != 'undefined') {
			pageSpan.attachEvent('onclick', function(e){setStyle(true);});
	   		siteSpan.attachEvent('onclick', function(e){setStyle(false);});
	   		div.attachEvent('onclick', function(e){removeStyleOptions();});
		} else {
			pageSpan.addEventListener('click', function(e){setStyle(true);}, false);
	    	siteSpan.addEventListener('click', function(e){setStyle(false);}, false);
	    	div.addEventListener('click', function(e){removeStyleOptions();}, false);
		}
		
		div.appendChild(divp);
		div.appendChild(divs);
		document.body.appendChild(div);
	}
	div.style.left = leftPosition + "px";
	div.style.top = (getAbsoluteTop(themeID + "_container") + 3) + "px";
	div.style.display = "block";
}

function setStyle(isPage) {
	removeStyleOptions();
	if (getThemeForStyle() == null) {
		return;
	}
	if (isPage && getPageID() == null) {
		return;
	}
	showLoadingMessage(getApplyingStyleText());
	setNewStyleToElements("usedThemeName", "themeName");
	setNewStyleForSelectedElement(getThemeForStyle() + "_themeNameContainer", "usedThemeName");
	ThemesEngine.setSelectedStyle(getThemeForStyle(), getPageID(), isPage, setStyleCallback);
}

function setStyleCallback(result) {
	if (getPageID() != null) {
		if (getPageID() != -1) {
			setTimeout("getPrewUrl('"+getPageID()+"')", 1000);
		}
	}
	closeLoadingMessage();
}

function resizeSlider() {
	var rightScroller = document.getElementById("rightScrollerContainer");
	var themesTicker = document.getElementById("themesTickerContainer");
	var container = document.getElementById("themesSliderContainer");
	if (rightScroller == null || themesTicker == null || container == null) {
		return;
	}
	var available = getTotalWidth() - 500;
	if (available > 0) {
		container.style.width = available + "px";
		themesTicker.style.left = (getScrollerImageWidth() - 3) + "px";
		rightScroller.style.left = (available - 23) + "px";
		themesTicker.style.width = (available - 44) + "px";
	}
}

function setButtonText(id, text) {
	var button = document.getElementById(id);
	if (button != null) {
		button.value = text;
	}
}

function newPage() {
	var newPage = document.getElementById("newPageContainer");
	if (CLICKED_CREATE) {
		closeNewPage(newPage);
	}
	else {
		CLICKED_CREATE = true;
		setButtonText("newPageButton", getCloseText());
		new Effect.Appear(newPage);
	}
}

function changeFrameHeight(change) {
	var frame = document.getElementById("treePages");
	if (frame == null) {
		return;
	}
	var current = frame.style.height;
	if (current == null) {
		return;
	}
	var temp = current.split("px");
	if (temp == null) {
		return;
	}
	var height = temp[0];
	height++;
	height--;
	height += change;
	frame.style.height = height + "px";
}

function resizeFrame() {
	var frame = document.getElementById("treePages");
	if (frame == null) {
		return;
	}
	frame.style.left = SPACE_FROM_LEFT + "px";
	var availableWidth = getTotalWidth() - RESERVED_WIDTH;
	if (availableWidth > 0) {
		frame.style.width = availableWidth + "px";
	}
	var availableHeight = getTotalHeight() - RESERVED_HEIGHT;
	if (availableHeight > 0) {
		frame.style.height = availableHeight + "px";
	}
	
	resizeContainer("site_tree_container", "site_tree_container_pages", RESERVED_HEIGHT, true);
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
	if (values == null || KEYWORDS == null) {
		return;
	}
	if (KEYWORDS.length != values.length) {
		return;
	}
	var element = null;
	for (var i = 0; i < KEYWORDS.length; i++) {
		element = document.getElementById(KEYWORDS[i]);
		if (element != null) {
			element.value = values[i];
		}
	}
}

function isStartPage(pageID) {
	ThemesEngine.isStartPage(pageID, isStartPageCallback);
}

function isStartPageCallback(isStart) {
	var button = document.getElementById("makeStartPage");
	if (button == null) {
		return;
	}
	button.disabled = isStart;
	if (isStart) {
		button.value = getStartPageText();
	}
	else {
		button.value = getMakeStartPageText();
	}
}

function makePageAsStartPage() {
	showLoadingMessage(getChangingStructureText());
	ThemesEngine.setAsStartPage(getPageID(), setAsStartPageCallback);
}

function setAsStartPageCallback(result) {
	if (result != null) {
		changePageTitleCallback(result);
	}
	window.location.href=window.location.href;
}

function closeNewPage(newPage) {
	CLICKED_CREATE = false;
	if (newPage != null) {
		newPage.style.display = "none";
	}
	setButtonText("newPageButton", getNewPageText());
}

function managePageInfoComponents() {
	removeStyleOptions();
}

function initializePages() {
	initScript(true, false, false);
	getGlobalPageId();

	showSlider(document.getElementById("themesSliderContainer"));
	resizeFrame();
	getPageInfoValues();
	isStartPage(getPageID());
	checkIfNotEmptySiteTree("div_id_current_structure_tree");
	addEvent(document, "click", managePageInfoComponents);	
}