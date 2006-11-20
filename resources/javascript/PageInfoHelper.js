var globalPageID = 1;

var scrollerImageWidth = 23;

function getScrollerImageWidth() {
	return scrollerImageWidth;
}

function savePageInfo() {
	showLoadingMessage("Saving...");
	ThemesEngine.getPageInfoElements(getPageInfoElementsCallback);
}

function getPageInfoElementsCallback(allKeywords) {
	if (globalPageID == null || allKeywords == null) {
		closeLoadingMessage();
		return;
	}
	var keywords = new Array();
	var values = new Array();
	var element = null;
	for (var i = 0; i < allKeywords.length; i++) {
		element = document.getElementById(allKeywords[i]);
		if (element != null) {
			if (element.value != "") {
				keywords.push(allKeywords[i]);
				values.push(element.value);
				element.value = "";
			}
		}
	}
	ThemesEngine.savePageInfo(globalPageID, keywords, values, savePageInfoCallback);
}

function savePageInfoCallback(result) {
	closeLoadingMessage();
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
		var left = 290;
		resizeSlider(left);
		container.style.position = "absolute";
		container.style.bottom = "30px";
		container.style.left = left + "px";
		container.className = "theme_slider";
		new Effect.Appear(container);
		button.value = "Hide Themes";
		getThemes(null, true);
	}
	else {
		removeStyleOptions();
		new Effect.Fade(container);
		button.value = "Show Themes";
	}
}

function setPageID(ID) {
	globalPageID = ID;
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
	
	var div = document.getElementById("chooseStyleLayer");
	if (div == null) {
		div = document.createElement("div");
		div.style.display = "none";
		div.setAttribute("id", "chooseStyleLayer");
		div.className = "themeChooseStyle";
		
		var divp = document.createElement("div");
		divp.className = "themeChooseStyleText";
		var pageSpan = document.createElement("span");
		pageSpan.setAttribute("id", "pageStyle");
		pageSpan.appendChild(document.createTextNode("Page"));
		divp.appendChild(pageSpan);
	
		var divs = document.createElement("div");
		divs.className = "themeChooseStyleText";
		var siteSpan = document.createElement("span");
		siteSpan.setAttribute("id", "siteStyle");
		siteSpan.appendChild(document.createTextNode("Site"));
		divs.appendChild(siteSpan);
		
		if (typeof div.attachEvent != 'undefined') {
			pageSpan.attachEvent('onclick', function(e){setStyle(themeID, true);});
	    	siteSpan.attachEvent('onclick', function(e){setStyle(themeID, false);});
	    	div.attachEvent('onclick', function(e){removeStyleOptions();});
	    } else {
	    	pageSpan.addEventListener('click', function(e){setStyle(themeID, true);}, true);
	    	siteSpan.addEventListener('click', function(e){setStyle(themeID, false);}, true);
	    	div.addEventListener('click', function(e){removeStyleOptions();}, true);
	   	}
		div.appendChild(divp);
		div.appendChild(divs);
		document.body.appendChild(div);
	}
	div.style.left = leftPosition + "px";
	div.style.top = (getAbsoluteTop(themeID + "_container") + 3) + "px";
	//new Effect.Appear(div);
	div.style.display = "block";
}

function getAbsoluteLeft(objectId) {
	o = document.getElementById(objectId);
	if (o == null) {
		return 0;
	}
	oLeft = o.offsetLeft;
	while(o.offsetParent != null) {
		oParent = o.offsetParent;
		oLeft += oParent.offsetLeft;
		o = oParent;
	}
	return oLeft;
}

function getAbsoluteTop(objectId) {
	o = document.getElementById(objectId);
	if (o == null) {
		return 0;
	}
	oTop = o.offsetTop;
	while(o.offsetParent != null) {
		oParent = o.offsetParent;
		oTop += oParent.offsetTop;
		o = oParent;
	}
	return oTop;
}

function setStyle(themeID, page) {
	removeStyleOptions();
	if (themeID == null) {
		return;
	}
	showLoadingMessage("Applying style...");
	ThemesEngine.setSelectedStyle(themeID, page, setStyleCallback);
}

function setStyleCallback(result) {
	closeLoadingMessage();
}

function resizeSlider(left) {
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

function getTotalWidth() {
  var width = 0;
  if(typeof(window.innerWidth) == "number") {
    width = window.innerWidth; // Non-IE
  } else if(document.documentElement && document.documentElement.clientWidth) {
    width = document.documentElement.clientWidth; // IE 6+ in 'standards compliant mode'
  } else if(document.body && document.body.clientWidth) {
    width = document.body.clientWidth; // IE 4 compatible
  }
  return width;
}