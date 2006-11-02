var globalThemeID = null;
var themesArray = new Array();

var imageWidth = 120;
var imageHeight = 140;
var currentIndex = 0;
var waitForStyle = 2500;

var containerID = "themes";

var enableStyleFunctions = true;

function isCorrectFileType(id, fileType, noFileMsg, invalidFileTypeMsg) {
	var input = document.getElementById(id);
	if (input) {
		if (input.value == "") {
			alert(noFileMsg);
			return false;
		}
		var nameParts = input.value.split(".");
		if (nameParts) {
			var lastPart = nameParts[nameParts.length - 1];
			if (lastPart.toLowerCase() != fileType) {
				alert(invalidFileTypeMsg);
				return false;
			}
		}
	}
	return true;
}

function closeLoadingMessage() {
	var elem = document.getElementById('busybuddy');
	if (elem) {
		if(elem.style) { 
	      elem.style.display = 'none';
	    } else {
	      elem.display = 'none' ;
	    }
	}
}

function getThemeStyleVariations(themeID) {
	setGlobalId(themeID);
	setThemeForPreview(themeID);
	ThemesPreviewsProvider.getThemeStyleVariations(themeID, insertStyleVariations);
}

function insertStyleVariations(variations) {
	var oldVariation = document.getElementById("themeStyleVariations");
	if (oldVariation.childNodes) {
		while (oldVariation.childNodes.length > 0) {
			oldVariation.removeChild(oldVariation.childNodes[0]);
		}
	}
	oldVariation.innerHTML = variations;
}

function changeTheme(themeID, styleGroupName, newStyleMember, type, checked) {
	showLoadingMessage('Changing theme...');
	var radio = true;
	if (type == "checkbox") {
		radio = false;
	}
	ThemesPreviewsProvider.changeTheme(themeID, styleGroupName, newStyleMember, document.getElementById("theme_name").value, radio, checked, changeThemeCallback);
}

function changeThemeCallback(themeID) {
	closeLoadingMessage();
	if (themeID == null) {
		document.getElementById('themeSaveButton').disabled = true;
	}
	else {
		setGlobalId(themeID);
		document.getElementById('themeSaveButton').disabled = false;
		getThemes(themeID);
	}
}

function saveTheme() {
	if (globalThemeID != null) {
		showLoadingMessage('Saving theme...');
		ThemesPreviewsProvider.saveTheme(globalThemeID, document.getElementById('theme_name').value, saveThemeCallback);
	}
}

function saveThemeCallback(themeID) {
	closeLoadingMessage();
	if (themeID != null) {
		setGlobalId(themeID);
		document.getElementById('themeSaveButton').disabled = false;
		getThemes(themeID);
	}
}

function enableButton(inputId) {
	/*if (document.getElementById(inputId).value != "") {
		document.getElementById('themeSaveButton').disabled = false;
	}*/
}

function setGlobalId(themeId) {
	globalThemeID = themeId;
}

function setThemeName(themeName) {
	removeStyleOptions();
	document.getElementById('theme_name').value = themeName;
}

function scroll(id) {
	removeStyleOptions();
	if (id == null) {
		return;
	}
	
	var move = true;
	
	if (id == "leftScroller") {
		// Move to the left
		if ((currentIndex - 1) >= 0) {
			currentIndex--;
		}
		else {
			move = false;
		}
		
		if (move){
			new Effect.MoveBy(document.getElementById(containerID), 0, (imageWidth + 12),  {queue: 'end', mode: 'relative', duration: 0.3 });
		}
		return;
	}
		
	if (id == "rightScroller") {
		// Move to the right
		if ((currentIndex + 1) < themesArray.length) {
			currentIndex++;
		}
		else {
			move = false;
		}
		
		if (move){
			new Effect.MoveBy(document.getElementById(containerID), 0, -(imageWidth + 12),  {queue: 'end', mode: 'relative', duration: 0.3 });
		}
	}
}

function getThemes(themeID) {
	showLoadingMessage("Generating preview");
	setGlobalId(themeID);
	ThemesPreviewsProvider.getThemesPreviewsInfo(getThemesCallback);
}

function getThemesCallback(themes) {
	if (themes == null) {
		closeLoadingMessage();
		return;
	}
	if (themes == "") {
		closeLoadingMessage();
		return;
	}
	var container = document.getElementById(containerID);
	if (container == null) {
		closeLoadingMessage();
		return;
	}
	container.innerHTML = "";
	themesArray = new Array();
	var info = themes.split(";");
	for (var i = 0; i < info.length; i++) {
		var themeFields = info[i].split("@");
		var theme = new Theme(themeFields[0], themeFields[1] + "?" + new Date().getTime(), themeFields[2] + "?" + new Date().getTime(), themeFields[3]);
		var div = document.createElement("div");
		div.className = "imageGallery";
		
		var textDiv = document.createElement("div");
		textDiv.className = "themeName";
		var span = document.createElement("span");
		span.appendChild(document.createTextNode(theme.themeName));
		textDiv.appendChild(span);
		div.appendChild(textDiv);
		
		var div1 = document.createElement("div");
		div1.setAttribute("id", theme.id + "_container");
		var image = document.createElement("img"); 
   		image.setAttribute("id", theme.id); 
   		image.setAttribute("src", theme.url);
   		image.setAttribute("width", imageWidth + "px");
   		image.setAttribute("height", imageHeight + "px");
   		image.setAttribute("title", theme.themeName);
		image.className = "reflect rheight20 ropacity50";
   		if (typeof container.attachEvent != 'undefined') {
            image.attachEvent('onclick', function(e){getThemeStyleVariations(this.id);});
            if (enableStyleFunctions) {
	            image.attachEvent('onmouseover', function(e){chooseStyle(this.id);});
	            image.attachEvent('onmouseout', function(e){recallStyle(this.id);});
            }
        } else {
        	image.addEventListener('click', function(e){getThemeStyleVariations(this.id);}, true);
        	if (enableStyleFunctions) {
	        	image.addEventListener('mouseover', function(e){chooseStyle(this.id);}, true);
	        	image.addEventListener('mouseout', function(e){recallStyle(this.id);}, true);
        	}
        }
        div1.className = "galleryImage firstInRow";
        div1.appendChild(image);
        div.appendChild(div1);
        
		container.appendChild(div);
		themesArray.push(theme);
	}
	container.style.width = Math.round(themesArray.length * 140) + "px";
	var theme = null;
	if (globalThemeID != null) {
		theme = getTheme(globalThemeID);
	}
	else {
		theme = themesArray[0];
	}
	if (theme != null) {
		getThemeStyleVariations(theme.id);
	}
	closeLoadingMessage();
}

function Theme(themeName, url, urlToBig, id) {
	this.themeName = themeName;
	this.url = url;
	this.urlToBig = urlToBig;
	this.id = id;
	this.applyStyle = false;
}

function setPreview(url) {
	var preview = document.getElementById("themePreview");
	if (preview == null) {
		preview = document.createElement("img");
		preview.setAttribute("id", "themePreview");
		preview.className = "bigThemePreview";
		document.getElementById("themePreviewContainer").appendChild(preview);
	}
	preview.src = url;
}

function getTheme(themeID) {
	var theme = null;
	for (var i = 0; i < themesArray.length; i++) {
		theme = themesArray[i];
		if (theme.id == themeID) {
			return theme;
		}
	}
	return null;
}

function setThemeForPreview(themeID) {
	var theme = getTheme(themeID);
	if (theme != null) {
		setPreview(theme.urlToBig);
		setThemeName(theme.themeName);
	}
}

function getThemeIndex(themeID) {
	var theme = null;
	for (var i = 0; i < themesArray.length; i++) {
		theme = themesArray[i];
		if (theme.id == themeID) {
			return i;
		}
	}
	return -1;
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
	var div = document.getElementById("chooseStyleLayer");
	if (div == null) {
		div = document.createElement("div");
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
			pageSpan.attachEvent('onclick', function(e){insertStyle(themeID, true);});
	    	siteSpan.attachEvent('onclick', function(e){insertStyle(themeID, false);});
	    	div.attachEvent('onclick', function(e){removeStyleOptions();});
	    } else {
	    	pageSpan.addEventListener('click', function(e){insertStyle(themeID, true);}, true);
	    	siteSpan.addEventListener('click', function(e){insertStyle(themeID, false);}, true);
	    	div.addEventListener('click', function(e){removeStyleOptions();}, true);
	   	}
		div.appendChild(divp);
		div.appendChild(divs);
		document.body.appendChild(div);
	}
	new Effect.Move(div, {x: getAbsoluteLeft(themeID + "_container") + 2, y: getAbsoluteTop(themeID + "_container") - Math.round(imageHeight * 0.62), mode: 'absolute'});
	div.style.visibility = "visible";
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

function removeStyleOptions() {
	var div = document.getElementById("chooseStyleLayer");
	if (div != null) {
		div.style.visibility="hidden";
	}
}

function insertStyle(themeID, page) {
	removeStyleOptions();
	if (themeID == null) {
		return;
	}
	showLoadingMessage("Applying style...");
	ThemesPreviewsProvider.setSelectedStyle(themeID, page, setSelectedStyleCallback);
}

function setSelectedStyleCallback(result) {
	closeLoadingMessage();
}

function insertStyleFile() {
	var style=document.createElement("link");
	style.setAttribute("type","text/css");
	style.setAttribute("href", "/idegaweb/bundles/com.idega.content.bundle/resources/style/themes_manager.css");
	style.setAttribute("rel","stylesheet");
	document.getElementsByTagName("head")[0].appendChild(style); 
}

function initScript(useStyling) {
	enableStyleFunctions = useStyling;
}