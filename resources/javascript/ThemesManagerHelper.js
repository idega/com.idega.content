var THEME_ID = null;
var THEMES = new Array();
var themeChanges = new Array();

var imageWidth = 149;
var imageHeight = 112;
var currentIndex = 0;
var waitForStyle = 100;
var SLIDE_DURATION = 500;

var containerID = "themes";
var themesContainerID = "themesSliderContainer";
var showThemesButtonID = "showThemesButton";

var enableStyleFunctions = true;
var enableThemeContainer = true;
var enableStyleVariations = true;

var needReflection = false;
var needApplyThemeForSite = false;

function getImageWidth() {
	return imageWidth;
}

function getAllImageSpace() {
	return (getImageWidth() + 8);
}

function getImageHeight() {
	return imageHeight;
}

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

function getThemeStyleVariations(themeID) {
	if (THEME_ID != null) {
		var oldTheme = getTheme(THEME_ID);
		if (oldTheme != null) {
			var oldClassName = "themeName";
			if (oldTheme.used) {
				oldClassName = "usedThemeName";
			}
			setNewStyleForSelectedElement(oldTheme.id + "_themeNameContainer", oldClassName);
		}
	}
	showLoadingMessage(getLoadingText());
	setGlobalId(themeID);
	var newTheme = getTheme(THEME_ID);
	if (!newTheme.used) {
		setNewStyleForSelectedElement(newTheme.id + "_themeNameContainer", "selectedThemeName");
	}
	setThemeForPreview(themeID);
	ThemesEngine.getThemeStyleVariations(themeID, insertStyleVariations);
}

function insertStyleVariations(variations) {
	closeLoadingMessage();
	var theme = getTheme(THEME_ID);
	if (theme != null) {
		setIfUsedTheme(theme.used);
	}
	else {
		setIfUsedTheme(false);
	}
	var oldVariation = document.getElementById("themeStyleVariations");
	if (oldVariation == null) {
		return;
	}
	if (oldVariation.childNodes) {
		while (oldVariation.childNodes.length > 0) {
			oldVariation.removeChild(oldVariation.childNodes[0]);
		}
	}
	oldVariation.innerHTML = variations;
}

function changeTheme(themeID, styleGroupName, newStyleMember, type, checked) {
	var radio = true;
	if (type == "checkbox") {
		radio = false;
	}
	var themeNameObj = document.getElementById("theme_name");
	if (themeNameObj != null) {
		showLoadingMessage(getThemeChangingText());
		ThemesEngine.changeTheme(themeID, styleGroupName, newStyleMember, themeNameObj.value, radio, checked, changeThemeCallback);
	}
}

function changeThemeCallback(themeID) {
	themeChanges = new Array();
	closeLoadingMessage();
	if (themeID == null) {
		document.getElementById('themeSaveButton').disabled = true;
	}
	else {
		setGlobalId(themeID);
		document.getElementById('themeSaveButton').disabled = false;
		getThemes(themeID, true);
	}
}

function saveTheme() {
	if (THEME_ID != null) {
		showLoadingMessage(getThemeSavingText());
		var themeNameObj = document.getElementById("theme_name");
		if (themeNameObj != null) {
			ThemesEngine.saveTheme(THEME_ID, themeNameObj.value, saveThemeCallback);
		}
	}
}

function saveThemeCallback(result) {
	closeLoadingMessage();
	if (result) {
		document.getElementById('themeSaveButton').disabled = false;
		getThemes(THEME_ID, true);
	}
	else {
		if (needApplyThemeForSite) {
			needApplyThemeForSite = false;
			applyThemeForSite(THEME_ID);
		}
	}
}

function manageButton(id, disable) {
	var button = document.getElementById(id);
	if (button != null) {
		button.disabled = disable;
	}
}

function setGlobalId(themeId) {
	THEME_ID = themeId;
}

function setThemeName(themeName) {
	removeStyleOptions();
	var themeNameObj = document.getElementById("theme_name");
	if (themeNameObj != null) {
		themeNameObj.value = themeName;
	}
}

function scroll(id) {
	removeStyleOptions();
	if (id == null) {
		return;
	}
	
	var themesToSlide = 1;
	var move = true;
	
	var moveToPosition = 0;
	var containerLeftValue = 0;
	if ($(containerID).style.left) {
		var pixelsFromLeft = $(containerID).style.left;
		temp = pixelsFromLeft.split("px");
		if (temp != null) {
			containerLeftValue = temp[0];
			containerLeftValue++;
			containerLeftValue--;
			moveToPosition = containerLeftValue;
		}
	}
	
	// Getting ticker length
	var tickerFrom = getAbsoluteLeft("themesTickerContainer");
	var tickerTo = getAbsoluteLeft("rightScrollerContainer");
	var tickerWidth = tickerTo - tickerFrom;
	
	// Checking if need to scroll
	var containerWidth = 0;
	var containerWidthWithPixels = $(containerID).style.width;
	var temp = containerWidthWithPixels.split("px");
	if (temp != null) {
		var widthValue = temp[0];
		widthValue++;
		widthValue--;
		containerWidth = widthValue;
	}
	if (tickerWidth > containerWidth) {	// All themes are visible in slider
		moveSlider(0);	// Restoring slider
		return;
	}
	themesToSlide = Math.floor(tickerWidth / getAllImageSpace());
	if (themesToSlide < 0) {	// No space to slide?
		return;
	}
	
	if (id == "leftScroller") {
		// Move to the left
		if ((currentIndex - themesToSlide) >= 0) {
			currentIndex -= themesToSlide;
			moveToPosition += (getAllImageSpace() * themesToSlide);
		}
		else {
			move = false;
		}
	}
	else {
		if (id == "rightScroller") {
			// Move to the right
			if ((currentIndex + themesToSlide) < THEMES.length) {
				currentIndex += themesToSlide;
				moveToPosition -= (getAllImageSpace() * themesToSlide);
			}
			else {
				move = false;
			}
		}
	}
		
	if (move) {
		moveSlider(moveToPosition);
	}
}

function moveSlider(moveToPosition) {
	var themesContainer = document.getElementById(containerID);
	if (themesContainer == null) {
		return;
	}
	hideThemeSliderButtons();
	window.setTimeout("showThemeSliderButtons()", SLIDE_DURATION);
	var slideMove = new Fx.Style(themesContainer, 'left', {duration: SLIDE_DURATION, transition: Fx.Transitions.quadOut});
	slideMove.start(moveToPosition);
	//window.setTimeout("showThemeSliderButtons()", SLIDE_DURATION);
}

function showThemeSliderButtons() {
	var ids = new Array();
	ids.push("leftScrollerContainer");
	ids.push("rightScrollerContainer");
	setElementsVisibilityProperty(ids, "visible");
}

function hideThemeSliderButtons() {
	var ids = new Array();
	ids.push("leftScrollerContainer");
	ids.push("rightScrollerContainer");
	setElementsVisibilityProperty(ids, "hidden");
}

function setElementsVisibilityProperty(ids, property) {
	if (ids == null) {
		return;
	}
	var element = null;
	for (var i = 0; i < ids.length; i++) {
		element = document.getElementById(ids[i]);
		if (element != null) {
			element.style.visibility = property;
		}
	}
}

function removeChildren(element) {
	if (element == null) {
		return;
	}
	var children = element.childNodes;
	if (children == null) {
		return;
	}
	var size = children.length;
	var child = null;
	for (var i = 0; i < size; i++) {
		child = children[0];
		if (child != null) {
			element.removeChild(child);
		}
	}
}

function getThemes(themeID, addReflect) {
	showLoadingMessage(getGeneratingPreviewText());
	setGlobalId(themeID);
	needReflection = addReflect;
	ThemesEngine.getThemes(getThemesCallback);
}

function getThemesCallback(themes) {
	closeLoadingMessage();
	if (themes == null) {
		hideThemesContainer();
		return;
	}

	showThemesContainer();
	var container = document.getElementById(containerID);
	if (container == null) {
		return;
	}
	
	removeChildren(container);
	
	THEMES = new Array();
	var simpleTheme = null;
	var date = new Date();
	var time = null;
	var theme = null
	for (var i = 0; i < themes.length; i++) {
		simpleTheme = themes[i];
		
		time = date.getTime()
		theme = new Theme(simpleTheme.name, simpleTheme.linkToSmallPreview + "?" + time, simpleTheme.linkToBigPreview + "?" + time, simpleTheme.id, simpleTheme.used);
		
		var div = document.createElement("div");
		div.setAttribute("id", theme.id + "_mainContainer");
		div.className = "imageGallery";
		
		// Is used?
		if (theme.used) {
			div.setAttribute("title", document.getElementById("defaultThemeLabel").value);
		}
		else {
			div.setAttribute("title", document.getElementById("notDefaultThemeLabel").value);
		}
		
		// Name
		var textDiv = document.createElement("div");
		textDiv.setAttribute("id", theme.id + "_themeNameContainer");
		if (theme.used) {
			textDiv.className = "usedThemeName";
		}
		else {
			textDiv.className = "themeName";
		}
		var themeNameContainer = document.createElement("strong");
		themeNameContainer.appendChild(document.createTextNode(theme.name));
		textDiv.appendChild(themeNameContainer);
		div.appendChild(textDiv);
		
		var imageDiv = document.createElement("div");
		imageDiv.setAttribute("id", theme.id + "_container");
		var image = document.createElement("img"); 
   		image.setAttribute("id", theme.id); 
   		image.setAttribute("src", theme.linkToSmallPreview);
   		image.setAttribute("width", getImageWidth() + "px");
   		image.setAttribute("height", getImageHeight() + "px");
   		//image.setAttribute("title", theme.themeName);
   		if (typeof container.attachEvent == "undefined") {
   			if (enableStyleVariations) {
        		image.addEventListener("click", function(e){getThemeStyleVariations(this.id);}, true);
        	}
        	if (enableStyleFunctions) {
	        	image.addEventListener("mouseover", function(e){chooseStyle(this.id);}, true);
	        	image.addEventListener("mouseout", function(e){recallStyle(this.id);}, true);
        	}
        } else {
        	if (enableStyleVariations) {
            	image.attachEvent("onclick", function(e){getThemeStyleVariations(this.id);});
   			}
            if (enableStyleFunctions) {
	            image.attachEvent("onmouseover", function(e){chooseStyle(this.id);});
	            image.attachEvent("onmouseout", function(e){recallStyle(this.id);});
            }
        }
        image.className = "reflect rheight18 ropacity68";
        imageDiv.className = "galleryImage firstInRow";
        imageDiv.appendChild(image);
        div.appendChild(imageDiv);
        
		container.appendChild(div);
		THEMES.push(theme);
	}
	container.style.width = Math.round(THEMES.length * getAllImageSpace()) + "px";
	
	theme = null;
	if (THEME_ID != null) {
		theme = getTheme(THEME_ID);
	}
	else {
		theme = THEMES[0];
	}
	if (theme != null) {
		if (enableStyleVariations) {
			getThemeStyleVariations(theme.id);
		}
		setGlobalId(theme.id);
	}
	addReflectionToThemes();
	if (needApplyThemeForSite) {
		needApplyThemeForSite = false;
		applyThemeForSite(THEME_ID);
	}
}

function setIfUsedTheme(used) {
	var element = document.getElementById("themeUsability");
	if (element == null) {
		return;
	}
	if (element.childNodes != null) {
		for (var i = 0; element.childNodes.length; i++) {
			element.removeChild(element.childNodes[i]);
		}
	}
	var text = "";
	if (used) {
		text = document.getElementById("defaultThemeLabel").value;
	}
	else {
		text = document.getElementById("notDefaultThemeLabel").value;
	}
	element.appendChild(document.createTextNode(text));
	
	// Highlight
	var highlight = new Fx.Style(element, 'background-color', {duration: 1500});
	highlight.start("#ffff99", "#F2F2F2");
}

function Theme(name, linkToSmallPreview, linkToBigPreview, id, used) {
	this.name = name;
	this.linkToSmallPreview = linkToSmallPreview;
	this.linkToBigPreview = linkToBigPreview;
	this.id = id;
	this.applyStyle = false;
	this.used = used;
}

function setPreview(url) {
	var preview = document.getElementById("themePreview");
	if (preview == null) {
		return;
	}
	preview.src = url;
}

function getTheme(themeID) {
	var theme = null;
	for (var i = 0; i < THEMES.length; i++) {
		theme = THEMES[i];
		if (theme.id == themeID) {
			return theme;
		}
	}
	return null;
}

function setThemeForPreview(themeID) {
	var theme = getTheme(themeID);
	if (theme != null) {
		setPreview(theme.linkToBigPreview);
		setThemeName(theme.name);
	}
}

function getThemeIndex(themeID) {
	var theme = null;
	for (var i = 0; i < THEMES.length; i++) {
		theme = THEMES[i];
		if (theme.id == themeID) {
			return i;
		}
	}
	return -1;
}

function removeStyleOptions() {
	var div = document.getElementById("chooseStyleLayer");
	if (div != null) {
		div.style.display = "none";
	}
}

function initScript(useStyling, enableContainer, enableVariations) {
	enableStyleFunctions = useStyling;
	enableThemeContainer = enableContainer;
	enableStyleVariations = enableVariations;
}

function hideThemesContainer() {
	var container = document.getElementById(themesContainerID);
	if (container != null) {
		container.style.display = "none";
	}
	manageButton(showThemesButtonID, true);
}

function showThemesContainer() {
	if (enableThemeContainer) {
		var container = document.getElementById(themesContainerID);
		if (container != null) {
			container.style.display = "block";
		}
	}
	manageButton(showThemesButtonID, false);
}

function restoreTheme() {
	showLoadingMessage(getRestoringThemeText());
	ThemesEngine.restoreTheme(THEME_ID, restoreThemeCallback);
}

function restoreThemeCallback(result) {
	closeLoadingMessage();
	getThemes(THEME_ID, true);
}

function addReflectionToThemes() {
	if (needReflection) {
		setTimeout("addAfterSleep()", 1000); // Needs to stop script, because reflection is added before images are loaded
	}
}

function addAfterSleep() {
	for (var i = 0; i < THEMES.length; i++) {
		Reflection.add(document.getElementById(THEMES[i].id), { height: 18/100, opacity: 68/100 });
	}
}

function saveAndApplyTheme() {
	if (THEME_ID == null) {
		return false;
	}
	needApplyThemeForSite = true;
	saveTheme();
}

function changeVariations() {
	if (THEME_ID == null) {
		return false;
	}
	if (themeChanges == null) {
		return false;
	}
	if (themeChanges.length == 0) {
		return false;
	}
	var themeNameObj = document.getElementById("theme_name");
	if (themeNameObj != null) {
		showLoadingMessage(getThemeChangingText());
		ThemesEngine.applyMultipleChangesToTheme(THEME_ID, themeChanges, themeNameObj.value, changeThemeCallback);
	}
}

function ThemeChange(themeId, styleGroupName, variation, radio, enabled) {
	this.themeId = themeId;
	this.styleGroupName = styleGroupName;
	this.variation = variation;
	this.radio = radio;
	this.enabled = enabled;
}

function removeThemeChange(index, elementsToRemove) {
	if (themeChanges == null) {
		return false;
	}
	if (index >= 0 && index < themeChanges.length) {
		themeChanges.splice(index, elementsToRemove);
		return true;
	}
	return false;
}

function addThemeChange(themeId, styleGroupName, variation, variationType, enabled) {
	if (THEME_ID != themeId) {
		themeChanges = new Array();	// Reseting array of changes
	}
	var radio = true;
	if (variationType == "checkbox") {
		radio = false;
		removeThemeChange(existThemeChange(themeId, styleGroupName, variation, radio, enabled), 1);
	}
	if (radio) {
		removeSameGroupChanges(themeId, styleGroupName);
	}
	
	var index = existThemeChange(themeId, styleGroupName, variation, radio, enabled);
	if (index < 0) {
		themeChanges.push(new ThemeChange(themeId, styleGroupName, variation, radio, enabled));
	}
}

function removeSameGroupChanges(themeId, styleGroupName) {
	if (themeChanges == null) {
		return false;
	}
	var themeChange = null;
	var elementsToRemove = new Array();
	for (var i = 0; i < themeChanges.length; i++) {
		themeChange = themeChanges[i];
		if (themeChange.themeId == themeId && themeChange.styleGroupName == styleGroupName) {
			elementsToRemove.push(i);
		}
	}
	for (var i = 0; i < elementsToRemove.length; i++) {
		removeThemeChange(elementsToRemove[i], 1);
	}
}

function existThemeChange(themeId, styleGroupName, variation, radio, enabled) {
	if (themeChanges == null) {
		return -1;
	}
	var existChange = false;
	var themeChange = null;
	var i = 0;
	for (i = 0; (i < themeChanges.length && !existChange); i++) {
		themeChange = themeChanges[i];
		if (themeChange.themeId == themeId && themeChange.styleGroupName == styleGroupName && themeChange.variation == variation) {
			existChange = true;
		}
	}
	if (existChange) {
		if (i == 0) {
			return 0;
		}
		return i - 1;
	}
	return -1;
}

function initializeThemes() {
	insertStyleFile();
	initScript(false, true, true);
	getThemes(null, true);
}