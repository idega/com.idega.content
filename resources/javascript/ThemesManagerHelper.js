var THEME_ID = null;
var themesArray = new Array();
var themeChanges = new Array();

var imageWidth = 149;
var imageHeight = 112;
var currentIndex = 0;
var waitForStyle = 100;

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
	setGlobalId(themeID);
	setThemeForPreview(themeID);
	ThemesEngine.getThemeStyleVariations(themeID, insertStyleVariations);
}

function insertStyleVariations(variations) {
	var oldVariation = document.getElementById("themeStyleVariations");
	if (oldVariation == null) {
		return;
	}
	if (oldVariation.childNodes) {
		while (oldVariation.childNodes.length > 0) {
			oldVariation.removeChild(oldVariation.childNodes[0]);
		}
	}
	console.log(variations);
	oldVariation.innerHTML = variations;
}

function changeTheme(themeID, styleGroupName, newStyleMember, type, checked) {
	showLoadingMessage("Changing...");
	var radio = true;
	if (type == "checkbox") {
		radio = false;
	}
	var themeNameObj = document.getElementById("theme_name");
	if (themeNameObj != null) {
		ThemesEngine.changeTheme(themeID, styleGroupName, newStyleMember, themeNameObj.value, radio, checked, changeThemeCallback);
	}
}

function changeThemeCallback(themeID) {
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
		showLoadingMessage("Saving...");
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
			new Effect.MoveBy(document.getElementById(containerID), 0, getAllImageSpace(), {duration: 0.01 });
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
			new Effect.MoveBy(document.getElementById(containerID), 0, -(getAllImageSpace()), {duration: 0.01 });
		}
	}
}

function getThemes(themeID, addReflect) {
	showLoadingMessage("Generating preview...");
	setGlobalId(themeID);
	needReflection = addReflect;
	ThemesEngine.getThemesPreviewsInfo(getThemesCallback);
}

function getThemesCallback(themes) {
	if (themes == null) {
		hideThemesContainer();
		closeLoadingMessage();
		return;
	}
	if (themes == "") {
		hideThemesContainer();
		closeLoadingMessage();
		return;
	}
	showThemesContainer();
	var container = document.getElementById(containerID);
	if (container == null) {
		closeLoadingMessage();
		return;
	}
	container.innerHTML = "";
	themesArray = new Array();
	var info = themes.split(";");
	var date = new Date();
	var time = null;
	for (var i = 0; i < info.length; i++) {
		var themeFields = info[i].split("@");
		time = date.getTime()
		var theme = new Theme(themeFields[0], themeFields[1] + "?" + time, themeFields[2] + "?" + time, themeFields[3]);
		var div = document.createElement("div");
		div.className = "imageGallery";
		
		var textDiv = document.createElement("div");
		textDiv.className = "themeName";
		var span = document.createElement("span");
		span.appendChild(document.createTextNode(theme.themeName));
		textDiv.appendChild(span);
		div.appendChild(textDiv);
		
		var imageDiv = document.createElement("div");
		imageDiv.setAttribute("id", theme.id + "_container");
		var image = document.createElement("img"); 
   		image.setAttribute("id", theme.id); 
   		image.setAttribute("src", theme.url);
   		image.setAttribute("width", getImageWidth() + "px");
   		image.setAttribute("height", getImageHeight() + "px");
   		image.setAttribute("title", theme.themeName);
   		if (typeof container.attachEvent != 'undefined') {
   			if (enableStyleVariations) {
            	image.attachEvent('onclick', function(e){getThemeStyleVariations(this.id);});
   			}
            if (enableStyleFunctions) {
	            image.attachEvent('onmouseover', function(e){chooseStyle(this.id);});
	            image.attachEvent('onmouseout', function(e){recallStyle(this.id);});
            }
        } else {
        	if (enableStyleVariations) {
        		image.addEventListener('click', function(e){getThemeStyleVariations(this.id);}, true);
        	}
        	if (enableStyleFunctions) {
	        	image.addEventListener('mouseover', function(e){chooseStyle(this.id);}, true);
	        	image.addEventListener('mouseout', function(e){recallStyle(this.id);}, true);
        	}
        }
        image.className = "reflect rheight18 ropacity68";
        imageDiv.className = "galleryImage firstInRow";
        imageDiv.appendChild(image);
        div.appendChild(imageDiv);
        
		container.appendChild(div);
		themesArray.push(theme);
	}
	container.style.width = Math.round(themesArray.length * getAllImageSpace()) + "px";
	
	var theme = null;
	if (THEME_ID != null) {
		theme = getTheme(THEME_ID);
	}
	else {
		theme = themesArray[0];
	}
	if (theme != null) {
		if (enableStyleVariations) {
			getThemeStyleVariations(theme.id);
		}
		setGlobalId(theme.id);
	}
	closeLoadingMessage();
	addReflectionToThemes();
	if (needApplyThemeForSite) {
		needApplyThemeForSite = false;
		applyThemeForSite(THEME_ID);
	}
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
		return;
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

function removeStyleOptions() {
	var div = document.getElementById("chooseStyleLayer");
	if (div != null) {
		div.style.display = "none";
	}
}

function insertStyleFile() {
	var style = document.createElement("link");
	style.setAttribute("type","text/css");
	style.setAttribute("href", "/idegaweb/bundles/com.idega.content.bundle/resources/style/themes_manager.css");
	style.setAttribute("rel","stylesheet");
	document.getElementsByTagName("head")[0].appendChild(style); 
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
	showLoadingMessage("Restoring...");
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
	for (var i = 0; i < themesArray.length; i++) {
		Reflection.add(document.getElementById(themesArray[i].id), { height: 18/100, opacity: 68/100 });
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
	alert("Sorry, this button is not working yet...");
	return;
	if (THEME_ID == null) {
		return false;
	}
	if (themeChanges == null) {
		return false;
	}
	showLoadingMessage("Changing...");
	ThemesEngine.applyMultipleChangesToTheme(THEME_ID, themeChanges, changeVariationsCallback);
}

function changeVariationsCallback(result) {
	themeChanges = new Array();
	closeLoadingMessage();
}

function ThemeChange(themeID, styleGroupName, variation, variationType, enabled) {
	this.themeID = themeID;
	this.styleGroupName = styleGroupName;
	this.variation = variation;
	this.variationType = variationType;
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

function addThemeChange(themeID, styleGroupName, variation, variationType, enabled) {
	if (THEME_ID != themeID) {
		themeChanges = new Array();	// Reseting array of changes
	}
	if (variationType == "checkbox") {
		removeThemeChange(existThemeChange(themeID, styleGroupName, variation, variationType, enabled), 1);
	}
	if (variationType == "radio") {
		removeSameGroupChanges(themeID, styleGroupName);
	}
	
	var index = existThemeChange(themeID, styleGroupName, variation, variationType, enabled);
	if (index < 0) {
		themeChanges.push(new ThemeChange(themeID, styleGroupName, variation, variationType, enabled));
	}
}

function removeSameGroupChanges(themeID, styleGroupName) {
	if (themeChanges == null) {
		return false;
	}
	var themeChange = null;
	var elementsToRemove = new Array();
	for (var i = 0; i < themeChanges.length; i++) {
		themeChange = themeChanges[i];
		if (themeChange.themeID == themeID && themeChange.styleGroupName == styleGroupName) {
			elementsToRemove.push(i);
		}
	}
	for (var i = 0; i < elementsToRemove.length; i++) {
		removeThemeChange(elementsToRemove[i], 1);
	}
}

function existThemeChange(themeID, styleGroupName, variation, variationType, enabled) {
	if (themeChanges == null) {
		return -1;
	}
	var existChange = false;
	var themeChange = null;
	var i = 0;
	for (i = 0; (i < themeChanges.length && !existChange); i++) {
		themeChange = themeChanges[i];
		if (themeChange.themeID == themeID && themeChange.styleGroupName == styleGroupName && themeChange.variation == variation) {
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