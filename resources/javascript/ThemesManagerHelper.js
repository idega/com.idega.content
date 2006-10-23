var globalThemeID = null;

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
	ThemesPreviewsProvider.getThemeStyleVariations(themeID, insertStyleVariations);
}

function insertStyleVariations(variations) {
	//document.getElementById('themeSaveButton').disabled = true;
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

function changeThemeCallback(result) {
	closeLoadingMessage();
	if (result == null) {
		document.getElementById('themeSaveButton').disabled = true;
	}
	else {
		globalThemeID = result;
		document.getElementById('themeSaveButton').disabled = false;
	}
}

function saveTheme() {
	if (globalThemeID != null) {
		showLoadingMessage('Saving theme...');
		ThemesPreviewsProvider.saveTheme(globalThemeID, document.getElementById('theme_name').value, saveThemeCallback);
	}
}

function saveThemeCallback(result) {
	closeLoadingMessage();
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
	document.getElementById('theme_name').value = themeName;
}