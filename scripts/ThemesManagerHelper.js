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
	document.getElementById('themeSaveButton').disabled = true;
	var oldVariation = document.getElementById("themeStyleVariations");
	if (oldVariation.childNodes) {
		while (oldVariation.childNodes.length > 0) {
			oldVariation.removeChild(oldVariation.childNodes[0]);
		}
	}
	oldVariation.innerHTML = variations;
}

function changeTheme(themeID, styleGroupName, newStyleMember) {
	showLoadingMessage('Changing theme...');
	ThemesPreviewsProvider.changeTheme(themeID, styleGroupName, newStyleMember, changeThemeCallback);
}

function changeThemeCallback(result) {
	closeLoadingMessage();
	globalThemeID = result;
	if (result == null) {
		document.getElementById('themeSaveButton').disabled = true;
	}
	else {
		document.getElementById('themeSaveButton').disabled = false;
	}
}

function saveTheme() {
	if (globalThemeID != null) {
		showLoadingMessage('Saving theme...');
		ThemesPreviewsProvider.saveTheme(globalThemeID, saveThemeCallback);
	}
}

function saveThemeCallback(result) {
	closeLoadingMessage();
}