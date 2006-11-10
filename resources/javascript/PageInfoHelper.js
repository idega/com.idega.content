var globalPageID = 1;

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
		container.style.display = "block";
		button.value = "Hide Themes";
	}
	else {
		container.style.display = "none";
		button.value = "Show Themes";
	}
}

function setPageID(ID) {
	globalPageID = ID;
}