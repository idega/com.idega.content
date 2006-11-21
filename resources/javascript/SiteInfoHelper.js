var LANGUAGE = null;

var KEYWORDS = null;

function setActiveLanguage() {
	setLanguageForSiteInfo(document.getElementById("siteInfoLocale"));
}

function setLanguageForSiteInfo(select) {
	if (select == null) {
		return;
	}
	if (select.options == null) {
		return;
	}
	var selected = select.options[select.selectedIndex];
	if (selected == null) {
		return;
	}
	if (selected.value == null) {
		return;
	}
	
	LANGUAGE = selected.value;
}

function getValues(object) {
	setLanguageForSiteInfo(object);
	if (KEYWORDS == null) {
		ThemesEngine.getSiteInfoElements(getSiteInfoElementsCallback);
	}
	else {
		getSiteInfoElementsCallback(KEYWORDS);
	}
}

function getSiteInfoElementsCallback(keywords) {
	KEYWORDS = keywords;
	if (LANGUAGE != null && KEYWORDS != null) {
		ThemesEngine.getSiteInfoValues(KEYWORDS, LANGUAGE, getSiteInfoValuesCallback);
	}
}

function getSiteInfoValuesCallback(values) {
	if (KEYWORDS == null || values == null) {
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

function saveSiteInfo() {
	showLoadingMessage("Saving..");
	if (KEYWORDS == null) {
		ThemesEngine.getSiteInfoElements(proceedSaving);
	}
	else {
		proceedSaving(KEYWORDS);
	}
}

function proceedSaving(keywords) {
	if (keywords == null) {
		closeLoadingMessage();
		return;
	}
	if (LANGUAGE == null) {
		closeLoadingMessage();
		return;
	}
	
	KEYWORDS = keywords;
	
	var values = new Array();
	var element = null;
	for (var i = 0; i < KEYWORDS.length; i++) {
		element = document.getElementById(KEYWORDS[i]);
		if (element != null) {
			values.push(element.value);
		}
	}
	ThemesEngine.saveSiteInfo(LANGUAGE, KEYWORDS, values, saveSiteInfoCallback);
}

function saveSiteInfoCallback(result) {
	closeLoadingMessage();
}