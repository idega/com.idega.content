var SITE_INFO_LANGUAGE = null;
var SITE_INFO_KEYWORDS = null;

function setActiveLanguage() {
	setLanguageForSiteInfo(document.getElementById('siteInfoLocale'));
}

function setLanguageForSiteInfo(select) {
	if (select == null) {
		return false;
	}
	if (select.options == null) {
		return false;
	}
	var selected = select.options[select.selectedIndex];
	if (selected == null) {
		return false;
	}
	if (selected.value == null) {
		return false;
	}
	
	SITE_INFO_LANGUAGE = selected.value;
}

function getValues(object) {
	if (object != null) {
		setLanguageForSiteInfo(object);
	}
	if (SITE_INFO_KEYWORDS == null) {
		ThemesEngine.getSiteInfoElements(getSiteInfoElementsCallback);
	}
	else {
		getSiteInfoElementsCallback(SITE_INFO_KEYWORDS);
	}
}

function getSiteInfoElementsCallback(keywords) {
	SITE_INFO_KEYWORDS = keywords;
	if (SITE_INFO_LANGUAGE != null && SITE_INFO_KEYWORDS != null) {
		ThemesEngine.getSiteInfoValues(SITE_INFO_KEYWORDS, SITE_INFO_LANGUAGE, getSiteInfoValuesCallback);
	}
}

function getSiteInfoValuesCallback(values) {
	if (SITE_INFO_KEYWORDS == null || values == null) {
		return false;
	}
	if (SITE_INFO_KEYWORDS.length != values.length) {
		return false;
	}
	
	var element = null;
	for (var i = 0; i < SITE_INFO_KEYWORDS.length; i++) {
		element = document.getElementById(SITE_INFO_KEYWORDS[i]);
		if (element != null) {
			element.value = values[i];
		}
	}
}

function saveSiteInfo() {
	showLoadingMessage(getThemeSavingText());
	if (SITE_INFO_KEYWORDS == null) {
		ThemesEngine.getSiteInfoElements(proceedSaving);
	}
	else {
		proceedSaving(SITE_INFO_KEYWORDS);
	}
}

function getElementByClassName(elementName, style) {
	if (elementName == null || style == null) {
		return null;
	}
	var elements = document.getElementsByTagName(elementName);
	if (elements == null) {
		return null;
	}
	var element = null;
	var found = false;
	for (var i = 0; (i < elements.length && !found); i++) {
		element = elements[i];
		if (element.className != null) {
			if (element.className == style) {
				found = true;
			}
		}
	}
	return element;
}

function proceedSaving(keywords) {
	if (keywords == null) {
		closeAllLoadingMessages();
		return false;
	}
	if (SITE_INFO_LANGUAGE == null) {
		setActiveLanguage();
		if (SITE_INFO_LANGUAGE == null) {
			closeAllLoadingMessages();
			return false;
		}
	}
	
	SITE_INFO_KEYWORDS = keywords;
	var values = new Array();
	var element = null;
	for (var i = 0; i < SITE_INFO_KEYWORDS.length; i++) {
		element = document.getElementById(SITE_INFO_KEYWORDS[i]);
		if (element != null) {
			values.push(element.value);
			if (SITE_INFO_KEYWORDS[i] == 'mainDomainName') {
				var siteName = getElementByClassName('div', 'ws_appinfo');
				if (siteName != null) {
					var newName = document.createTextNode(element.value);
					if (siteName.firstChild == null) {
						siteName.appendChild(newName);
					}
					else {
						siteName.firstChild.data = element.value;
					}
				}
			}
		}
	}
	ThemesEngine.saveSiteInfo(SITE_INFO_LANGUAGE, SITE_INFO_KEYWORDS, values, saveSiteInfoCallback);
	return false;
}

function saveSiteInfoCallback(result) {
	if (result) {
		getPrewUrl(getPageID());
	}
	closeAllLoadingMessages();
}

function initialiazeSiteManager() {
	/*setIsSiteMap(true);
	setNeedRedirect(false);
	setActiveLanguage();
	
	resizeTreeContainerInThemes(RESERVED_HEIGHT_FOR_SITE);
	
	checkIfNotEmptySiteTree(ALL_CURRENT_SITE_STRUCTURE_TREE_ID);

	var width = getTotalWidth() - 462;
	if (width > 0) {
		var pagesTemplatesContainer = $('pagesTypesContainer');
		if (pagesTemplatesContainer) {
			pagesTemplatesContainer.setStyle('width', width + 'px');
		}
		
		var siteTemplatesContainer = $('siteTemplatesContainer');
		if (siteTemplatesContainer) {
			siteTemplatesContainer.setStyle('width', width + 'px');
		}
	}*/
}

function registerSiteActions() {
    registerActionsForSiteTree();
    boldCurrentTreeElement();
}