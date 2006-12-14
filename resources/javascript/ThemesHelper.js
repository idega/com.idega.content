var PAGE_ID = null;

function getPageID() {
	return PAGE_ID;
}

function setPageID(ID) {
	PAGE_ID = ID;
	ThemesEngine.setPageId(ID, nothingToDo);
}

function nothingToDo(parameter) {
}

function changePageTitleCallback(result) {
	if (result == null) {
		return;
	}
	var pageUri = document.getElementById("pageUri");
	if (pageUri != null) {
		pageUri.value = result;
	}
}

function changePageTitleInPageInfo(title) {
	if (title == null) {
		return;
	}
	var element = document.getElementById("pageTitle");
	if (element == null) {
		return;
	}
	element.value = title;
}