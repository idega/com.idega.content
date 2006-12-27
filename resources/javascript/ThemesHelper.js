var PAGE_ID = null;

var TOTAL_WIDTH = 0;
var TOTAL_HEIGHT = 0;

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

function getTotalWidth() {
	if (TOTAL_WIDTH != 0) {
		return TOTAL_WIDTH;
	}
	if(typeof(window.innerWidth) == "number") {
		TOTAL_WIDTH = window.innerWidth; // Non-IE
	} else if(document.documentElement && document.documentElement.clientWidth) {
		TOTAL_WIDTH = document.documentElement.clientWidth; // IE 6+ in 'standards compliant mode'
	} else if(document.body && document.body.clientWidth) {
		TOTAL_WIDTH = document.body.clientWidth; // IE 4 compatible
	}
	return TOTAL_WIDTH;
}

function getTotalHeight() {
	if (TOTAL_HEIGHT != 0) {
		return TOTAL_HEIGHT;
	}
	if(typeof(window.innerHeight) == "number") {
		TOTAL_HEIGHT = window.innerHeight; // Non-IE
	} else if(document.documentElement && document.documentElement.clientHeight) {
		TOTAL_HEIGHT = document.documentElement.clientHeight; // IE 6+ in 'standards compliant mode'
	} else if(document.body && document.body.clientHeight) {
		TOTAL_HEIGHT = document.body.clientHeight; // IE 4 compatible
	}
	return TOTAL_HEIGHT;
}

function getRealContainerByStyle(containerID, styleClass) {
	var container = document.getElementById(containerID);
	if (container == null) {
		return;
	}
	var children = container.childNodes;
	if (children == null) {
		return;
	}
	var realContainer = null;
	var found = false;
	for (var i = 0; (i < children.length && !found); i++) {
		realContainer = children[i];
		if (realContainer != null) {
			if (realContainer.className == styleClass) {
				found = true;
			}
		}
	}
	return realContainer;
}

function resizeContainer(containerID, styleClass, usedSpace, changeHeight) {
	var realContainer = getRealContainerByStyle(containerID, styleClass);
	if (realContainer != null) {
		if (changeHeight) {
			realContainer.style.height = (getTotalHeight() - usedSpace) + "px";
		}
		else {
			realContainer.style.width = (getTotalWidth() - usedSpace) + "px";
		} 
	}
}

function checkIfNotEmptySiteTree(id) {
	if (id == null) {
		return;
	}
	var treeContainer = document.getElementById(id);
	if (treeContainer == null) {
		return;
	}
	if (treeContainer.childNodes != null) {
		if (treeContainer.childNodes.length != 0) {
			return;
		}
	}
}