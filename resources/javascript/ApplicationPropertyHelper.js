var SITE_INFO_KEYWORD_FROM_BOX = null;
var CLICKED_ON_PROPERTY = false;

var PARENT_ELEMENT = null;
var CHILD_ELEMENT = null;
var CHANGED_CHILDREN_LIST = null;
var ORIGINAL_ELEMENT_INDEX = 0;

var APPLICATION_PROPERTY = "application_property";
var EDIT_BOX_ID = "changeSiteInfoBox";

var SAVING_SITE_INFO_VALUE_MESSAGE_TEXT = "Saving...";
function changeSiteInfo(id, savingSiteInfoValueMessageText, needsReload) {
	if (id == null) {
		return;
	}
	if (savingSiteInfoValueMessageText != null) {
		SAVING_SITE_INFO_VALUE_MESSAGE_TEXT = savingSiteInfoValueMessageText;
	}
	CLICKED_ON_PROPERTY = true;
	$(document).addEvent('click', function() {
		showSiteInfoValue();
	});
	changeSiteInfoValue(id, needsReload);
}

function changeSiteInfoValue(id, needsReload) {
	if (id == null) {
		return;
	}
	showSiteInfoValue();
	SITE_INFO_KEYWORD_FROM_BOX = id;
	
	var element = $(id);
	if (element == null) {
		return;
	}
	CHILD_ELEMENT = element;
	
	var editBox = $(EDIT_BOX_ID);
	if (editBox == null) {
		editBox = new Element("input");
		editBox.setAttribute("type", "input");
		editBox.setAttribute("id", EDIT_BOX_ID);
		editBox.addEvents({
			'keyup': function(e) {
				e = new Event(e);
				saveSiteInfoValueWithEnter(e, needsReload);
				e.stop();
			},
			'blur': function(e) {
				e = new Event(e);
				saveSiteInfoValueWithBlur(e, needsReload);
				e.stop();
			}
		});
	}
	else {
		editBox.value = "";
		editBox.style.display = "inline";
		var parentNode = editBox.parentNode;
		if (parentNode != null) {
			parentNode.removeChild(editBox);
		}
	}
	
	PARENT_ELEMENT = CHILD_ELEMENT.parentNode;
	ORIGINAL_ELEMENT_INDEX = findChildIndex(PARENT_ELEMENT, CHILD_ELEMENT);
	
	if ($(CHILD_ELEMENT).getTag() == 'img') {
		editBox.value = $(CHILD_ELEMENT).getProperty('src');
	}
	else {
		var children = CHILD_ELEMENT.childNodes;
		if (children != null) {
			if (children[0] != null) {
				if (children[0].nodeValue != null) {
					editBox.value = children[0].nodeValue;
				}
			}
		}
	}
	
	if (element.getAttribute(APPLICATION_PROPERTY) == null) {
		element.setAttribute(APPLICATION_PROPERTY, true);
	}

	appendEditBoxToExactPlace(element, editBox);
	editBox.focus();
	editBox.select();
	
	if (PARENT_ELEMENT == null) {
		element.style.visibility = "hidden";
	}
	else {
		PARENT_ELEMENT.removeChild(CHILD_ELEMENT);
		CHANGED_CHILDREN_LIST = new Array();
		for (var i = 0; i < PARENT_ELEMENT.childNodes.length; i++) {
			CHANGED_CHILDREN_LIST.push(PARENT_ELEMENT.childNodes[i]);
		}
	}
}


function appendEditBoxToExactPlace(element, edit) {
	if (element == null || edit == null) {
		return;
	}
	var container = null;
	if (element.tagName != null) {
		if ((element.tagName != "div" && element.tagName != "DIV") && (element.tagName != "img" && element.tagName != "IMG")) {
			container = document.createElement(element.tagName);
			container.setAttribute("id", EDIT_BOX_ID + "_container");
			container.appendChild(edit);
		}
	}
	var parentNode = element.parentNode;
	if (parentNode != null) {
		if (container != null) {
			parentNode.insertBefore(container, element);
		}
		else {
			parentNode.insertBefore(edit, element);
		}
	}
	else {
		if (container != null) {
			element.appendChild(container);
		}
		else {
			element.appendChild(edit);
		}
	}
}

function saveSiteInfoValueWithBlur(event, needsReload) {
	if (event == null) {
		return false;
	}
	if (event.type == "blur" || event.type == "onblur") {
		mainSaveSiteInfo($(EDIT_BOX_ID), needsReload);
	}
	return false;
}

function saveSiteInfoValueWithEnter(event, needsReload) {
	if (event == null) {
		return false;
	}
	
	if (event.key) {
		if ('enter' == event.key) {
			mainSaveSiteInfo($(EDIT_BOX_ID), needsReload);
		}
	}
	return false;
}

function mainSaveSiteInfo(component, needsReload) {
	if (SITE_INFO_KEYWORD_FROM_BOX == null || component == null) {
		return false;
	}
	
	var value = component.value;
	
	if (PARENT_ELEMENT == null) {
		var element = document.getElementById(SITE_INFO_KEYWORD_FROM_BOX); // Setting new value
		setElementNewValue(element, value);
	}
	else {
		setElementNewValue(CHILD_ELEMENT, value);
	}
	
	showLoadingMessage(SAVING_SITE_INFO_VALUE_MESSAGE_TEXT);
	ThemesEngine.saveSiteInfoValue(SITE_INFO_KEYWORD_FROM_BOX, value, {
		callback: function(result) {
			closeAllLoadingMessages();
			showSiteInfoValue();
			
			if (needsReload) {
				reloadPage();
			}
			
			try {
				window.parent.updateSiteInfoBoxWithNewValues();
			} catch(e) {};
		}
	});
}

function showSiteInfoValue() {
	var editBox = $(EDIT_BOX_ID);
	if (editBox != null) {
		var container = editBox.parentNode;
		if (container != null) {
			container.removeChild(editBox);
			if (container.id != null) {
				if (container.id == EDIT_BOX_ID + "_container") {
					var parentContainer = container.parentNode;
					if (parentContainer != null) {
						parentContainer.removeChild(container);
					}
				}
			}
		}
		else {
			editBox.style.display = "none";
		}
	}
	
	if (SITE_INFO_KEYWORD_FROM_BOX == null) {
		return;
	}
	
	if (PARENT_ELEMENT == null) {
		var element = $(SITE_INFO_KEYWORD_FROM_BOX);
		if (element != null) {
			if (element.style.visibility == "hidden") {
				CLICKED_ON_PROPERTY = false;
				element.style.visibility = "visible";
			}
		}
	}
	else {
		CLICKED_ON_PROPERTY = false;
		appendChildrenToExactPlace();
		PARENT_ELEMENT = null;
		CHILD_ELEMENT = null;
		CHANGED_CHILDREN_LIST = null;
	}
}

function findChildIndex(parentNode, element) {
	if (parentNode == null || element == null) {
		return -1;
	}
	var children = parentNode.childNodes;
	if (children == null) {
		return -1;
	}
	var i = 0;
	for (i = 0; i < children.length; i++) {
		if (element == children[i]) {
			return i;
		}
	}
	return i - 1;
}

function appendChildrenToExactPlace() {
	if (PARENT_ELEMENT == null || CHILD_ELEMENT == null || CHANGED_CHILDREN_LIST == null) {
		return;
	}
	var children = PARENT_ELEMENT.childNodes;
	if (children == null) {
		return;
	}
	if (ORIGINAL_ELEMENT_INDEX < 0) {
		ORIGINAL_ELEMENT_INDEX = 0;
	}
	var newChildrenList = new Array();
	for (var i = 0; i < CHANGED_CHILDREN_LIST.length; i++) { // Making copy of exact current structure
		newChildrenList.push(CHANGED_CHILDREN_LIST[i]);
	}
	newChildrenList.splice(ORIGINAL_ELEMENT_INDEX, 1, CHILD_ELEMENT); // Adding original element with new value
	for (var i = 0; i < children.length; i++) { // Removing old children
		PARENT_ELEMENT.removeChild(children[i]);
	}
	for (var i = 0; i < newChildrenList.length; i++) { // Appending new children
		PARENT_ELEMENT.appendChild(newChildrenList[i]);
	}
}

function setElementNewValue(element, value) {
	if (element == null || value == null) {
		return;
	}
	if (element.value != null) {
		element.value = value;
	}
	else {
		var children = element.childNodes;
		if (children != null) {
			for (var j = 0; j < children.length; j++) {
				element.removeChild(children[j]);
			}				
		}
		if (value == "               " || value == "" || value == " ") {
			for (var i = 0; i < 15; i++) {
				element.appendChild(document.createTextNode("\u00A0"));
			}
		}
		else {
			element.appendChild(document.createTextNode(value));
		}
	}
}