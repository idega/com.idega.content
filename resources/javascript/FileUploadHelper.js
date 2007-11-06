function uploadFiles(id, message, showProgressBar, showMessage, zipFile, invalidTypeMessage, formId, actionAfterUpload) {
	var inputs = getInputsForUpload(id);
	var files = getFilesValuesToUpload(inputs, zipFile, invalidTypeMessage);
	if (files.length == 0) {
		return false;
	}
	
	var form = null;
	if (formId != null) {
		form = document.getElementById(formId);
	}
	if (form == null) {
		form = findElementInPage(document.getElementById(id), 'FORM');
	}
	if (form == null) {
		return false;
	}
	if (!form.id) {
		var tempFormId = new Date().getTime() + '_uploadForm';
		form.setAttribute('id', tempFormId);
		formId = tempFormId;
	}
	form.setAttribute('enctype', 'multipart/form-data');
	
	if (showMessage) {
		showLoadingMessage(message);
	}
	
	YAHOO.util.Connect.setForm(formId, true);
	var uploadHandler = {
		upload: function(o) {
			closeAllLoadingMessages();
			
			for (var i = 0; i < inputs.length; i++) {
				inputs[i].setAttribute('value', '');
			}
			
			executeUserDefinedActionsAfterUploadFinished(actionAfterUpload);
		}
	};
	YAHOO.util.Connect.asyncRequest('POST', '/servlet/ContentFileUploadServlet', uploadHandler);
}

function findElementInPage(container, tagName) {
	if (container == null || tagName == null) {
		return null;
	}
	
	if (container.tagName) {
		if (container.tagName == tagName) {
			return container;
		}
	}
	
	var children = container.childNodes;
	if (children != null) {
		var element = null;
		for (var i = 0; i < children.length; i++) {
			element = children[i];
			if (element.tagName) {
				if (element.tagName == tagName) {
					return element;
				}
			}
		}
	}
	
	return findElementInPage(container.parentNode, tagName);
}

function executeUserDefinedActionsAfterUploadFinished(actionAfterUpload) {
	if (actionAfterUpload != null) {
		var customFunction = function() {
			window.eval(actionAfterUpload);
		}
		customFunction();
	}
}

function removeFileInput(id, message) {
	var confirmed = window.confirm(message);
	if (confirmed) {
		var container = document.getElementById(id);
		var parentContainer = container.parentNode;
		parentContainer.removeChild(container);
	}
}

function getInputsForUpload(id) {
	return getElementsByClassName(document.getElementById(id), 'input', 'fileUploadInputStyle');
}

function getFilesValuesToUpload(inputs, zipFile, invalidTypeMessage) {
	var files = new Array();
	for (var i = 0; i < inputs.length; i++) {
		var file = document.getElementById(inputs[i].id);
		var fileValue = file.value;
		if (fileValue != null && fileValue != '') {
			if (zipFile) {
				if (isCorrectFileType(file.id, 'zip', null, invalidTypeMessage)) {
					files.push(fileValue);
				}
			}
			else {
				files.push(fileValue);
			}
		}
	}
	
	return files;
}

function addFileInputForUpload(id, message) {
	showLoadingMessage(message);
	
	FileUploader.getRenderedFileInput({
		callback: function(component) {
			closeAllLoadingMessages();
			
			var container = document.getElementById(id);
			if (component == null || container == null) {
				return false;
			}
			
			insertNodesToContainer(component, container);
		}
	});
}