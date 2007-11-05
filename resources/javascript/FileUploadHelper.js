function uploadFiles(id, message, showProgressBar, showMessage, zipFile, invalidTypeMessage, formId, actionAfterUpload) {
	var inputs = getInputsForUpload(id);
	var files = getFilesValuesToUpload(inputs, zipFile, invalidTypeMessage);
	if (files.length == 0) {
		return false;
	}
	
	if (showMessage) {
		showLoadingMessage(message);
	}
	
	var form = document.getElementById(formId);
	if (form == null) {
		formId = 'uploadForm';
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