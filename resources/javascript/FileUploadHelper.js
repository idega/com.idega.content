if (!FileUploadHelper) var FileUploadHelper = {};

var UPLOADING_FILE_PROGRESS_BOX_TEXT = 'Uploading file';
var UPLOADING_FILE_PLEASE_WAIT_PROGRESS_BOX_TEXT = 'completed, please wait...';
var UPLOADING_FILE_PROGRESS_BOX_FILE_UPLOADED_TEXT = 'Upload was successfully finished.';

function uploadFiles(id, message, showProgressBar, showMessage, zipFile, invalidTypeMessage, formId, progressBarId, localization, actionAfterUpload,
						actionAfterCounterReset) {
	if (localization != null) {
		if (localization.length == 3) {
			UPLOADING_FILE_PROGRESS_BOX_TEXT = localization[0];
			UPLOADING_FILE_PLEASE_WAIT_PROGRESS_BOX_TEXT = localization[1];
			UPLOADING_FILE_PROGRESS_BOX_FILE_UPLOADED_TEXT = localization[2];
		}
	}
	
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
	
	var uploadHandler = {
		upload: function(o) {
			closeAllLoadingMessages();
			
			for (var i = 0; i < inputs.length; i++) {
				inputs[i].setAttribute('value', '');
				inputs[i].value = '';
			}
			
			executeUserDefinedActionsAfterUploadFinished(actionAfterUpload);
		}
	};
	
	FileUploadListener.resetFileUploaderCounters({
		callback: function(result) {
			YAHOO.util.Connect.setForm(formId, true);
			YAHOO.util.Connect.asyncRequest('POST', '/servlet/ContentFileUploadServlet', uploadHandler);
			
			if (showProgressBar) {
				showUploadInfoInProgressBar(progressBarId, actionAfterCounterReset);
			}
		}
	});
}

function showUploadInfoInProgressBar(progressBarId, actionAfterCounterReset) {
	document.getElementById(progressBarId).style.visibility = 'visible';	
	fillProgressBoxWithFileUploadInfo(progressBarId, actionAfterCounterReset);
}

function fillProgressBoxWithFileUploadInfo(progressBarId, actionAfterCounterReset) {
	FileUploadListener.getFileUploadStatus({
		callback: function(status) {
			var textBox = document.getElementById(progressBarId + '_progressText');
			
			if (status == null) {
				status = '0';
			}

			if (status == '100') {
				textBox.innerHTML = UPLOADING_FILE_PROGRESS_BOX_FILE_UPLOADED_TEXT;
				var functionAfterCompletedUpload = function() {
					resetFileUploaderCounterAfterTimeOut(progressBarId, actionAfterCounterReset);
				}
				window.setTimeout(functionAfterCompletedUpload, 2000);
				return false;
			}
			else {
				textBox.innerHTML = UPLOADING_FILE_PROGRESS_BOX_TEXT + ': ' + status + '% ' + UPLOADING_FILE_PLEASE_WAIT_PROGRESS_BOX_TEXT;
				var functionWhileUploading = function() {
					fillProgressBoxWithFileUploadInfo(progressBarId, actionAfterCounterReset);
				}
				window.setTimeout(functionWhileUploading, 1000);
			}
		}
	});
}

function resetFileUploaderCounterAfterTimeOut(progressBarId, customActionAfterCounterReset) {
	FileUploadListener.resetFileUploaderCounters({
		callback:function(result) {
			document.getElementById(progressBarId).style.visibility = 'hidden';
			executeUserDefinedActionsAfterUploadFinished(customActionAfterCounterReset);
		}
	});
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
	
	FileUploader.getRenderedFileInput(id, {
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

FileUploadHelper.changeUploadPath = function(newUploadPath, className) {
	var uploadPathInputs = $$('input.' + className);
	if (uploadPathInputs == null || uploadPathInputs.length == 0) {
		return false;
	}
	
	var input = null;
	for (var i = 0; i < uploadPathInputs.length; i++) {
		input = $(uploadPathInputs[i]);
		input.setProperty('value', newUploadPath);
	}
}