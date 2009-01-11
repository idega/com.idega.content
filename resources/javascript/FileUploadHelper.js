if (!FileUploadHelper) var FileUploadHelper = {};

var UPLOADING_FILE_PROGRESS_BOX_TEXT = 'Uploading file';
var UPLOADING_FILE_PLEASE_WAIT_PROGRESS_BOX_TEXT = 'completed, please wait...';
var UPLOADING_FILE_PROGRESS_BOX_FILE_UPLOADED_TEXT = 'Upload was successfully finished.';

FileUploadHelper.uploadFiles = function(id, message, showProgressBar, showMessage, zipFile, invalidTypeMessage, formId, progressBarId, localization, actionAfterUpload,
						actionAfterCounterReset, uploadId) {
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
			
			var inputsToRemove = new Array();
			for (var i = 0; i < inputs.length; i++) {
				inputs[i].setAttribute('value', '');
				inputs[i].value = '';
				
				if (i > 0) {
					inputsToRemove.push(inputs[i]);
				}
			}
			jQuery.each(inputsToRemove, function() {
				var input = jQuery(this);
				input.parent().hide('normal', function() {
					input.parent().remove();
				})
			});
			
			executeUserDefinedActionsAfterUploadFinished(actionAfterUpload);
		}
	};
	
	FileUploadListener.resetFileUploaderCounters({
		callback: function(result) {
			YAHOO.util.Connect.setForm(formId, true);
			YAHOO.util.Connect.asyncRequest('POST', '/servlet/ContentFileUploadServlet', uploadHandler);
			
			if (showProgressBar) {
				jQuery('#' + progressBarId).parent().hide('fast', function() {
					jQuery('#' + progressBarId).progressBar(0, { showText: true});
					showUploadInfoInProgressBar(progressBarId, actionAfterCounterReset);
				});
			}
		}
	});
}

function showUploadInfoInProgressBar(progressBarId, actionAfterCounterReset) {
	jQuery('#' + progressBarId).parent().show('normal', function() {
		fillProgressBoxWithFileUploadInfo(progressBarId, actionAfterCounterReset);
	});
}

function fillProgressBoxWithFileUploadInfo(progressBarId, actionAfterCounterReset) {
	FileUploadListener.getFileUploadStatus({
		callback: function(status) {
			if (status == null) {
				status = '0';
			}
			
			jQuery('#' + progressBarId).progressBar(status);

			if (status == '100') {
				jQuery('#' + progressBarId).html(UPLOADING_FILE_PROGRESS_BOX_FILE_UPLOADED_TEXT);
				var functionAfterCompletedUpload = function() {
					resetFileUploaderCounterAfterTimeOut(progressBarId, actionAfterCounterReset);
				}
				window.setTimeout(functionAfterCompletedUpload, 2000);
				return false;
			}
			else {
				var functionWhileUploading = function() {
					fillProgressBoxWithFileUploadInfo(progressBarId, actionAfterCounterReset);
				}
				window.setTimeout(functionWhileUploading, 750);
			}
		}
	});
}

function resetFileUploaderCounterAfterTimeOut(progressBarId, customActionAfterCounterReset) {
	FileUploadListener.resetFileUploaderCounters({
		callback:function(result) {
			jQuery('#' + progressBarId).hide('normal', function() {
				var parentContainer = jQuery('#' + progressBarId).parent();
				jQuery('#' + progressBarId).remove();
				
				jQuery(parentContainer).hide('fast', function() {
					jQuery(parentContainer).append('<span id=\''+progressBarId+'\' class=\'progressBar\' />');
					jQuery('#' + progressBarId).progressBar(0, { showText: true});
					executeUserDefinedActionsAfterUploadFinished(customActionAfterCounterReset);
				});
			});	
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
	var confirmed = false;
	var value = document.getElementById(id).getValue();
	if (value == null || value == '') {
		confirmed = true;
	}
	
	if (!confirmed) {
		confirmed = window.confirm(message);
	}
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

function addFileInputForUpload(id, message, className, showProgressBar, addjQuery, autoAddFileInput) {
	var foundEmptyInput = false;
	var currentInputs = $$('input.' + className, id);
	if (currentInputs != null) {
		var valueProperty = null;
		for (var i = 0; (i < currentInputs.length && !foundEmptyInput); i++) {
			valueProperty = $(currentInputs[i]).getProperty('value');
			foundEmptyInput = (valueProperty == null || valueProperty == '');
		}
	}
	if (foundEmptyInput) {
		return;
	}
	
	showLoadingMessage(message);
	
	FileUploader.getRenderedFileInput(id, showProgressBar, addjQuery, autoAddFileInput, {
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
	jQuery.each(jQuery('input.' + className), function() {
		jQuery(this).attr('value', newUploadPath);
	});
}

FileUploadHelper.reRenderComponent = function(id) {
	FileUploader.getRenderedComponent(id, {
		callback: function(componentHTML) {
			if (componentHTML == null) {
				reloadPage();
				return false;
			}
			
			var componentToReplace = jQuery('#' + id);
			if (componentToReplace == null || componentToReplace.length == 0) {
				reloadPage();
				return false;
			}
			
			componentToReplace.replaceWith(jQuery(componentHTML));
		}
	});
}