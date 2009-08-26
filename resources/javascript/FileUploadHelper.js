if (!FileUploadHelper) var FileUploadHelper = {};

FileUploadHelper.allUploadedFiles = [];
FileUploadHelper.uploadedFiles = null;

FileUploadHelper.properties = {
	id: null,
	showProgressBar: true,
	showMessage: true,
	zipFile: false,
	formId: null,
	progressBarId: null,
	localizations: {
		UPLOADING_FILE_PROGRESS_BOX_TEXT: 'Uploading file',
		UPLOADING_FILE_PLEASE_WAIT_PROGRESS_BOX_TEXT: 'completed, please wait...',
		UPLOADING_FILE_PROGRESS_BOX_FILE_UPLOADED_TEXT: 'Upload was successfully finished.',
		UPLOADING_FILE_MESSAGE: 'Uploading...',
		UPLOADING_FILE_INVALID_TYPE_MESSAGE: 'Unsupported file type! Only zip files allowed'
	},
	actionAfterUpload: null,
	actionAfterCounterReset: null,
	uploadId: null,
	autoUpload: false,
	showUploadedFiles: false
}

FileUploadHelper.setProperties = function(properties) {
	FileUploadHelper.properties = properties;
}

FileUploadHelper.uploadFiles = function() {
	var inputs = getInputsForUpload(FileUploadHelper.properties.id);
	var files = getFilesValuesToUpload(inputs, FileUploadHelper.properties.zipFile, FileUploadHelper.properties.localizations.UPLOADING_FILE_INVALID_TYPE_MESSAGE);
	if (files.length == 0) {
		return false;
	}
	
	var form = null;
	if (FileUploadHelper.properties.formId != null) {
		form = document.getElementById(FileUploadHelper.properties.formId);
	}
	if (form == null) {
		form = findElementInPage(document.getElementById(FileUploadHelper.properties.id), 'FORM');
	}
	if (form == null) {
		return false;
	}
	if (!form.id) {
		var tempFormId = 'id' + new Date().getTime() + '_uploadForm';
		form.setAttribute('id', tempFormId);
		FileUploadHelper.properties.formId = tempFormId;
	}
	form.setAttribute('enctype', 'multipart/form-data');
	
	if (FileUploadHelper.properties.showMessage || FileUploadHelper.properties.autoUpload) {
		showLoadingMessage(FileUploadHelper.properties.localizations.UPLOADING_FILE_MESSAGE);
	}
	
	var uploadHandler = {
		upload: function(o) {
			closeAllLoadingMessages();
			
			FileUploadHelper.uploadedFiles = files;
			if (FileUploadHelper.uploadedFiles != null) {
				for (var i = 0; i < FileUploadHelper.uploadedFiles.length; i++) {
					FileUploadHelper.allUploadedFiles.push(FileUploadHelper.getRealUploadedFile(FileUploadHelper.uploadedFiles[i]));
				}
			}
			
			executeUserDefinedActionsAfterUploadFinished(FileUploadHelper.properties.actionAfterUpload);
			
			if (FileUploadHelper.properties.showUploadedFiles) {
				FileUploadHelper.showUploadedFiles();
			}
			
			FileUploadHelper.uploadedFiles = null;
			var inputsToRemove = new Array();
			for (var i = 0; i < inputs.length; i++) {
				jQuery(inputs[i]).attr('value', '');
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
		}
	};
	
	var progressBarId = FileUploadHelper.properties.progressBarId;
	FileUploadListener.resetFileUploaderCounters({
		callback: function(result) {
			YAHOO.util.Connect.setForm(FileUploadHelper.properties.formId, true);
			YAHOO.util.Connect.asyncRequest('POST', '/servlet/ContentFileUploadServlet', uploadHandler);
			
			if (FileUploadHelper.properties.showProgressBar) {
				jQuery('#' + progressBarId).parent().hide('fast', function() {
					jQuery('#' + progressBarId).progressBar(0, { showText: true});
					jQuery('#' + progressBarId).css('display', 'block');
					showUploadInfoInProgressBar(progressBarId, FileUploadHelper.properties.actionAfterCounterReset);
				});
			}
		}
	});
}

FileUploadHelper.showUploadedFiles = function() {
	var filesList = jQuery('div.fileUploadViewerUploadedFilesContainerStyle');
	if (filesList == null || filesList.length == 0) {
		jQuery('div.fileUploadViewerMainLayerStyle').append('<div class=\'spacer\'/><div class=\'fileUploadViewerUploadedFilesContainerStyle\' />');
		filesList = jQuery('div.fileUploadViewerUploadedFilesContainerStyle');
	}
	
	var uploadPath = FileUploadHelper.getUploadPath();
	FileUploader.getUploadedFilesList(FileUploadHelper.allUploadedFiles, uploadPath, {
		callback: function(component) {
			if (component == null) {
				return;
			}
			
			filesList.hide('fast', function() {
				filesList.empty().append(component).show('fast');
			});
		}
	});
}

FileUploadHelper.deleteUploadedFile = function(id, file) {
	FileUploader.deleteFile(file, {
		callback: function(result) {
			if (result == null) {
				return;
			}
			
			if (result.id == 'false') {
				humanMsg.displayMsg(result.value);
				return;
			}
			
			humanMsg.displayMsg(result.value);
			var container = jQuery('#' + id).parent();
			jQuery('#' + id).hide('normal', function() {
				jQuery('#' + id).remove();
				if (jQuery('li', container).length == 0) {
					container.parent().remove();
				}
			});
			
			removeElementFromArray(FileUploadHelper.allUploadedFiles, file);
		}
	});
}

FileUploadHelper.getUploadPath = function() {
	return jQuery('input.web2FileUploaderPathValue[type=\'hidden\'][name=\'web2FileUploaderPathValue\']').attr('value');
}

FileUploadHelper.getRealUploadedFile = function(file) {
	var uploadPath = FileUploadHelper.getUploadPath();
	var index = file.lastIndexOf('/');
	if (index != -1) {
		file = file.substring(index + 1);
	}
	index = file.lastIndexOf('\\');
	if (index != -1) {
		file = file.substring(index + 1);
	}
	
	return uploadPath + file;
}

FileUploadHelper.removeAllUploadedFiles = function() {
	if (FileUploadHelper.allUploadedFiles == null || FileUploadHelper.allUploadedFiles.length == 0) {
		return;
	}
	
	LazyLoader.loadMultiple(['/dwr/engine.js', '/dwr/interface/FileUploader.js'], function() {
		FileUploader.deleteFiles(FileUploadHelper.allUploadedFiles, {
			callback: function(result) {
				if (result == null) {
					return;
				}
				
				if (result.id == 'false') {
					humanMsg.displayMsg(result.value);
					return;
				}
				
				jQuery('div.fileUploadViewerUploadedFilesContainerStyle').hide('fast', function() {
					jQuery('div.fileUploadViewerUploadedFilesContainerStyle').remove();
				});
				FileUploadHelper.allUploadedFiles = [];
			}
		});
	}, null);
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
				jQuery('#' + progressBarId).html(FileUploadHelper.properties.localizations.UPLOADING_FILE_PROGRESS_BOX_FILE_UPLOADED_TEXT);
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
					jQuery(parentContainer).append('<span id=\''+progressBarId+'\' class=\'progressBar\' style=\'display: none;\'/>');
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
	var inputs = new Array();
	jQuery.each(jQuery('input.fileUploadInputStyle', jQuery('#' + id)), function() {
		inputs.push(this);
	});
	return inputs;
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

function addFileInputForUpload(id, message, className, showProgressBar, addjQuery, autoAddFileInput, autoUpload) {
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
	
	FileUploader.getRenderedFileInput(id, showProgressBar, addjQuery, autoAddFileInput, autoUpload, {
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