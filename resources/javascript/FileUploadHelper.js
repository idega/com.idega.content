if (!FileUploadHelper) var FileUploadHelper = {};

FileUploadHelper.allUploadedFiles = [];
FileUploadHelper.uploadedFiles = null;
FileUploadHelper.uploadingWithFrame = false;

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
		UPLOADING_FILE_INVALID_TYPE_MESSAGE: 'Unsupported file type! Only zip files allowed',
		UPLOADING_FILE_FAILED: 'Sorry, some error occurred - unable to upload file(s). Please, try again',
		UPLOADING_FILE_EXCEEDED_SIZE: 'Sorry, the size of selected file(s) is exceeding the max allowed size',
		CHOOSE_FILE: 'Choose file',
		FILES_SELECTED: 'files selected',
		SELECTED_FILE: 'Selected file',
		FLASH_IS_MISSING: 'Unable to upload file(s): you need to install Flash plug-in',
		LOADING: 'Loading...',
		MOVING_DATA_INTO_THE_PLACE: 'Preparing data...'
	},
	actionAfterUpload: null,
	actionAfterCounterReset: null,
	uploadId: null,
	autoUpload: false,
	showUploadedFiles: false,
	fakeFileDeletion: false,
	actionAfterUploadedToRepository: null,
	stripNonRomanLetters: false,
	maxSize: 20971520,	//	20 MBs
	uploadImage: null,
	sessionId: null,
	swfObject: null,
	swfUploadScript: null,
	swfUploadPlugin: null,
	needFlash: false,
	initializeScriptsAction: null,
	failure: false
}

FileUploadHelper.bytesToUpload = 0;
FileUploadHelper.bytesCompleted = 0;
FileUploadHelper.filesToUpload = 0;
FileUploadHelper.selectedFiles = [];

FileUploadHelper.setProperties = function(properties) {
	FileUploadHelper.properties = properties;
	FileUploadHelper.properties.maxSize--;
	FileUploadHelper.properties.maxSize++;
	
	jQuery('input[type=\'file\']').each(function() {
		FileUploadHelper.properties.needFlash = this.files == null;
	});
	if (IE)
		FileUploadHelper.properties.needFlash = true;
	if (!FileUploadHelper.properties.needFlash)
		return;
	
	FileUploadHelper.properties.initializeScriptsAction();
}

FileUploadHelper.initializeFlashUploader = function() {
	showLoadingMessage(FileUploadHelper.properties.localizations.LOADING);
	jQuery(jQuery('input.fileUploadAddInputStyle')).hide('fast');
	LazyLoader.loadMultiple([FileUploadHelper.properties.swfObject, FileUploadHelper.properties.swfUploadScript], function() {
		if (swfobject == null || !swfobject.hasFlashPlayerVersion("9")) {
			closeAllLoadingMessages();
			return false;
		}
		
		try {
			if (jQuery('.swfupload-control').length == 0) {
				var buttonId = "spanSWFUploadButton";
				jQuery('#' + FileUploadHelper.properties.id).append('<div class="swfupload-control"><span id="'+ buttonId +'"></span></div>');
				var settings = {
					flash_url : FileUploadHelper.properties.swfUploadPlugin,
					upload_url: "/servlet/ContentFileUploadServlet;jsessionid=" + FileUploadHelper.properties.sessionId,
					
					file_post_name: 'web2FileUploadField',
					
					file_size_limit : FileUploadHelper.properties.maxSize + " B",
					file_types : FileUploadHelper.properties.zipFile ? "*.zip" : "*.*",
					file_types_description : FileUploadHelper.properties.zipFile ? "ZIP" : "",
					file_upload_limit : 100,
					file_queue_limit : 0,
					custom_settings : {
						progressTarget : "fsUploadProgress",
						cancelButtonId : "btnCancel"
					},
					debug: false,
	
					// Button settings
					button_image_url: FileUploadHelper.properties.uploadImage,
					button_width: "180",
					button_height: "24",
					button_placeholder_id: buttonId,
					button_text: '<span class="upload_button_style">' + FileUploadHelper.properties.localizations.CHOOSE_FILE + '</span>',
					button_text_style: ".upload_button_style {font-size: 14; text-align: center;}",
					button_action: SWFUpload == null ? null : SWFUpload.BUTTON_ACTION.SELECT_FILES,
					button_cursor: SWFUpload == null ? null : SWFUpload.CURSOR.HAND,
					
					file_queued_handler: function(file) {
						if (file == null)
							return false;
						
						FileUploadHelper.bytesToUpload += file.size;
						FileUploadHelper.selectedFiles.push({id: file.id, name: file.name});
					},
					file_queue_error_handler: function(params) {
						humanMsg.displayMsg(FileUploadHelper.properties.localizations.UPLOADING_FILE_EXCEEDED_SIZE, {timeOut: 3000});
					},
					file_dialog_start_handler: function() {
					},
					file_dialog_complete_handler: function(filesToUpload) {
						if (FileUploadHelper.bytesToUpload >= FileUploadHelper.properties.maxSize) {
							return FileUploadHelper.removeAllFilesFromQueue(true);
						}
						
						FileUploadHelper.filesToUpload += filesToUpload;
						
						var uploadInfo = 'swfupload-info';
						if (jQuery('div.swfupload-info', jQuery('#' + FileUploadHelper.properties.id)).length == 0)
							jQuery('.swfupload-control', jQuery('#' + FileUploadHelper.properties.id)).append('<div id=\'' + uploadInfo +
							'\' class=\'' + uploadInfo + '\'></div>');
						else
							jQuery('div.swfupload-info', jQuery('#' + FileUploadHelper.properties.id)).empty();
						
						if (FileUploadHelper.filesToUpload == 1) {
							jQuery('div.swfupload-info', jQuery('#' + FileUploadHelper.properties.id)).html('<span>' +
								FileUploadHelper.properties.localizations.SELECTED_FILE +
								': ' + FileUploadHelper.selectedFiles[0].name + '</span>');
						} else if (FileUploadHelper.filesToUpload > 1) {
							jQuery('div.swfupload-info', jQuery('#' + FileUploadHelper.properties.id)).html('<span>' +
								FileUploadHelper.filesToUpload + ' ' +
								FileUploadHelper.properties.localizations.FILES_SELECTED + '</span>');
						}
						
						if (FileUploadHelper.properties.autoUpload && FileUploadHelper.filesToUpload > 0)
							FileUploadHelper.uploadFiles();
					},
					upload_start_handler: function(file) {
					},
					upload_progress_handler: function(file, bytesCompleted, bytesTotal) {
						var bytesUploaded = FileUploadHelper.bytesCompleted + bytesCompleted;
						var progress = bytesUploaded / FileUploadHelper.bytesToUpload * 100;
						progress = Math.round(progress);
						FileUploadHelper.updateProgressBar(progress + '', FileUploadHelper.properties.progressBarId,
							FileUploadHelper.properties.actionAfterCounterReset, false, null);
					},
					upload_error_handler: function(file, errorCode, message) {
						FileUploadHelper.bytesToUpload = 0;
						FileUploadHelper.bytesCompleted = 0;
						LazyLoader.loadMultiple(['/dwr/engine.js', '/dwr/interface/WebUtil.js'], function() {
							WebUtil.sendEmail(null, null, 'Error uploading file (name: ' + file.name + ', size: ' + file.size + ') on ' +
								window.location.href,
								'Error code: ' + errorCode + '\nmessage: ' + message);
						}, null);
						humanMsg.displayMsg(FileUploadHelper.properties.localizations.UPLOADING_FILE_FAILED, {timeOut: 3000});
					},
					upload_success_handler: function(file, serverData, receivedResponse) {
						FileUploadHelper.filesToUpload--;
						FileUploadHelper.bytesCompleted += file.size;
						
						if (FileUploadHelper.filesToUpload > 0) {
							FileUploadHelper.uploadFilesUsingFlash();
						} else {
							FileUploadHelper.selectedFiles = [];
							FileUploadHelper.bytesToUpload = 0;
							FileUploadHelper.manageResponse(serverData, getInputsForUpload(FileUploadHelper.properties.id));
						}
					},
					upload_complete_handler: function(file) {
					},
					debug_handler: function(message) {
					},
					swfupload_loaded_handler: function() {
						jQuery('input.fileUploadInputStyle').each(function() {
							jQuery(this).parent().hide('fast', function() {
								jQuery('input[type=\'hidden\']').each(function() {
									var input = jQuery(this);
									var classAttr = input == null || !input.attr ? null : input.attr('class');
									if (classAttr != null) {
										if (classAttr.indexOf('web2FileUploader') != -1) {
											FileUploadHelper.swfu.addPostParam(input.attr('name'), input.attr('value'));
										}
									}
								});
								
								jQuery(this).remove();
							});
						});
					}
				};
				FileUploadHelper.swfu = new SWFUpload(settings);
			}
			closeAllLoadingMessages();
		} catch (e) {
			jQuery('input.fileUploadInputStyle').parent().show('fast');
			FileUploadHelper.swfu = null;
			closeAllLoadingMessages();
		}
	});
}

FileUploadHelper.removeAllFilesFromQueue = function(resetNumberOfBytes) {
	for (var i = 0; i < FileUploadHelper.selectedFiles.length; i++) {
		FileUploadHelper.swfu.cancelUpload(FileUploadHelper.selectedFiles[i].id);
	}
	
	FileUploadHelper.filesToUpload = 0;
	FileUploadHelper.selectedFiles = [];
	if (resetNumberOfBytes)
		FileUploadHelper.bytesToUpload = 0;
	humanMsg.displayMsg(FileUploadHelper.properties.localizations.UPLOADING_FILE_EXCEEDED_SIZE, {timeOut: 3000});
	return false;
}

FileUploadHelper.uploadFilesUsingFlash = function() {
	try {
		FileUploadHelper.swfu.startUpload();
		return true;
	} catch (e) {
		humanMsg.displayMsg(FileUploadHelper.properties.localizations.UPLOADING_FILE_FAILED, {timeOut: 3000});
	}
	return false;
}

FileUploadHelper.prepareProgressBar = function(callback) {
	var progressBarId = FileUploadHelper.properties.progressBarId;
	if (FileUploadHelper.properties.showProgressBar) {
		jQuery('#' + progressBarId).parent().hide('fast', function() {
			jQuery('#' + progressBarId).progressBar(0, {showText: true});
			jQuery('#' + progressBarId).css('display', 'block');
			jQuery('#' + progressBarId).parent().show('normal', function() {
				if (callback)
					return callback();
			});
		});
	} else if (callback)
		return callback();
	
	return true;
}

FileUploadHelper.uploadFiles = function() {
	FileUploadHelper.bytesCompleted = 0;
	
	if (FileUploadHelper.swfu != null) {
		if (FileUploadHelper.filesToUpload == 0) {
			closeAllLoadingMessages();
			return false;
		}
		
		if (FileUploadHelper.properties.showMessage || FileUploadHelper.properties.autoUpload) {
			showLoadingMessage(FileUploadHelper.properties.localizations.UPLOADING_FILE_MESSAGE);
		}
		return FileUploadHelper.prepareProgressBar(function() {
			FileUploadHelper.uploadFilesUsingFlash();
		});
	}
	
	var inputs = getInputsForUpload(FileUploadHelper.properties.id);
	var files = getFilesValuesToUpload(inputs, FileUploadHelper.properties.zipFile,
		FileUploadHelper.properties.localizations.UPLOADING_FILE_INVALID_TYPE_MESSAGE);
	if (files.length == 0) {
		return false;
	}
	
	var totalSize = 0;
	for (var i = 0; i < inputs.length; i++) {
		var fileInput = inputs[i];
		if (fileInput == null)
			return false;
			
		var filesByInput = fileInput.files;
		if (filesByInput == null || filesByInput.length == 0) {
			//	Not HTML 5 browser!
		} else {
			for (var j = 0; j < filesByInput.length; j++) {
				var file = filesByInput[j];
				totalSize += file.size;
				if (totalSize >= FileUploadHelper.properties.maxSize) {
					humanMsg.displayMsg(FileUploadHelper.properties.localizations.UPLOADING_FILE_EXCEEDED_SIZE, {timeOut: 3000});
					return false;
				}
			}
		}
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
	
	FileUploadHelper.properties.failure = false;
	var uploadHandler = {
		upload: function(o) {
			FileUploadHelper.manageResponse(o == null ? null : o.responseText, inputs);
		},
		failure: function(o) {
			FileUploadHelper.properties.failure = true;
			FileUploadHelper.manageResponse(null, inputs);
		}
	};
	
	var fileItemNumber = FileUploadHelper.getFileElementNumber(FileUploadHelper.properties.formId);
	FileUploadListener.resetFileUploaderCounters(FileUploadHelper.properties.uploadId, FileUploadHelper.properties.maxSize, {
		callback: function(result) {
			var utilError = false;
			try {
				YAHOO.util.Connect.setForm(FileUploadHelper.properties.formId, true);
				YAHOO.util.Connect.asyncRequest('POST', '/servlet/ContentFileUploadServlet?fileItem=' + fileItemNumber, uploadHandler);
			} catch (e) {
				utilError = true;
			}
			if (utilError) {
				FileUploadHelper.uploadingWithFrame = true;
				jQuery(document.body).append('<iframe style="display: none;" name="uploadFrame">iframe</iframe>');
				form.enctype = 'multipart/form-data';
				form.action = '/servlet/ContentFileUploadServlet?fileItem=' + fileItemNumber;
				form.method = 'post';
				form.target = 'uploadFrame';
				form.submit();
			}
			
			FileUploadHelper.prepareProgressBar(function() {
				showUploadInfoInProgressBar(FileUploadHelper.properties.progressBarId, FileUploadHelper.properties.actionAfterCounterReset);
			});
		}
	});
}

FileUploadHelper.getFileElementNumber = function(containerId) {
	var allElements = jQuery(':input', jQuery('#' + containerId));
	var needless = 0;
	for (var index = 0; index < allElements.length; index++) {
		var element = allElements[index];
		if (element.name == 'web2FileUploadField')
			return index - needless;
		
		if ((element.type == 'checkbox' || element.type == 'radio') && !element.checked)
			needless++;
		else if (element.type == 'submit')
			needless++;
	}
	return -1;
}

FileUploadHelper.manageResponse = function(response, inputs) {
	if (response == null) {
		closeAllLoadingMessages();
		FileUploadHelper.properties.failure = true;
		humanMsg.displayMsg(FileUploadHelper.properties.localizations.UPLOADING_FILE_FAILED, {timeOut: 3000});
		jQuery('#' + FileUploadHelper.properties.progressBarId).parent().hide('fast');
		return false;
	}

	if (response != null) {
		var key = 'web2FilesUploaderFilesListStarts';
		response = response.substring(response.indexOf(key) + key.length);
		response = response.replace('</pre>', '');
		
		FileUploadHelper.uploadedFiles = response.split(',');
		if (FileUploadHelper.uploadedFiles == null) {
			humanMsg.displayMsg(FileUploadHelper.properties.localizations.UPLOADING_FILE_FAILED);
			return;
		} else if (FileUploadHelper.uploadedFiles[0].indexOf('error=') != -1) {
			var firstValue = FileUploadHelper.uploadedFiles[0];
			var customMessage = firstValue.substr('error='.length);
			for (var i = 1; i < FileUploadHelper.uploadedFiles.length; i++) {
				customMessage += FileUploadHelper.uploadedFiles[i];
				if (i + 1 < FileUploadHelper.uploadedFiles.length)
					customMessage += ',';
			}
			closeAllLoadingMessages();
			humanMsg.displayMsg(customMessage);
			return;
		} else {
			for (var i = 0; i < FileUploadHelper.uploadedFiles.length; i++) {
				FileUploadHelper.allUploadedFiles.push(FileUploadHelper.getRealUploadedFile(FileUploadHelper.uploadedFiles[i]));
			}
		}
	}
	
	executeUserDefinedActionsAfterUploadFinished(FileUploadHelper.properties.actionAfterUpload);
			
	FileUploadHelper.executeActionAfterUploadedToRepository(inputs);
}

FileUploadHelper.executeActionAfterUploadedToRepository = function(inputs) {
	if (FileUploadHelper.properties.uploadId == null) {
		FileUploadHelper.prepareToShowUploadedFiles(inputs);
		
		closeAllLoadingMessages();
		return;
	}
	
	FileUploadListener.isUploadSuccessful(FileUploadHelper.properties.uploadId, {
		callback: function(result) {
			if (result) {
				FileUploadHelper.prepareToShowUploadedFiles(inputs);
				
				closeAllLoadingMessages();
				executeUserDefinedActionsAfterUploadFinished(FileUploadHelper.properties.actionAfterUploadedToRepository);
			} else if (result == null) {
				window.setTimeout(FileUploadHelper.executeActionAfterUploadedToRepository, 250);
			} else {
				closeAllLoadingMessages();
				humanMsg.displayMsg(FileUploadHelper.properties.localizations.UPLOADING_FILE_FAILED);
			}
		}
	});
}

FileUploadHelper.prepareToShowUploadedFiles = function(inputs) {
	if (FileUploadHelper.properties.showUploadedFiles) {
		FileUploadHelper.showUploadedFiles(FileUploadHelper.properties.fakeFileDeletion);
	}
		
	FileUploadHelper.uploadedFiles = null;
	var inputsToRemove = new Array();
	if (inputs != null) {
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
}

FileUploadHelper.showUploadedFiles = function(fakeFileDeletion) {
	var filesList = jQuery('div.fileUploadViewerUploadedFilesContainerStyle');
	if (filesList == null || filesList.length == 0) {
		jQuery('div.fileUploadViewerMainLayerStyle').append('<div class=\'spacer\'/><div class=\'fileUploadViewerUploadedFilesContainerStyle\' />');
		filesList = jQuery('div.fileUploadViewerUploadedFilesContainerStyle');
	}
	
	var filesListCallback = function(results) {
		if (results == null) {
			return;
		}
		
		var component = results[results.length - 1];
		filesList.hide('fast', function() {
			filesList.empty().append(component).show('fast');
		});
	}
	
	var uploadPath = FileUploadHelper.getUploadPath();
	if (FileUploadHelper.properties.uploadId == null) {
		FileUploader.getUploadedFilesList(FileUploadHelper.allUploadedFiles, uploadPath, fakeFileDeletion,
			FileUploadHelper.properties.stripNonRomanLetters, {
			callback: function(results) {
				filesListCallback(results);
			}
		});
	} else {
		FileUploader.getUploadedFilesListById(FileUploadHelper.properties.uploadId, uploadPath, fakeFileDeletion,
			FileUploadHelper.properties.stripNonRomanLetters, {
			callback: function(results) {
				filesListCallback(results);
			}
		});
	}
}

FileUploadHelper.deleteUploadedFile = function(id, file, fakeFileDeletion, callback) {
	
	LazyLoader.loadMultiple(['/dwr/engine.js', '/dwr/interface/FileUploader.js'], function() {
		FileUploader.deleteFile(file, fakeFileDeletion, {
			callback: function(result) {

				removeElementFromArray(FileUploadHelper.allUploadedFiles, file);

				if (callback) {
					callback(result);

				} else {
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
				}

				
			}
		});
	});
}

FileUploadHelper.getUploadPath = function() {
	return jQuery('input.web2FileUploaderPathValue[type=\'hidden\'][name=\'web2FileUploaderPathValue\']').attr('value');
}

FileUploadHelper.setUploadPath = function(path) {
	jQuery('input.web2FileUploaderPathValue[type=\'hidden\'][name=\'web2FileUploaderPathValue\']').attr('value', path);
}

FileUploadHelper.getRealUploadedFile = function(file) {
	var uploadPath = FileUploadHelper.getUploadPath();
	if (uploadPath.substring(uploadPath.length - 1) != '/') {
		uploadPath += '/';
	}
	
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

FileUploadHelper.removeAllUploadedFiles = function(fakeFileDeletion) {
	if (FileUploadHelper.allUploadedFiles == null || FileUploadHelper.allUploadedFiles.length == 0) {
		return;
	}
	
	LazyLoader.loadMultiple(['/dwr/engine.js', '/dwr/interface/FileUploader.js'], function() {
		FileUploader.deleteFiles(FileUploadHelper.allUploadedFiles, fakeFileDeletion, {
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
	fillProgressBoxWithFileUploadInfo(progressBarId, actionAfterCounterReset, null);
}

function fillProgressBoxWithFileUploadInfo(progressBarId, actionAfterCounterReset, intervalId) {
	FileUploadListener.getFileUploadStatus(FileUploadHelper.properties.uploadId, {
		callback: function(status) {
			FileUploadHelper.updateProgressBar(FileUploadHelper.properties.failure ? '-1' : status, progressBarId, actionAfterCounterReset, true,
			intervalId);
		}
	});
}

FileUploadHelper.updateProgressBar = function(progress, progressBarId, actionAfterCounterReset, callAfterTimeOut, intervalId) {
	if (progress == null || progress == 0)
		progress = '0';
	
	jQuery('#' + progressBarId).progressBar(progress == '-1' ? '0' : progress);

	if (progress == '100') {
		if (intervalId != null)
			window.clearInterval(intervalId);
		
		FileUploadHelper.reportUploadStatus(progressBarId, actionAfterCounterReset,
		FileUploadHelper.properties.localizations.UPLOADING_FILE_PROGRESS_BOX_FILE_UPLOADED_TEXT);
		if (FileUploadHelper.uploadingWithFrame) {
			FileUploadHelper.uploadingWithFrame = false;
			FileUploadHelper.executeActionAfterUploadedToRepository(getInputsForUpload(FileUploadHelper.properties.id));
		}
		return false;
	} else if (progress == '-1') {
		if (intervalId != null)
			window.clearInterval(intervalId);
		FileUploadHelper.reportUploadStatus(progressBarId, actionAfterCounterReset, FileUploadHelper.properties.localizations.UPLOADING_FILE_FAILED);
		return false;
	} else if (callAfterTimeOut) {
		var functionWhileUploading = function() {
			fillProgressBoxWithFileUploadInfo(progressBarId, actionAfterCounterReset, intervalId);
		}
		if (intervalId == null)
			intervalId = window.setInterval(functionWhileUploading, 750);
	}
}

FileUploadHelper.reportUploadStatus = function(progressBarId, actionAfterCounterReset, text) {
	jQuery('#' + progressBarId).html(text);
	var functionAfterCompletedUpload = function() {
		resetFileUploaderCounterAfterTimeOut(progressBarId, actionAfterCounterReset);
	}
	window.setTimeout(functionAfterCompletedUpload, 2000);
}

function resetFileUploaderCounterAfterTimeOut(progressBarId, customActionAfterCounterReset) {
	FileUploadListener.resetFileUploaderCounters(FileUploadHelper.properties.uploadId, FileUploadHelper.properties.maxSize, {
		callback:function(result) {
			jQuery('#' + progressBarId).hide('normal', function() {
				var parentContainer = jQuery('#' + progressBarId).parent();
				jQuery('#' + progressBarId).remove();
				
				jQuery('div.swfupload-info', jQuery('#' + FileUploadHelper.properties.id)).empty();
				
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
	var value = document.getElementById(id).value;
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
			} else {
				files.push(fileValue);
			}
		}
	}
	
	return files;
}

function addFileInputForUpload(id, message, className, showProgressBar, addjQuery, autoAddFileInput, autoUpload) {
	if (FileUploadHelper.properties.needFlash)
		return;
	
	var styleAttribute = null;
	jQuery('input.' + className, jQuery('#' + id)).each(function() {
		styleAttribute = jQuery(this).attr('style');
		
		var valueProperty = this.value;
		if (valueProperty == null || valueProperty == '')
			return;
	});
	
	showLoadingMessage(message);
	
	FileUploader.getRenderedFileInput(id, showProgressBar, addjQuery, autoAddFileInput, autoUpload, styleAttribute, {
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

function __flash__removeCallback(instance, name) {
}