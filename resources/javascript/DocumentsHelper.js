if (DocumentsHelper == null) var DocumentsHelper = {};

DocumentsHelper.doDeleteResource = function(loadingMessage, pathInRepository) {
	showLoadingMessage(loadingMessage);
	DocumentsService.doDelete(pathInRepository, {
		callback: function(result) {
			if (result == null || pathInRepository == null) {
				jQuery('#uploadForm').submit();
				return false;
			}
			
			humanMsg.displayMsg(result.value, {
				timeout: 1500,
				callback: function() {
					var currentPath = pathInRepository.substring(0, pathInRepository.lastIndexOf('/') + 1);
					return oamSubmitForm('uploadForm', 'uploadForm:gt_list_l:0:wb_list', null, [['wdurl',currentPath], ['isf','true']]);
				}
			});
		},
		errorHandler: function(msg, ex) {
			alert('Failed to delete ' + pathInRepository + '.\nError message from server: ' + msg + '.\nException: ' + ex);
			reloadPage();
		}
	});
}