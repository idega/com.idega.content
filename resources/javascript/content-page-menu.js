if (ContentPageMenuHelper == null) var ContentPageMenuHelper = {};

jQuery.noConflict();

jQuery(window).load(function() {
	jQuery('.contentPageMenu a.edit, .contentPageMenu a.add').each(function() {
		var link = jQuery(this);
		link.fancybox({
			maxWidth	: 1080,
			fitToView	: false,
			height		: '100%',
			autoSize	: false,
			closeClick	: false,
			openEffect	: 'none',
			closeEffect	: 'none',
			afterShow: function() {
				tinymce.init({
	                // Location of TinyMCE script
	                script_url : '/idegaweb/bundles/com.idega.block.web2.0.bundle/resources/javascript/tinymce/4.1.7/tinymce.min.js',
	                
	                selector: '#contentPageEditFormText',
	
	                plugins: [
				        "advlist autolink lists link image charmap print preview anchor",
				        "searchreplace visualblocks code fullscreen",
				        "insertdatetime media table contextmenu paste"
				    ],
				    file_browser_callback: function(field_name, url, type, win) {
				        ContentPageMenuHelper.doHandleFileUpload(field_name, url, type, win);
				    },
				    toolbar: "insertfile undo redo | styleselect | bold italic | alignleft aligncenter alignright alignjustify | bullist numlist outdent indent | link image"
				});
				
				return true;
			}
		});
	});
	
	jQuery('.contentPageMenu a.delete').click(function() {
		var link = jQuery(this);
		var alert = link.attr('rel');
		
		return confirm(alert);
	});
	
	jQuery('#contentPageEditForm a.close').live('click', function(event) {
		event.preventDefault();
		jQuery.fancybox.close();
	});
	
	jQuery('#contentPageEditForm a.store').live('click', function(event) {
		event.preventDefault();
		var form = jQuery('form#contentPageEditForm');
		form.submit();
		jQuery.fancybox.close();
	});
	
	if (IE) {
		LazyLoader.load('/idegaweb/bundles/com.idega.block.web2.0.bundle/resources/javascript/jquery-plugins/jquery.form.js', function() {
			jQuery('#contentPageImagesForm').ajaxForm(function() {});
		});
	}
});

ContentPageMenuHelper.doFinishUpload = function(uploadedFile) {
	closeAllLoadingMessages();
	jQuery('.mce-btn.mce-open').parent().find('.mce-textbox').val(uploadedFile).closest('.mce-window').find('.mce-primary').click();
}

ContentPageMenuHelper.doStartFileUpload = function(input, message) {
	showLoadingMessage(message);
	if (IE) {
		jQuery('#contentPageImagesForm').ajaxSubmit({ success: function(d){eval(d);} });this.value='';
	} else {
		jQuery('#contentPageImagesForm').submit();this.value='';
	}
}

ContentPageMenuHelper.doHandleFileUpload = function(field_name, url, type, win) {
	jQuery('input', jQuery('#contentPageImagesForm')).click();
	return true;
}