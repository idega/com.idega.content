jQuery.noConflict();

jQuery(window).load(function() {
	jQuery('.contentPageMenu a.edit, .contentPageMenu a.add').each(function() {
		var link = jQuery(this);
		link.fancybox({
			maxWidth	: 675,
			fitToView	: false,
			height		: '100%',
			autoSize	: false,
			closeClick	: false,
			openEffect	: 'none',
			closeEffect	: 'none',
			afterShow: function() {
				jQuery('textarea.tinymce').tinymce({
	                // Location of TinyMCE script
	                script_url : '/idegaweb/bundles/com.idega.block.web2.0.bundle/resources/javascript/tinymce/3.5.8/tiny_mce.js',
	
	                // General options
	                theme : "advanced",
	                plugins : "pagebreak,style,layer,table,save,advhr,advimage,advlink,emotions,iespell,inlinepopups,insertdatetime,preview,media,searchreplace,print,contextmenu,paste,directionality,fullscreen,noneditable,visualchars,nonbreaking,xhtmlxtras,template",
	
	                // Theme options
	                theme_advanced_buttons1 : "bold,italic,underline,strikethrough,|,justifyleft,justifycenter,justifyright,justifyfull,styleselect,formatselect,fontselect,fontsizeselect",
	                theme_advanced_buttons2 : "cut,copy,paste,pastetext,pasteword,|,search,|,bullist,numlist,|,outdent,indent,blockquote,|,undo,redo,|,link,unlink,image,cleanup,help,code,|,preview,|,forecolor,backcolor",
	                theme_advanced_buttons3 : "tablecontrols,|,hr,removeformat,visualaid,|,sub,sup,|,charmap,emotions,media,advhr,|,fullscreen",
	                theme_advanced_toolbar_location : "top",
	                theme_advanced_toolbar_align : "left",
	                theme_advanced_statusbar_location : "bottom",
	                theme_advanced_resizing : true,
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
});