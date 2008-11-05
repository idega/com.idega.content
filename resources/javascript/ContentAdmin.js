jQuery.noConflict();

jQuery(document).ready(function() {
	jQuery('div.content_item_toolbar').hover(
		function() {
			jQuery(this).parent('.blog-entry').addClass('hovered');
		},
		function() {
			jQuery(this).parent('.blog-entry').removeClass('hovered');
		}
	);
});