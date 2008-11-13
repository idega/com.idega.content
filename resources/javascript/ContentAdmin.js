jQuery(document).ready(function() {
	jQuery('div.content_item_toolbar').hover(
		function() {
			jQuery(this).parent('.blog-entry, .article_item').addClass('hovered');
		},
		function() {
			jQuery(this).parent('.blog-entry, .article_item').removeClass('hovered');
		}
	);
});