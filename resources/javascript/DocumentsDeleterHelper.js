window.onload = function() {
	jQuery('.iw_search_result_link_delete a.linkedWithLinker').click(function() {
		var link = jQuery(this);
		var alert = link.attr('rel');
		
		return confirm(alert);
	});
};