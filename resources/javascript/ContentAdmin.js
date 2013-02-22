var ContentAdmin = {};

ContentAdmin.uuid = null;

jQuery(window).load(function() {
	jQuery('div.content_item_toolbar').hover(
		function() {
			jQuery(this).parent('.blog-entry, .article_item').addClass('hovered');
		},
		function() {
			jQuery(this).parent('.blog-entry, .article_item').removeClass('hovered');
		}
	);
	
	jQuery('a.streamerLink').each(function() {
		var link = jQuery(this);
		link.attr('data-fancybox-type', 'ajax');
		link.fancybox({
			autoScale: false,
			autoDimensions: false,
			width: 450,
			height: 175,
			afterClose: function() {
				RepositoryItemStreamer.cleanStreamHistory(ContentAdmin.uuid);
				ContentAdmin.uuid = null;
			}
		});
	});
});

ContentAdmin.streamToRemoteServer = function(serverId, url, directoryId, reCreateId, uuid, containerId) {
	ContentAdmin.uuid = uuid;
	var reCreateStructure = true;
	jQuery('input[type=\'radio\']', 'div.repositoryItemStreamerReCreateStructure').each(function() {
		var checked = jQuery(this).attr('checked') == 'checked';
		if (checked) {
			reCreateStructure = 'true' == jQuery(this).val();
		}
	});
	
	jQuery('#' + containerId).empty();
	showLoadingMessage('');
	
	RepositoryItemStreamer.streamToServer(jQuery('#' + serverId).val(), url, jQuery('#' + directoryId).val(), reCreateStructure, uuid, {
		callback: function(result) {
			RepositoryItemStreamer.getStreamResults(uuid, {
				callback: function(streamResults) {
					IWCORE.insertRenderedComponent(streamResults, {
						callback: function() {
							RepositoryItemStreamer.cleanStreamHistory(uuid);
							closeAllLoadingMessages();
							alert(result.value);
						},
						container: containerId,
						rewrite: true
					});
				}
			});
		},
		errorHandler: function(ob1, ob2) {
			closeAllLoadingMessages();
		}
	});
}