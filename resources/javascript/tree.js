var RELOAD_PAGE = false;
var SHOW_EDIT_PAGES = false;
var SHOW_SOURCE_PAGES = false;

var PAGE_ID_FROM_FRAME = null;

function saveMyTree(newParentNodeId, sourceNodeId, numberInLevel, nodesToIncrease, nodesToDecrease) {
	showLoadingMessage(getMovingText());
	setPageID(sourceNodeId, true);
	REFRESH_PAGE_INFO = !isSiteMap();

	if (numberInLevel == null) {
		numberInLevel = 0;
	}

	LucidEngine.movePage(newParentNodeId, sourceNodeId, numberInLevel, nodesToIncrease, nodesToDecrease, {
		callback: function(result) {
			closeAllLoadingMessages();
			if (result) {
				getPageInfoValues();
				boldCurrentTreeElement();
			}
			else {
				reloadPage();
			}
		}
	});
}

function getNewId(id){
	return id;
}

function deletePage(pageId, followingNodes, nodeBeingDeletedId) {
	var elementToRemove = $(nodeBeingDeletedId);
	if (elementToRemove != null) {
		elementToRemove.injectInside($(JSTreeObj.floatingContainer));
	}
	
	var wantToDelete = confirm(getAreYouSureText());
	if (wantToDelete) {
		showLoadingMessage(getDeletingText());
		setPageID(null, true);
		RELOAD_PAGE = true;
		
		LucidEngine.deletePageAndDecrease(pageId, true, followingNodes, {
			callback: function(result) {
				if (!result) {
					executeActionsAfterSiteTreeInLucidWasChanged(result);
				}
				
				LucidEngine.deleteArticlesFromDeletedPages(pageId);
				executeActionsAfterSiteTreeInLucidWasChanged(result);
			}
		});
	}
	else {
		try {
			treeObj.restoreTreeStructure();
		} catch(e) {
			reloadPage();
		}
	}
}

function executeActionsAfterSiteTreeInLucidWasChanged(param) {
	closeAllLoadingMessages();
	if (RELOAD_PAGE) {
		RELOAD_PAGE = false;
		getGlobalPageId();
	}
	
	var trashCan = document.getElementById('trash');
	JSTreeObj.deleteNodes = false;
	trashCan.style.opacity = 0.5;		
}

function setFrameUrlForLucidApplication(uri, pageId, canActAsBuilderUser) {
	var frame = document.getElementById('treePages');
	if (frame == null) {
		return false;
	}
	
	if (uri == null) {
		uri = '';
	}
	if (uri == '') {
		frame.src = uri;
		return false;
	}
	
	if (SHOW_SOURCE_PAGES && canActAsBuilderUser) {
		uri = '/servlet/ObjectInstanciator?idegaweb_instance_class=' + IB_SOURCE_VIEW_CLASS;
		if (pageId != null) {
			uri += '&pageForSourceId=' + pageId;
		}
	}
	if (SHOW_EDIT_PAGES && canActAsBuilderUser) {
		if (uri.charAt(uri.length-1) != '/') {
			uri += '/';
		}
		uri += '?view=builder';
	}
	
	if (SHOW_EDIT_PAGES) {
		jQuery('#showPageModules').removeClass('disabledButtonInPages');
		jQuery('#showThemesButton').removeClass('disabledButtonInPages');
	}
	else {
		if (!jQuery('#showPageModules').hasClass('disabledButtonInPages')) {
			jQuery('#showPageModules').addClass('disabledButtonInPages');
		}
		if (!SHOW_SOURCE_PAGES) {
			jQuery('#showThemesButton').removeClass('disabledButtonInPages');
		}
		else {
			if (!jQuery('#showThemesButton').hasClass('disabledButtonInPages')) {
				jQuery('#showThemesButton').addClass('disabledButtonInPages');
			}
		}
	}
	
	frame.src = '';
	frame.src = uri;
	chagePageName = false;
}

function getPageUriByCheckedId() {
	var currentId = getPageID();
	var uri = getPagePreviewInFrameUri();
	
	if (uri == null) {
		getPrewUrl(currentId);
	}
	else {
		LucidEngine.getPageIdByUri(uri, {
			callback: function(id) {
				if (id != null) {
					PAGE_ID_FROM_FRAME = id;
				}
				
				if (PAGE_ID_FROM_FRAME != null && PAGE_ID_FROM_FRAME != currentId) {
					if (!WORKING_WITH_TEMPLATE && currentId != null) {
						PAGE_ID_FROM_FRAME = currentId;
					}
					getPrewUrl(PAGE_ID_FROM_FRAME);
				}
				else {
					PAGE_ID_FROM_FRAME = null;
					getPrewUrl(currentId);
				}
			}
		});
	}
}
						
function getPrewUrl(nodeID, uri) {
	if (WORKING_WITH_TEMPLATE) {
		if (nodeID != TEMPLATE_ID) {
			nodeID = TEMPLATE_ID;
			PAGE_ID_FROM_FRAME = null;
		}		
	}
	
	if (uri) {
		setFrameUrlForLucidApplication(uri, nodeID, LucidHelper.applicationInfo.canActAsBuilderUser);
	}
	else {
		LucidEngine.getPageUri(nodeID, {
			callback: function(uri) {
				setFrameUrlForLucidApplication(uri, nodeID, LucidHelper.applicationInfo.canActAsBuilderUser);
			}
		});
	}
}