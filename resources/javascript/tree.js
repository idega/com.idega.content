var ajaxObjects = new Array();

var RELOAD_PAGE = false;
var REFRESH_PAGE_INFO = false;
var SHOW_EDIT_PAGES = true;
var SHOW_SOURCE_PAGES = false;

if(changePageName == null) var changePageName = false;

function saveMyTree(newParentNodeId, sourceNodeId, numberInLevel, nodesToIncrease, nodesToDecrease) {
	showLoadingMessage(getMovingText());
	setPageID(sourceNodeId);
	REFRESH_PAGE_INFO = !isSiteMap();
	ThemesEngine.movePage(newParentNodeId, sourceNodeId, numberInLevel, nodesToIncrease, nodesToDecrease, empty);
}

function getNewId(id){
	return id;
}

function deletePage(pageId, followingNodes){
	var wantToDelete = confirm(getAreYouSureText());
	if (wantToDelete) {
		showLoadingMessage(getDeletingText());
		setPageID(null);
		RELOAD_PAGE = true;
		
		if (followingNodes) {
			ThemesEngine.deletePageAndDecrease(pageId, true, followingNodes, empty);
		}
		else {	
			ThemesEngine.deletePageAndDecrease(pageId, true, null, empty);
		}
	}
	else {
		if (treeObj) {
			treeObj.restoreTreeStructure();
		}
	}
}

function empty(param) {
	closeLoadingMessage();
	if (RELOAD_PAGE) {
		RELOAD_PAGE = false;
		getGlobalPageId();
	}
	if (REFRESH_PAGE_INFO) {
		REFRESH_PAGE_INFO = false;
		getPageInfoValues();
	}
	var trashCan = document.getElementById('trash');
	JSTreeObj.deleteNodes = false;
	trashCan.style.opacity = 0.5;		
}

function setFrameUrl(uri) {
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
	
	if (SHOW_SOURCE_PAGES) {
		uri = '/servlet/ObjectInstanciator?idegaweb_instance_class=' + IB_SOURCE_VIEW_CLASS;
	}
	if (SHOW_EDIT_PAGES) {
		if (uri.charAt(uri.length-1) != '/') {
			uri += '/';
		}
		uri += '?view=builder';
	}
	
	frame.src = '';
	frame.src = uri;
	chagePageName = false;
}
						
function getPrewUrl(nodeID) {
	ThemesEngine.getPageUri(nodeID, setFrameUrl);
}
						
function getId(){
	return this.parentNode.id;
}

function changeName() {
	document.getElementById('page_tree_div').id = url;
}