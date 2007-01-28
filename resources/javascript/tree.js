var ajaxObjects = new Array();

var RELOAD_PAGE = false;
var REFRESH_PAGE_INFO = false;
	
function saveMyTree(newParentNodeId, sourceNodeId) {
	showLoadingMessage("Moving...");
	setPageID(sourceNodeId);
	REFRESH_PAGE_INFO = !isSiteMap();
	ThemesEngine.movePage(newParentNodeId, sourceNodeId, empty);
}

function getNewId(id){
	return id;
}

function deletePage(pageId){
	var wantToDelete = confirm("Are you sure?");
	if (wantToDelete){
		showLoadingMessage("Deleting...");
		setPageID(null);
		RELOAD_PAGE = true;
		ThemesEngine.deletePage(pageId, true, empty);
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
}

function setFrameUrl(url) {
	var frame = document.getElementById('treePages');
	if (frame != null) {
		frame.src=url;
	}
}
						
function getPrewUrl(nodeID) {
	PagePreview.getPreviewUrl(nodeID, setFrameUrl);
}
						
function getId(){
	return this.parentNode.id;
}

function changeName() {
//	document.getElementById('treePages').id=url;
	document.getElementById('page_tree_div').id=url;
}