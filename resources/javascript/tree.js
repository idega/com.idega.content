var ajaxObjects = new Array();

var RELOAD_PAGE = false;
	
function saveMyTree(newParentNodeId, sourceNodeId) {
	saveString = treeObj.getNodeOrders();
	//console.log("newParentNodeId: " + newParentNodeId + ", sourceNodeId: " + sourceNodeId);
	showLoadingMessage("Moving...");
	setPageID(sourceNodeId);
	ThemesEngine.movePage(newParentNodeId, sourceNodeId, empty);
}


function getNewId(id){
	return id;
}

function deletePage(pageId){
	showLoadingMessage("Deleting...");
	setPageID(null);
	RELOAD_PAGE = true;
	ThemesEngine.deletePage(pageId, true, empty);
}

function empty(param) {
	closeLoadingMessage();
	if (RELOAD_PAGE) {
		RELOAD_PAGE = false;
		getGlobalPageId();
	}
}

function setFrameUrl(url) {
	var frame = document.getElementById('treePages');
	if (frame != null) {
		frame.src=url;
	}
}
						
function getPrewUrl(nodeID){
	PagePreview.getPreviewUrl(nodeID, setFrameUrl);
}
						
function getId(){
	return this.parentNode.id;
}

function changeName() {
//	document.getElementById('treePages').id=url;
	document.getElementById('page_tree_div').id=url;
}