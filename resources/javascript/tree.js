var ajaxObjects = new Array();
	
function saveMyTree(newParentNodeId, sourceNodeId) {
	saveString = treeObj.getNodeOrders();
	console.log("newParentNodeId: " + newParentNodeId + ", sourceNodeId: " + sourceNodeId);
	//BuilderService.movePage(newParentNodeId, sourceNodeId, empty);
	ThemesEngine.movePage(newParentNodeId, sourceNodeId, empty);
}


function getNewId(id){
	return id;
}

function deletePage(pageId){
	showLoadingMessage("Deleting...");
	ThemesEngine.deletePage(pageId, true, empty);
}

function empty(param) {
	closeLoadingMessage();
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