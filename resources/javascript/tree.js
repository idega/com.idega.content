var ajaxObjects = new Array();
	
function saveMyTree(newParentNodeId, sourceNodeId) {
	saveString = treeObj.getNodeOrders();
	BuilderService.movePage(newParentNodeId, sourceNodeId, empty);
}
function empty(param) {
}

function testingIds(url) {
	document.getElementById('treePages').src=url;
}
						
function getPrewUrl(nodeID){
	var lists = document.getElementsByTagName("LI");
	PagePreview.getPreviewUrl(nodeID, testingIds);
}
						
function getId(){
	return this.parentNode.id;
}

function changeName() {
	document.getElementById('treePages').id=url;
}