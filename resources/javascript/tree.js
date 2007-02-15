var ajaxObjects = new Array();

var RELOAD_PAGE = false;
var REFRESH_PAGE_INFO = false;
	
function saveMyTree(newParentNodeId, sourceNodeId, numberInLevel, nodesToIncrease, nodesToDecrease) {
	
//console.log('saveMyTree');
//console.log(numberInLevel);
//console.log(nodesToIncrease);
//console.log('nodesToIncrease');
/*
if(nodesToIncrease){
	for(var i = 0; i < nodesToIncrease.length; i++)
		console.log(nodesToIncrease[i]);
}
*/
//console.log('nodesToDecrease');
//console.log(nodesToDecrease);
/*
if(nodesToDecrease){
	for(var i = 0; i < nodesToDecrease.length; i++)
		console.log(nodesToDecrease[i]);
}
*/
	showLoadingMessage("Moving...");
	setPageID(sourceNodeId);
	REFRESH_PAGE_INFO = !isSiteMap();
//alert(numberInLevel);	
	ThemesEngine.movePage(newParentNodeId, sourceNodeId, numberInLevel, nodesToIncrease, nodesToDecrease, empty);
}

function getNewId(id){
	return id;
}

function deletePage(pageId, followingNodes){
	var wantToDelete = confirm("Are you sure?");
	if (wantToDelete){
		showLoadingMessage("Deleting...");
		setPageID(null);
		RELOAD_PAGE = true;
		
		if(followingNodes){
//console.log('followingNodes1');
			ThemesEngine.deletePageAndDecrease(pageId, true, followingNodes, empty);
		}
		else{
//console.log('not followingNodes');			
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