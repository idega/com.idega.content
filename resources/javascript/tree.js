var ajaxObjects = new Array();

var RELOAD_PAGE = false;
var REFRESH_PAGE_INFO = false;

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
}

function setFrameUrl(url) {
	if (url == null) {
		return;
	}
	if (url == "") {
		return;
	}
	if (url.charAt(url.length-1) != "/") {
		url += "/";
	}
	url += "?view=builder";
	var frame = document.getElementById("treePages");
	if (frame != null) {
//		if (!changePageName){ 
			frame.src=url;
//		}
	}
	chagePageName = false;
}
						
function getPrewUrl(nodeID) {
	PagePreview.getPreviewUrl(nodeID, setFrameUrl);
}
						
function getId(){
	return this.parentNode.id;
}

function changeName() {
	document.getElementById('page_tree_div').id = url;
}

function initializeTree() {
	setIsSiteMap(true);
	setNeedRedirect(true);
	setActiveLanguage();
	resizeContainer("site_tree_container", "site_tree_container_site", 335, true);
	resizeContainer("pagesTypesContainer", "pagesTypesContainer", 302, false);
	resizeContainer("siteTemplatesContainer", "siteTemplatesContainer", 302, false);
	resizeContainer("siteTemplatesContainer", "siteTemplatesContainer", 287, true);
	checkIfNotEmptySiteTree("div_id_current_structure_tree");
}