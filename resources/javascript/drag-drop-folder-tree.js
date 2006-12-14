	/************************************************************************************************************
	(C) www.dhtmlgoodies.com, July 2006
	
	Update log:
		August, 8th, 2006: Replaced getLeftPos and getTopPos methods. 
	
	This is a script from www.dhtmlgoodies.com. You will find this and a lot of other scripts at our website.	
	
	Terms of use:
	You are free to use this script as long as the copyright message is kept intact. However, you may not
	redistribute, sell or repost it without our permission.
	
	Thank you!
	
	www.dhtmlgoodies.com
	Alf Magne Kalleland
	
	************************************************************************************************************/
		
	var JSTreeObj;
	var treeUlCounter = 0;
	var nodeId = 1;
	var thisTree;
	var divClass = '';
	var globalDivId = null;
		
	/* Constructor */
	function JSDragDropTree()
	{
		var thisTree = false;
		var idOfTree;
		
		var sourceTree;
		var actionOnMouseUp;
		
		var imageFolder;
		var folderImage;
		var plusImage;
		var minusImage;
		var maximumDepth;
		var dragNode_source;
		var dragNode_parent;
		var dragNode_sourceNextSib;
		var dragNode_noSiblings; 
		var deleteNodes;
		
		var dragNode_destination;
		var floatingContainer;
		var dragDropTimer;
		var dropTargetIndicator;
		var insertAsSub;
		var indicator_offsetX;
		var indicator_offsetX_sub;
		var indicator_offsetY;
		var newPageId;
		var treeStructure;
		var parentId;

		this.parentid = -1;
		this.newPageId = -1;
		this.deleteNodes = false;
		this.sourceTreee = true;
		this.actionOnMouseUp = 'empty';
		
		this.imageFolder = '/idegaweb/bundles/com.idega.content.bundle/resources/images/';
		this.iconFolder = '/idegaweb/bundles/com.idega.content.bundle/resources/images/pageIcons/';
//		this.folderImage = 'dhtmlgoodies_folder.gif';
//		this.folderImage = 'treeviewer_node_leaf.gif';
		this.folderImage = 'text.png';
//		this.plusImage = 'dhtmlgoodies_plus.gif';
//		this.minusImage = 'dhtmlgoodies_minus.gif';
		this.plusImage = 'nav-plus.gif';
		this.minusImage = 'nav-minus.gif';
		this.maximumDepth = 6;
		var messageMaximumDepthReached;
				
		this.floatingContainer = document.createElement('UL');
		this.floatingContainer.style.position = 'absolute';
		this.floatingContainer.style.display='none';
		this.floatingContainer.id = 'floatingContainer';
		this.insertAsSub = false;
		document.body.appendChild(this.floatingContainer);
		this.dragDropTimer = -1;
		this.dragNode_noSiblings = false;
		
		if(document.all){
			this.indicator_offsetX = 2;	// Offset position of small black lines indicating where nodes would be dropped.
			this.indicator_offsetX_sub = 4;
			this.indicator_offsetY = 2;
		}else{
			this.indicator_offsetX = 1;	// Offset position of small black lines indicating where nodes would be dropped.
			this.indicator_offsetX_sub = 3;
			this.indicator_offsetY = 2;			
		}
		if(navigator.userAgent.indexOf('Opera')>=0){
			this.indicator_offsetX = 2;	// Offset position of small black lines indicating where nodes would be dropped.
			this.indicator_offsetX_sub = 3;
			this.indicator_offsetY = -7;				
		}

		this.messageMaximumDepthReached = ''; // Use '' if you don't want to display a message 
	}
	
	
	/* JSDragDropTree class */
	JSDragDropTree.prototype = {
		
		Get_Cookie : function(name) { 
		   var start = document.cookie.indexOf(name+"="); 
		   var len = start+name.length+1; 
		   if ((!start) && (name != document.cookie.substring(0,name.length))) return null; 
		   if (start == -1) return null; 
		   var end = document.cookie.indexOf(";",len); 
		   if (end == -1) end = document.cookie.length; 
		   return unescape(document.cookie.substring(len,end)); 
		} 
		,
		// This function has been slightly modified
		Set_Cookie : function(name,value,expires,path,domain,secure) { 			
			expires = expires * 60*60*24*1000;
			var today = new Date();
			var expires_date = new Date( today.getTime() + (expires) );
		    var cookieString = name + "=" +escape(value) + 
		       ( (expires) ? ";expires=" + expires_date.toGMTString() : "") + 
		       ( (path) ? ";path=" + path : "") + 
		       ( (domain) ? ";domain=" + domain : "") + 
		       ( (secure) ? ";secure" : ""); 
		    document.cookie = cookieString; 
		} 
		,setMaximumDepth : function(maxDepth)
		{
			this.maximumDepth = maxDepth;	
		}
		,setMessageMaximumDepthReached : function(newMessage)
		{
			this.messageMaximumDepthReached = newMessage;
		}
		,	
		setImageFolder : function(path)
		{
			this.imageFolder = path;	
		}
		,
		setFolderImage : function(imagePath)
		{
			this.folderImage = imagePath;			
		}
		,
		setPlusImage : function(imagePath)
		{
			this.plusImage = imagePath;				
		}
		,
		setMinusImage : function(imagePath)
		{
			this.minusImage = imagePath;			
		}
		,		
		setTreeId : function(idOfTree)
		{
			this.idOfTree = idOfTree;		
		}	
		,
		expandAll : function()
		{
			var menuItems = document.getElementById(this.idOfTree).getElementsByTagName('LI');		
			for(var no=0;no<menuItems.length;no++){
				var subItems = menuItems[no].getElementsByTagName('UL');
				if(subItems.length>0 && subItems[0].style.display!='block'){				
					JSTreeObj.showHideNode(false,menuItems[no].id);

				}			
			}
		}	
		,
		collapseAll : function()
		{
			var menuItems = document.getElementById(this.idOfTree).getElementsByTagName('LI');
			for(var no=0;no<menuItems.length;no++){
				var subItems = menuItems[no].getElementsByTagName('UL');
				if(subItems.length>0 && subItems[0].style.display=='block'){
					JSTreeObj.showHideNode(false,menuItems[no].id);
				}			
			}		
		}	
		,
		/*
		Find top pos of a tree node
		*/
		getTopPos : function(obj){
			var top = obj.offsetTop/1;
			while((obj = obj.offsetParent) != null){
				if(obj.tagName!='HTML')top += obj.offsetTop;
			}			
			if(document.all)
				top = top/1 + 13;
			else top = top/1 + 4;		
			return top;
		}
		,	
		/*
		Find left pos of a tree node
		*/
		getLeftPos : function(obj){
			var left = obj.offsetLeft/1 + 1;
			while((obj = obj.offsetParent) != null){
				if(obj.tagName!='HTML')left += obj.offsetLeft;
			}
	  			
			if(document.all)
				left = left/1 - 2;
			return left;
		}	
		,
		showHideNode : function(e,inputId) {			
			if(inputId){
				if(!document.getElementById(inputId))
					return;
				thisNode = document.getElementById(inputId).getElementsByTagName('IMG')[0]; 
			}
			else {
				thisNode = this;
				if(this.tagName=='A')
					thisNode = this.parentNode.getElementsByTagName('IMG')[0];											
			}					
			if(thisNode.style.visibility=='hidden')
				return;		
			var parentNode = thisNode.parentNode;
			inputId = parentNode.id.replace(/[^0-9]/g,'');
			if(thisNode.src.indexOf(JSTreeObj.plusImage)>=0){
				thisNode.src = thisNode.src.replace(JSTreeObj.plusImage,JSTreeObj.minusImage);
				var ul = parentNode.getElementsByTagName('UL')[0];
				ul.style.display='block';
				if(!initExpandedNodes)initExpandedNodes = ',';
				if(initExpandedNodes.indexOf(',' + inputId + ',')<0) initExpandedNodes = initExpandedNodes + inputId + ',';
			}else{					
				thisNode.src = thisNode.src.replace(JSTreeObj.minusImage,JSTreeObj.plusImage);
				parentNode.getElementsByTagName('UL')[0].style.display='none';
				initExpandedNodes = initExpandedNodes.replace(',' + inputId,'');
			}	

			JSTreeObj.Set_Cookie('dhtmlgoodies_expandedNodes',initExpandedNodes,500);			
			return false;						
		}
		,
		copyDragableNode : function(e)
		{			
			var sourceTree = false;			
			var liTag = document.getElementsByTagName('LI')[0];
			
			var subs = JSTreeObj.floatingContainer.getElementsByTagName('LI');

			if(subs.length>0){
				if(JSTreeObj.dragNode_sourceNextSib){
					JSTreeObj.dragNode_parent.insertBefore(JSTreeObj.dragNode_source,JSTreeObj.dragNode_sourceNextSib);
				}
				else{
					JSTreeObj.dragNode_parent.appendChild(JSTreeObj.dragNode_source);
				}
			}
			
			JSTreeObj.dragNode_source = this.parentNode;
			JSTreeObj.dragNode_parent = this.parentNode.parentNode;

			var parentDiv = JSTreeObj.dragNode_parent;
			while(true){
				if (parentDiv.getElementsByTagName('DIV')[0]){
					globalDivId = parentDiv.getElementsByTagName('DIV')[0].id;
					break;					
				}
				parentDiv = parentDiv.parentNode;
			}
			JSTreeObj.dragNode_sourceNextSib = false;
			
			if(JSTreeObj.dragNode_source.nextSibling)JSTreeObj.dragNode_sourceNextSib = JSTreeObj.dragNode_source.nextSibling;
			JSTreeObj.dragNode_destination = false;
			JSTreeObj.dragDropTimer = 0;
			JSTreeObj.timerDragCopy();
			
			return false;
		}
		,
		timerDragCopy : function()
		{	
			if(this.dragDropTimer>=0 && this.dragDropTimer<10) {
				this.dragDropTimer = this.dragDropTimer + 1;
				setTimeout('JSTreeObj.timerDragCopy()',20);
				return;
			}
			if(this.dragDropTimer==10)
			{
				JSTreeObj.floatingContainer.style.display='block';
				var tempNode = JSTreeObj.dragNode_source.cloneNode(true);
				tempNode.id = 'floatingContainer'+ tempNode.id;
				JSTreeObj.floatingContainer.appendChild(tempNode);									
			}
		}
		,		
		
		/* Initialize drag */ 
		initDrag : function(e)
		{
//			document.documentElement.onmouseup = JSTreeObj.actionOnMouseUp;
			
//			var sourceTree = false;			
			var liTag = document.getElementsByTagName('LI')[0];
			
			var subs = JSTreeObj.floatingContainer.getElementsByTagName('LI');

			if(subs.length>0){
				if(JSTreeObj.dragNode_sourceNextSib){
					JSTreeObj.dragNode_parent.insertBefore(JSTreeObj.dragNode_source,JSTreeObj.dragNode_sourceNextSib);
				}
				else{
					JSTreeObj.dragNode_parent.appendChild(JSTreeObj.dragNode_source);
				}					
			}
			
			JSTreeObj.dragNode_source = this.parentNode;
			JSTreeObj.dragNode_parent = this.parentNode.parentNode;

			var parentDiv = JSTreeObj.dragNode_parent;
			while(true){
				if (parentDiv.getElementsByTagName('DIV')[0]){
					globalDivId = parentDiv.getElementsByTagName('DIV')[0].id;
					break;					
				}
				parentDiv = parentDiv.parentNode;
			}
			JSTreeObj.dragNode_sourceNextSib = false;
			
			if(JSTreeObj.dragNode_source.nextSibling)JSTreeObj.dragNode_sourceNextSib = JSTreeObj.dragNode_source.nextSibling;
			JSTreeObj.dragNode_destination = false;
			JSTreeObj.dragDropTimer = 0;
			JSTreeObj.timerDrag();
			
			return false;
		}
		,
		timerDrag : function()
		{	
			if(this.dragDropTimer>=0 && this.dragDropTimer<10){
				this.dragDropTimer = this.dragDropTimer + 1;
				setTimeout('JSTreeObj.timerDrag()',20);
				return;
			}
			if(this.dragDropTimer==10)
			{
				JSTreeObj.floatingContainer.style.display='block';
				JSTreeObj.floatingContainer.appendChild(JSTreeObj.dragNode_source);	
			}
		}
		,
		moveDragableNodes : function(e){			
			
			var divTags = document.getElementsByTagName('div');
			var divTag = null;
			for(var i = 0; i < divTags.length; i++){
				if(divTags[i].getAttribute('class') == 'current_structure')
					divTag = divTags[i];
			}
			
			if(JSTreeObj.dragDropTimer<10)return;
			if(document.all)e = event;
			dragDrop_x = e.clientX/1 + 5 + document.body.scrollLeft;
			dragDrop_y = e.clientY/1 + 5 + document.documentElement.scrollTop;	

//			dragDrop_x = e.clientX/1 -60+ document.body.scrollLeft;
//			dragDrop_y = e.clientY/1 -20+ document.documentElement.scrollTop;	

			JSTreeObj.floatingContainer.style.left = dragDrop_x + 'px';
			JSTreeObj.floatingContainer.style.top = dragDrop_y + 'px';
			JSTreeObj.floatingContainer.style.opacity = 0.5;

			var thisObj = this;
			if(thisObj.tagName=='A' || thisObj.tagName=='IMG')
				thisObj = thisObj.parentNode;
			JSTreeObj.dragNode_noSiblings = false;
			var tmpVar = thisObj.getAttribute('noSiblings');
			if(!tmpVar)tmpVar = thisObj.noSiblings;
			if(tmpVar=='true')JSTreeObj.dragNode_noSiblings=true;

			var tmpVar = thisObj.getAttribute('sourceTree');
			
			if(!tmpVar)
				tmpVar = thisObj.sourceTree;
			if(tmpVar=='true')
				JSTreeObj.dragNode_sourceTree=true;
				
			if(thisObj && thisObj.id) {
				JSTreeObj.dragNode_destination = thisObj;
				var img = thisObj.getElementsByTagName('IMG')[1];
				var tmpObj= JSTreeObj.dropTargetIndicator;
				tmpObj.style.display='block';
				
				var eventSourceObj = this;
				if(JSTreeObj.dragNode_noSiblings && eventSourceObj.tagName=='IMG')
					eventSourceObj = eventSourceObj.nextSibling;
				
				var tmpImg = tmpObj.getElementsByTagName('IMG')[0];
				if(this.tagName=='A' || JSTreeObj.dragNode_noSiblings) {
					tmpImg.src = tmpImg.src.replace('ind1','ind2');	
					JSTreeObj.insertAsSub = true;
					tmpObj.style.left = (JSTreeObj.getLeftPos(eventSourceObj) + JSTreeObj.indicator_offsetX_sub) + 'px';
				}else{
					tmpImg.src = tmpImg.src.replace('ind2','ind1');
					JSTreeObj.insertAsSub = false;
					tmpObj.style.left = (JSTreeObj.getLeftPos(eventSourceObj) + JSTreeObj.indicator_offsetX) + 'px';
				}
				if(!thisObj || !thisObj.id){
					tmpImg.style.visibility = 'hidden';
				}
				tmpObj.style.top = (JSTreeObj.getTopPos(thisObj) + JSTreeObj.indicator_offsetY + 9) + 'px';
			}			
			else{
//				console.log('JSTreeObj.dragNode_destination = '+JSTreeObj.dragNode_destination);											
/*
//b
			var parentDiv = thisObj;
console.log('parentDiv = '+parentDiv);
			if(globalDivId == parentDiv.getElementsByTagName('DIV')[0].id){
				console.log('globalDivId = '+ globalDivId + ' parentDiv.getElementsByTagName(DIV)[0].id = '+ parentDiv.getElementsByTagName('DIV')[0].id);
			}
			else 
				console.log('globalDivId = '+ globalDivId + ' parentDiv.getElementsByTagName(DIV)[0].id = '+ parentDiv.getElementsByTagName('DIV')[0].id);
//e
 * 
 */
			}
			return false;
			
		}
		,
		dropDragableNodes:function()
		{	
			var parent;
			if(JSTreeObj.dragDropTimer<10){				
				JSTreeObj.dragDropTimer = -1;
				return;
			}
				
			var showMessage = false;
			if(JSTreeObj.dragNode_destination){	// Check depth
				var countUp = JSTreeObj.dragDropCountLevels(JSTreeObj.dragNode_destination,'up');
				var countDown = JSTreeObj.dragDropCountLevels(JSTreeObj.dragNode_source,'down');
				var countLevels = countUp/1 + countDown/1 + (JSTreeObj.insertAsSub?1:0);		
				
				if(countLevels>JSTreeObj.maximumDepth){
					JSTreeObj.dragNode_destination = false;
					showMessage = true; 	// Used later down in this function
				}
			}
						
			var sourceTree = document.getElementById(JSTreeObj.dragNode_source.id).getAttribute("sourceTree");
						
			if(JSTreeObj.dragNode_destination){
				if(JSTreeObj.insertAsSub){
					var uls = JSTreeObj.dragNode_destination.getElementsByTagName('UL');
					if(uls.length>0){
						ul = uls[0];
						ul.style.display='block';
						
						var lis = ul.getElementsByTagName('LI');						
						
						var li = JSTreeObj.dragNode_source.getElementsByTagName('LI')[0];
						
						if(lis.length>0){	// Sub elements exists - drop dragable node before the first one
							ul.insertBefore(JSTreeObj.dragNode_source,lis[0]);	
						}else {	// No sub exists - use the appendChild method - This line should not be executed unless there's something wrong in the HTML, i.e empty <ul>
							ul.appendChild(JSTreeObj.dragNode_source);	
						}
					}else{
						var ul = document.createElement('UL');
						ul.style.display='block';
						JSTreeObj.dragNode_destination.appendChild(ul);
						ul.appendChild(JSTreeObj.dragNode_source);
					}
					var img = JSTreeObj.dragNode_destination.getElementsByTagName('IMG')[0];	

					img.style.visibility='visible';
					img.src = img.src.replace(JSTreeObj.plusImage,JSTreeObj.minusImage);					
					
				}else{
					if(JSTreeObj.dragNode_destination.nextSibling){
						var nextSib = JSTreeObj.dragNode_destination.nextSibling;
						nextSib.parentNode.insertBefore(JSTreeObj.dragNode_source,nextSib);						
					}else{
						JSTreeObj.dragNode_destination.parentNode.appendChild(JSTreeObj.dragNode_source);
					}

				}	
				/* Clear parent object */
				var tmpObj = JSTreeObj.dragNode_parent;
				var lis = tmpObj.getElementsByTagName('LI');

				if(lis.length==0){
					var tmpSpan = tmpObj.parentNode;
					var img = tmpSpan.parentNode.getElementsByTagName('IMG')[0];
					img.style.visibility='hidden';	// Hide [+],[-] icon
				}
				
			}else{
				// Putting the item back to it's original location
				
				
				if(JSTreeObj.dragNode_sourceNextSib){
					JSTreeObj.dragNode_parent.insertBefore(JSTreeObj.dragNode_source,JSTreeObj.dragNode_sourceNextSib);

				}else{
					JSTreeObj.dragNode_parent.appendChild(JSTreeObj.dragNode_source);
				}		
				return;						
			}
			
			JSTreeObj.dropTargetIndicator.style.display='none';		
			JSTreeObj.dragDropTimer = -1;	
			if(showMessage && JSTreeObj.messageMaximumDepthReached)alert(JSTreeObj.messageMaximumDepthReached);
//			JSTreeObj.getNewParent(null,null,JSTreeObj.dragNode_source.id,null);			
			
//			saveMyTree(JSTreeObj.getNewParent(null,null,JSTreeObj.dragNode_source.id), JSTreeObj.dragNode_source.id);		
//			saveMyTree(JSTreeObj.getNewParent(null,null,JSTreeObj.dragNode_source.id, null), JSTreeObj.dragNode_source.id);		

			
//			JSTreeObj.initTree();
			
			var parentDiv = JSTreeObj.dragNode_destination;
//
			while(true){
				if (parentDiv.getElementsByTagName('DIV')){
					if (parentDiv.getElementsByTagName('DIV')[0]){
	
						if(globalDivId == parentDiv.getElementsByTagName('DIV')[0].id){
							console.log('moving node node ID = '+JSTreeObj.dragNode_source.id+" newParent = "+treeObj.getNewParent(null,null,JSTreeObj.dragNode_source.id, null));
							saveMyTree(treeObj.getNewParent(null,null,JSTreeObj.dragNode_source.id, null), JSTreeObj.dragNode_source.id);									
						}
						else
							console.log('creating node newParent = '+treeObj.getNewParent(null,null,JSTreeObj.dragNode_source.id, null));
						break;	
										
					}
					parentDiv = parentDiv.parentNode;
				}
			}			
//
		}
		,
		deleteNodeChilds : function(nodeParent){
			var childs = nodeParent.getElementsByTagName('li');
			var nodeChilds = nodeParent.childNodes;
			for(var i = 0; i < childs.length; i++){
				if (nodeChilds[i].parentNode == nodeParent){
//					console.log('Child '+ childs[i].id);
					JSTreeObj.getNodeChilds(nodeChilds[i]);
					console.log('delete node = '+ childs[i].id);
					deletePage(childs[i].id);
				}
			}
		}		
		,
		dropDragableNodesCopy:function()
		{		
			var parent;
			if(JSTreeObj.dragDropTimer<10){				
				JSTreeObj.dragDropTimer = -1;
				return;
			}

			if(JSTreeObj.deleteNodes == true){				
				deletePage(JSTreeObj.dragNode_source.id);
				if(JSTreeObj.floatingContainer.getElementsByTagName('LI')[0]){
					if (document.getElementById(JSTreeObj.floatingContainer.getElementsByTagName('LI')[0].id)){
						JSTreeObj.floatingContainer.removeChild(document.getElementById(JSTreeObj.floatingContainer.getElementsByTagName('LI')[0].id));
					}
				}
				var tmpObj = JSTreeObj.dragNode_parent;
				var lis = tmpObj.getElementsByTagName('LI');
				if(lis.length==0){
					var tmpSpan = tmpObj.parentNode;
					var img = tmpSpan.parentNode.getElementsByTagName('IMG')[0];
					img.style.visibility='hidden';	// Hide [+],[-] icon
					tmpObj.parentNode.removeChild(tmpObj);											
				}				
								
				JSTreeObj.dropTargetIndicator.style.display='none';				
				JSTreeObj.dragDropTimer = -1;
				return;
			}				
								
			var showMessage = false;
			if(JSTreeObj.dragNode_destination){	// Check depth
				var countUp = JSTreeObj.dragDropCountLevels(JSTreeObj.dragNode_destination,'up');
				var countDown = JSTreeObj.dragDropCountLevels(JSTreeObj.dragNode_source,'down');
				var countLevels = countUp/1 + countDown/1 + (JSTreeObj.insertAsSub?1:0);		
				
				if(countLevels>JSTreeObj.maximumDepth){
					JSTreeObj.dragNode_destination = false;
					showMessage = true; 	// Used later down in this function
				}
			}
			if(JSTreeObj.dragNode_destination){
				if(JSTreeObj.insertAsSub){
					var uls = JSTreeObj.dragNode_destination.getElementsByTagName('UL');
					if(uls.length>0){
						ul = uls[0];
						ul.style.display='block';
						var lis = ul.getElementsByTagName('LI');						
						
						var li = JSTreeObj.dragNode_source.getElementsByTagName('LI')[0];
						
						if(lis.length>0){	// Sub elements exists - drop dragable node before the first one
							ul.insertBefore(document.getElementById(JSTreeObj.floatingContainer.getElementsByTagName('LI')[0].id),lis[0]);	
						}else {	// No sub exists - use the appendChild method - This line should not be executed unless there's something wrong in the HTML, i.e empty <ul>
console.log(ul);						
							ul.appendChild(document.getElementById(JSTreeObj.floatingContainer.getElementsByTagName('LI')[0].id));	
						}								
					}else{
						var ul = document.createElement('UL');
						ul.style.display='block';
						JSTreeObj.dragNode_destination.appendChild(ul);
console.log(ul);			
console.log(ul.parentNode);
var childElement = document.getElementById(JSTreeObj.floatingContainer.getElementsByTagName('LI')[0].id);
console.log(childElement);
						ul.appendChild(childElement);											
//						ul.appendChild(document.getElementById(JSTreeObj.floatingContainer.getElementsByTagName('LI')[0].id));											
					}
					var img = JSTreeObj.dragNode_destination.getElementsByTagName('IMG')[0];	

					img.style.visibility='visible';
					img.src = img.src.replace(JSTreeObj.plusImage,JSTreeObj.minusImage);					
					
				}else{	
					if(JSTreeObj.dragNode_destination.nextSibling){
						var remId;

						if(JSTreeObj.floatingContainer.getElementsByTagName('LI')[0]){
							remId = JSTreeObj.floatingContainer.getElementsByTagName('LI')[0].id;
						}

						if (document.getElementById(remId)){
							var el = document.getElementById(remId);
						}						
						var nextSib = JSTreeObj.dragNode_destination.nextSibling;
						nextSib.parentNode.insertBefore(el,nextSib);						
					}else{
						var remId;
						if(JSTreeObj.floatingContainer.getElementsByTagName('LI')[0]){
							remId = JSTreeObj.floatingContainer.getElementsByTagName('LI')[0].id;
						}
						if (document.getElementById(remId)){
							var el = document.getElementById(remId);
						}
							JSTreeObj.dragNode_destination.parentNode.appendChild(el);
					}
				}	
				/* Clear parent object */
				
				var tmpObj = JSTreeObj.dragNode_parent;
				var lis = tmpObj.getElementsByTagName('LI');
//console.log('JSTreeObj.dragNode_destination = '+ JSTreeObj.dragNode_destination);		
				if(lis.length==0){
					var tmpSpan = tmpObj.parentNode;
					var img = tmpSpan.parentNode.getElementsByTagName('IMG')[0];
					img.style.visibility='hidden';	// Hide [+],[-] icon
					tmpObj.parentNode.removeChild(tmpObj);											
				}				
//				JSTreeObj.getNewNodeId(JSTreeObj.dragNode_source.id);
				
//				JSTreeObj.initNode(getElementsByTagName('li').getElementById()
//				JSTreeObj.dragNode_source.id);
				
			}else{
				// Putting the item back to it's original location

					if(JSTreeObj.floatingContainer.getElementsByTagName('LI')[0]){
						if (document.getElementById(JSTreeObj.floatingContainer.getElementsByTagName('LI')[0].id)){
//							var el = document.getElementById('floatingContainer'+JSTreeObj.floatingContainer.getElementsByTagName('LI')[0].id);
							JSTreeObj.floatingContainer.removeChild(document.getElementById(JSTreeObj.floatingContainer.getElementsByTagName('LI')[0].id));
						}
					}
					return;
/*				
				if(JSTreeObj.dragNode_sourceNextSib){
					JSTreeObj.dragNode_parent.insertBefore(JSTreeObj.dragNode_source,JSTreeObj.dragNode_sourceNextSib);
					var remId;
					if(JSTreeObj.floatingContainer.getElementsByTagName('LI')[0]){
						remId = JSTreeObj.floatingContainer.getElementsByTagName('LI')[0].id;
					}
					if (document.getElementById(remId)){
						var el = document.getElementById(remId);
						JSTreeObj.floatingContainer.removeChild(el);
					}
				}
				return;						
*/ 
			}
			JSTreeObj.dropTargetIndicator.style.display='none';		
			JSTreeObj.dragDropTimer = -1;	
			if(showMessage && JSTreeObj.messageMaximumDepthReached)alert(JSTreeObj.messageMaximumDepthReached);
//			JSTreeObj.getNewParent(null,null,JSTreeObj.dragNode_source.id,null);			
			
//			saveMyTree(JSTreeObj.getNewParent(null,null,JSTreeObj.dragNode_source.id), JSTreeObj.dragNode_source.id);		
//			saveMyTree(JSTreeObj.getNewParent(null,null,JSTreeObj.dragNode_source.id, null), JSTreeObj.dragNode_source.id);		
			
			
//			JSTreeObj.initTree();
			
			var parentDiv = JSTreeObj.dragNode_destination;
//
			while(true){
				if (parentDiv.getElementsByTagName('DIV')){
					if (parentDiv.getElementsByTagName('DIV')[0]){
						if(globalDivId == parentDiv.getElementsByTagName('DIV')[0].id){
							console.log('moving node node ID = '+JSTreeObj.dragNode_source.id+" newParent = "+treeObj.getNewParent(null,null,JSTreeObj.dragNode_source.id, null));
							saveMyTree(treeObj.getNewParent(null,null,JSTreeObj.dragNode_source.id, null), JSTreeObj.dragNode_source.id);		
						}
						else{
							console.log('creating node newParent = '+treeObj.getNewParent(null,null,JSTreeObj.dragNode_source.id, null)+ 
							' pagetype = '+JSTreeObj.dragNode_source.getAttribute('pagetype') + ' templatefile = '+JSTreeObj.dragNode_source.getAttribute('templatefile'));
//							saveNewPage(treeObj.getNewParent(null,null,JSTreeObj.dragNode_source.id, null), JSTreeObj.dragNode_source.getAttribute('pagetype'), JSTreeObj.dragNode_source.getAttribute('templatefile'), 'newPage');
							JSTreeObj.saveNewPage(treeObj.getNewParent(null,null,JSTreeObj.dragNode_source.id, null), JSTreeObj.dragNode_source.getAttribute('pagetype'), JSTreeObj.dragNode_source.getAttribute('templatefile'), 
							(JSTreeObj.dragNode_source.getElementsByTagName('A')[0]).innerHTML);
						}
							//need name
							
						break;	
										
					}
					parentDiv = parentDiv.parentNode;
				}
			}			
//
		}
		,
/*
		getNewNodeId : function(id){
				
		}
		,
*/ 
		createDropIndicator : function()
		{
			this.dropTargetIndicator = document.createElement('DIV');
			this.dropTargetIndicator.style.position = 'absolute';			
			
//			this.dropTargetIndicator.style.left = '20px';
			
			this.dropTargetIndicator.style.display='none';			
			var img = document.createElement('IMG');
			img.src = this.imageFolder + 'dragDrop_ind1.gif';
			img.id = 'dragDropIndicatorImage';
			this.dropTargetIndicator.appendChild(img);
			document.body.appendChild(this.dropTargetIndicator);			
		}
		,
		dragDropCountLevels : function(obj,direction,stopAtObject){
			var countLevels = 0;
			if(direction=='up'){
				while(obj.parentNode && obj.parentNode!=stopAtObject){
					obj = obj.parentNode;
					if(obj.tagName=='UL')countLevels = countLevels/1 +1;
				}		
				return countLevels;
			}	
			
			if(direction=='down'){ 
				var subObjects = obj.getElementsByTagName('LI');
				for(var no=0;no<subObjects.length;no++){
					countLevels = Math.max(countLevels,JSTreeObj.dragDropCountLevels(subObjects[no],"up",obj));
				}
				return countLevels;
			}	
		}		
		,
		cancelEvent : function()
		{
			return false;	
		}
		,
		cancelSelectionEvent : function()
		{		
			if(JSTreeObj.dragDropTimer<10)return true;
			return false;	
		}
		,
//		getNewParent : function(initObj,saveString,child,numericParentID) {	
		getNewParent : function(initObj,saveString,child,newParent) {
			if(!saveString)var saveString = '';
			if(!initObj){
				initObj = document.getElementById(this.idOfTree);
			}
			var lis = initObj.getElementsByTagName('LI');
			if(lis.length>0){
				var li = lis[0];
				while(li){
					if(li.id){
						if(saveString.length>0)saveString = saveString + ',';
						var numericID = li.id.replace(/[^0-9]/gi,'');
						if(numericID.length==0)numericID='A';
	
						if(numericParentID)
							numericParentID = li.parentNode.parentNode.id.replace(/[^0-9]/gi,'');
						else
							var numericParentID = li.parentNode.parentNode.id.replace(/[^0-9]/gi,'');
						if(numericID!='0'){
							saveString = saveString + numericID;
							saveString = saveString + '-';														
							if(li.parentNode.id!=this.idOfTree)
								saveString = saveString + numericParentID;
							else saveString = saveString + '0';
							if (numericID.toString() == child.toString()) {
								newParent = numericParentID;
								return newParent;
							}							
						}
						var ul = li.getElementsByTagName('UL');
						if(ul.length>0){
							newParent = this.getNewParent(ul[0],saveString,child,newParent);	
						}	
					}			
					li = li.nextSibling;
				}
			}

			if(initObj.id == this.idOfTree){
				return newParent;							
			}
			return newParent;		

		}
		,
		getNodeOrders : function(initObj,saveString)
		{
			
			if(!saveString)var saveString = '';
			if(!initObj){
				initObj = document.getElementById(this.idOfTree);
			}
			var lis = initObj.getElementsByTagName('LI');

			if(lis.length>0){
				var li = lis[0];
				while(li){
					if(li.id){
						if(saveString.length>0)saveString = saveString + ',';
						var numericID = li.id.replace(/[^0-9]/gi,'');
						if(numericID.length==0)numericID='A';
						var numericParentID = li.parentNode.parentNode.id.replace(/[^0-9]/gi,'');
						if(numericID!='0'){
							saveString = saveString + numericID;
							saveString = saveString + '-';														
							if(li.parentNode.id!=this.idOfTree)
								saveString = saveString + numericParentID;
							else saveString = saveString + '0';
						}
						var ul = li.getElementsByTagName('UL');
						if(ul.length>0){
							saveString = this.getNodeOrders(ul[0],saveString);	
						}	
					}			
					li = li.nextSibling;
				}
			}

			if(initObj.id == this.idOfTree){
				return saveString;
							
			}
			return saveString;
		}
		,
/*		
		initNode : function(nodeId){
				// No children var set ?

			document.documentElement.onselectstart = JSTreeObj.cancelSelectionEvent;
			document.documentElement.ondragstart = JSTreeObj.cancelEvent;


				var nodeEl = document.getElementById(nodeId);
				
				var noChildren = false;
				var tmpVar = nodeEl.getAttribute('noChildren');
				if(!tmpVar)tmpVar = nodeEl.noChildren;
				if(tmpVar=='true')noChildren=true;

				var sourceTree = false;
				var tmpVar = nodeEl.getAttribute('sourceTree');
				if(!tmpVar)
					tmpVar = nodeEl.sourceTree;
				if(tmpVar=='true')
					sourceTree=true;
				// No drag var set ?
				var noDrag = false;
				var tmpVar = nodeEl.getAttribute('noDrag');
				if(!tmpVar)tmpVar = nodeEl.noDrag;
				if(tmpVar=='true')noDrag=true;
						 
				nodeId++;
				var subItems = nodeEl.getElementsByTagName('UL');
				var img = document.createElement('IMG');
				img.src = this.imageFolder + this.plusImage;
				img.onclick = JSTreeObj.showHideNode;
				
				if(subItems.length==0)
					img.style.visibility='hidden';
				else{
					subItems[0].id = 'tree_ul_' + treeUlCounter;
					treeUlCounter++;
				}
				var aTag = nodeEl.getElementsByTagName('A')[0];
				if(aTag.id)
					numericId = aTag.id.replace(/[^0-9]/g,'');
				else
					numericId = (no+1);			
				aTag.id = nodeEl.id + 'a';
	
				var input = document.createElement('INPUT');
				input.style.width = '40%';
				input.style.display='none';
				
				nodeEl.insertBefore(input,aTag);
				input.id = nodeEl.id + 'input';
	
				input.onblur = hideEdit;
				
				input.onkeypress = withEnter;
				aTag.ondblclick = initEditLabel;	
					if(!noDrag)aTag.onmousedown = J§.initDrag;
					if(!noChildren)aTag.onmousemove = JSTreeObj.moveDragableNodes;
					if(sourceTree)aTag.onmousedown = JSTreeObj.copyDragableNode;
					nodeEl.insertBefore(img,input);
	
					var folderImg = document.createElement('IMG');
					if(!noDrag)folderImg.onmousedown = JSTreeObj.initDrag;
					if(!noChildren)folderImg.onmousemove = JSTreeObj.moveDragableNodes;
					if(nodeEl.className){
						folderImg.src = this.imageFolder + menuItems[no].className;
					}else{
						folderImg.src = this.imageFolder + this.folderImage;
					}
					nodeEl.insertBefore(folderImg,input);			
		}
		,
*/		
		
		prepareToDelete : function(){
			if(JSTreeObj.deleteNodes == true)
				JSTreeObj.deleteNodes = false;
			else
				JSTreeObj.deleteNodes = true;
		}
		,
		initTree : function()
		{						
			JSTreeObj = this;
			JSTreeObj.createDropIndicator();
			document.documentElement.onselectstart = JSTreeObj.cancelSelectionEvent;
			document.documentElement.ondragstart = JSTreeObj.cancelEvent;
			var nodeId = 0;
			var dhtmlgoodies_tree = document.getElementById(this.idOfTree);
			var menuItems = dhtmlgoodies_tree.getElementsByTagName('LI');	// Get an array of all menu items
			var item = menuItems[0];
			for(var no=0;no<menuItems.length;no++){
				// No children var set ?
				
				var noChildren = false;
				var tmpVar = menuItems[no].getAttribute('noChildren');
				if(!tmpVar)tmpVar = menuItems[no].noChildren;
				if(tmpVar=='true')noChildren=true;

				var sourceTree = false;
				var tmpVar = menuItems[no].getAttribute('sourceTree');
				if(!tmpVar)
					tmpVar = menuItems[no].sourceTree;
				if(tmpVar=='true')
					sourceTree=true;
				// No drag var set ?
				var noDrag = false;
				var tmpVar = menuItems[no].getAttribute('noDrag');
				if(!tmpVar)tmpVar = menuItems[no].noDrag;
				if(tmpVar=='true')noDrag=true;
				
				var iconfile = null;		 
				var tmpVar = menuItems[no].getAttribute('iconfile');
				if(!tmpVar)				
					tmpVar = menuItems[no].iconfile;
				if(tmpVar)
					iconfile = tmpVar;
				else {
					var pageType = menuItems[no].getAttribute('pagetype');
					if (pageType)
//						iconfile = this.imageFolder + menuItems[no].getAttribute('pagetype') +'.png';
						iconfile = this.iconFolder + menuItems[no].getAttribute('pagetype') +'.png';
					else
						iconfile = this.iconFolder + this.folderImage;
				}

				var templatefile = null;		 
				var tmpVar = menuItems[no].getAttribute('templatefile');
				if(!tmpVar)				
					tmpVar = menuItems[no].templatefile;
				if(tmpVar)
					templatefile = tmpVar;
									
				nodeId++;
				var subItems = menuItems[no].getElementsByTagName('UL');
				var img = document.createElement('IMG');
				img.src = this.imageFolder + this.plusImage;
				img.onclick = JSTreeObj.showHideNode;
				
				if(subItems.length==0)
					img.style.visibility='hidden';
				else{
					subItems[0].id = 'tree_ul_' + treeUlCounter;
					treeUlCounter++;
				}
						
				var aTag = menuItems[no].getElementsByTagName('A')[0];

//risky code
				
				if(aTag.id)
					numericId = aTag.id.replace(/[^0-9]/g,'');
				else
					numericId = (no+1);			
	
				aTag.id = menuItems[no].id + 'a';
	
				var input = document.createElement('INPUT');
				input.style.width = '40%';
				input.style.display='none';
				
				menuItems[no].insertBefore(input,aTag);
	
				input.id = menuItems[no].id + 'input';
	
				input.onblur = hideEdit;
				
				input.onkeypress = withEnter;
						
//			menuItems[no].insertBefore(img,input);
//			menuItems[no].id = 'dhtmlgoodies_treeNode' + numericId;
//			aTag.onclick = okToNavigate;				

				aTag.ondblclick = initEditLabel;	
				if(!noDrag)aTag.onmousedown = JSTreeObj.initDrag;
				if(!noChildren)aTag.onmousemove = JSTreeObj.moveDragableNodes;
				if(sourceTree)aTag.onmousedown = JSTreeObj.copyDragableNode;
								
				menuItems[no].insertBefore(img,input);

				var folderImg = document.createElement('IMG');
				
				if(!noDrag){
					if(sourceTree)
						folderImg.onmousedown = JSTreeObj.copyDragableNode;
					else
						folderImg.onmousedown = JSTreeObj.initDrag;
				}
				if(!noChildren)folderImg.onmousemove = JSTreeObj.moveDragableNodes;
				
				if(menuItems[no].className){
					folderImg.src = this.imageFolder + menuItems[no].className;
				}else{
					folderImg.src = this.imageFolder + this.folderImage;					
//					folderImg.src = this.imageFolder + this.iconFolder;					
				}
				if(iconfile)
					folderImg.src = iconfile;
				menuItems[no].insertBefore(folderImg,input);				
			}	
		
			initExpandedNodes = this.Get_Cookie('dhtmlgoodies_expandedNodes');
			if(initExpandedNodes){
				var nodes = initExpandedNodes.split(',');
/*
				for(var no=0;no<nodes.length;no++){
					if(nodes[no])this.showHideNode(false,nodes[no]);	
				}			
*/
			}			
			document.documentElement.onmousemove = JSTreeObj.moveDragableNodes;	
			
			document.documentElement.onmouseup = JSTreeObj.dropDragableNodesCopy;
			
			if(sourceTree){			
//				JSTreeObj.actionOnMouseUp = 'copy';
				this.actionOnMouseUp = 'copy';
//				console.log('copy');
//				document.documentElement.onmouseup = JSTreeObj.dropDragableNodesCopy;
			}
			else{
				this.actionOnMouseUp = 'move';
//				console.log('move');				
//				JSTreeObj.actionOnMouseUp = 'move';
//				document.documentElement.onmouseup = JSTreeObj.dropDragableNodes;
			}
		}
		,	
		
		initNode : function(node)
		{						
			var noChildren = false;
			var sourceTree = false;
			var noDrag = false;
	
			var iconfile = null;		 
			var tmpVar = node.getAttribute('iconfile');
			if(!tmpVar)				
				tmpVar = node.iconfile;
			if(tmpVar)
				iconfile = tmpVar;
			else {
				var pageType = node.getAttribute('pagetype');
				if (pageType)
					iconfile = this.iconFolder + node.getAttribute('pagetype') +'.png';
				else
					iconfile = this.iconFolder + this.folderImage;
			}

			var templatefile = null;		 
			var tmpVar = node.getAttribute('templatefile');
			if(!tmpVar)				
				tmpVar = node.templatefile;
			if(tmpVar)
				templatefile = tmpVar;

			nodeId++;
			var subItems = node.getElementsByTagName('UL');

			var aTag = node.getElementsByTagName('A')[0];

//			var input = document.createElement('INPUT');
			var input = node.getElementsByTagName('INPUT')[0];
			input.style.width = '40%';
			input.style.display='none';
				
			input.onblur = hideEdit;
			
			input.id = node.id + 'input';
				
			input.onkeypress = withEnter;
						
			aTag.onclick = okToNavigate;				

				aTag.ondblclick = initEditLabel;
				if(!noDrag)aTag.onmousedown = JSTreeObj.initDrag;
				if(!noChildren)aTag.onmousemove = JSTreeObj.moveDragableNodes;
				if(sourceTree)aTag.onmousedown = JSTreeObj.copyDragableNode;
								
				var images = node.getElementsByTagName('img');				
				var folderImg = images[1];
				
				if(!noDrag){
					if(sourceTree)
						folderImg.onmousedown = JSTreeObj.copyDragableNode;
					else
						folderImg.onmousedown = JSTreeObj.initDrag;
				}
				if(!noChildren)folderImg.onmousemove = JSTreeObj.moveDragableNodes;
				
				if(node.className){
					folderImg.src = this.imageFolder + node.className;
				}else{
					folderImg.src = this.imageFolder + this.folderImage;					
					folderImg.src = this.imageFolder + this.iconFolder;					
				}
				if(iconfile)
					folderImg.src = iconfile;
			document.documentElement.onmousemove = JSTreeObj.moveDragableNodes;
			
			document.documentElement.onmouseup = JSTreeObj.dropDragableNodesCopy;
/*			
			if(sourceTree){			
				this.actionOnMouseUp = 'copy';
			}
			else{
			*/ 
				this.actionOnMouseUp = 'move';
//			}
		}
		,			
		
		saveNewPage : function (newParentNodeId, pagetype, templatefile, pageName){
			treeStructure = new Array();
			
//			JSTreeObj.dragNode_source.id = 'temporary';	
	
			JSTreeObj.getStructure(JSTreeObj.dragNode_source, newParentNodeId);			
			showLoadingMessage("Creating...");			
			ThemesEngine.beforeCreatePage(treeStructure, JSTreeObj.getNewId);
		}	
		,
		getStructure : function(source, parentId){
					
			JSTreeObj.parentId = parentId;		
			var newParent = document.getElementById(parentId);
			var newChilds = newParent.getElementsByTagName('li');
/*
			var nodeId = source.id;
			var nodeName = (source.getElementsByTagName('A')[0]).innerHTML;
			var pageType = source.getAttribute('pagetype');
			var templateFile = source.getAttribute('templatefile');
*/			
			for (var i = 0; i < newChilds.length; i++){			

				var nodeId = newChilds[i].id;
				var nodeName = (newChilds[i].getElementsByTagName('A')[0]).innerHTML;
				var pageType = newChilds[i].getAttribute('pagetype');
				if(i != 0)
					parentId = newChilds[i].parentNode.parentNode.parentNode.id;
				var templateFile = newChilds[i].getAttribute('templatefile');		

				treeStructure.push(nodeId);		
				treeStructure.push(parentId);			
				treeStructure.push(nodeName);
				treeStructure.push(pageType);
				treeStructure.push(templateFile);				
				
				parentId = newChilds[i].getAttribute('id');
			}			
			return treeStructure;					
		}
		,
		getNewId : function(id){
			closeLoadingMessage();
			if (id == null) {
				return;
			}
			var newParent = document.getElementById(JSTreeObj.parentId);
			var newChilds = newParent.getElementsByTagName('li');		

			for(var i = 0; i < newChilds.length; i++){
				(document.getElementById(newChilds[i].id)).setAttribute("id", id[i]);	
				JSTreeObj.initNode(document.getElementById(id[i]));		
				var newName = (document.getElementById(id[i]).getElementsByTagName('A')[0]).innerHTML;
				ThemesEngine.changePageUri(id[i], newName, empty);			
			}
			var lastID = id[id.length - 1];
			setPageID(lastID);
			getPrewUrl(lastID);

		}
		,
		getNodeChilds : function(nodeParent, newParentId){
			var childs = nodeParent.getElementsByTagName('li');
			var nodeChilds = nodeParent.childNodes;
			for(var i = 0; i < childs.length; i++){
				if (nodeChilds[i].parentNode == nodeParent){
					JSTreeObj.saveNewPage(newParentId, childs[i].getAttribute('pagetype'), childs[i].getAttribute('templatefile'), 
						(childs[i].getElementsByTagName('A')[0]).innerHTML);
					JSTreeObj.getNodeChilds(nodeChilds[i], JSTreeObj.newPageId);
				}
			}
		}
		,
		setReceved : function(rec){
		JSTreeObj.receved = rec;	
		}
		,
		getReceved : function(){
			return JSTreeObj.receved;
		}
	}
	
	function withEnter(e) {
		if (window.event) {
			if (e.keyCode == 13) {
				hideEdit();
			}
		}
		else {
			if (e.which == 13) {
				hideEdit();
			}
		}
	}

	function okToNavigate()
	{
		if(editCounter<10)return true;
		return false;		
	}
	
	var editCounter = -1;
	var editEl = false;
	
	function initEditLabel()
	{	
//		alert('initEditLabel');
		if(editEl)hideEdit();
		editCounter = 0;
		editEl = this;	// Referenc to a Tag
		startEditLabel();
	}
	
	function startEditLabel()
	{
			var el = editEl.previousSibling;
			el.value = editEl.innerHTML;
			editEl.style.display='none';
			el.style.display='inline';	
			el.select();
			el.focus();
			return;

	}
	
	function showUpdate()
	{
		document.getElementById('ajaxMessage').innerHTML = ajax.response;
	}
	
	function hideEdit()
	{		

		var editObj = editEl.previousSibling;	
		
		if(editObj == null) {
			return;
		}
		
		if(editObj.value.length>0){
			editEl.innerHTML = editObj.value;
//			var changeNameId = editObj.id.replace(/[^0-9]/g,'');

console.log('editObj.id ' + editObj.id + ' editObj.id.replace '+editObj.id.replace(/[^0-9]/g,'')+' editObj.value '+editObj.value);
//console.log('editObj.id '+editObj.id+' editObj.id.replace(/[^0-9]/g,'') '+editObj.id.replace(/[^0-9]/g,''));
			var changeNameId = editObj.id.replace(/[^0-9]/g,'');
			var newName = editObj.value;
				
//			BuilderService.changeNodeName(changeNameId, newName, empty);
			BuilderService.changePageName(changeNameId, newName, empty);
			
			ThemesEngine.changePageUri(changeNameId, newName, empty);
			
//			ajax.requestFile = fileName + '?updateNode='+editObj.id.replace(/[^0-9]/g,'') + '&newValue='+editObj.value;	// Specifying which file to get
//			ajax.onCompletion = showUpdate;	// Specify function that will be executed after file has been found
//			ajax.runAJAX();		// Execute AJAX function
						
		}
		editEl.style.display='inline';
		editObj.style.display='none';
		editEl = false;			
		editCounter=-1;
	}
	
	function mouseUpEvent()
	{
		editCounter=-1;		
	}		