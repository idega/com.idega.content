
function OtrixWebTreePackage(){this.TYPE_TREE="Tree";this.TYPE_TREE_NODE="TreeNode";this.FLAG_PLUS_MINUS_ICON="opm";this.PROP_COLLAPSED_ICON="oci";this.PROP_EXPANDED_ICON="oei";this.PROP_MINUS_ICON="omi";this.PROP_PLUS_ICON="opi";this.init=function(){otrix.factory.addClass(otrix.webtree.TYPE_TREE,OtrixTree);otrix.factory.addClass(otrix.webtree.TYPE_TREE_NODE,OtrixTreeNode);};this.onExpandedOnDemand=function(ctx){if(!otrix.isNull(ctx)){var eventSourceId=ctx.getTarget();if(otrix.NaES(eventSourceId)){var childId=eventSourceId+"_cc";var childrenClone=ctx.importElementById(childId,document);if(!otrix.isNull(childrenClone)){var elmSource=document.getElementById(eventSourceId);if(!otrix.isNull(elmSource)){var table=elmSource.parentNode;var nextTr=elmSource.nextSibling;if(otrix.isNull(nextTr)){table.appendChild(childrenClone);}
else{table.insertBefore(childrenClone,nextTr);}
otrix.webtree.onExpandNode(elmSource);}}}}};this.onExpandNode=function(node){node=otrix.getUIElementById(node);node.updateIcon(node.getExpandedIcon());node.updateJunctionIcon(node.getMinusIcon());node.repaint();};}
otrix.webtree=new OtrixWebTreePackage();function OtrixTree(e){this.OtrixUIComponent(e);}
OtrixTree.onKeyDown=function(evt){var xEvt=otrix.toEvent(evt);if(otrix.isNull(xEvt)){return;}
var snode=this.getSelectedElement();if(!otrix.isNull(snode)){if(otrix.event.isUpArrow(xEvt.keyCode)){var sn=snode.getPreviousNode();if(!otrix.isNull(sn)){sn.select();}
xEvt.preventDefault();}
else if(otrix.event.isDownArrow(xEvt.keyCode)){var nn=snode.getNextNode();if(!otrix.isNull(nn)){nn.select();}
xEvt.preventDefault();}
else if(otrix.event.isLeftArrow(xEvt.keyCode)){snode.collapse();xEvt.preventDefault();}
else if(otrix.event.isRightArrow(xEvt.keyCode)){snode.expand();xEvt.preventDefault();}}};OtrixTree._extends(OtrixUIComponent);function OtrixTreeNode(e){this.OtrixCommand(e);}
OtrixTreeNode.prototype.collapse=function(){var cc=this.getChildContainer();if(!otrix.isNull(cc)){cc.hide(true);var pc=this.getComponent();if(!otrix.isNull(pc)){var sn=pc.getSelectedElement();if(this.isAncestor(sn)){this.select();}}
this.updateIcon(this.getCollapsedIcon());this.updateJunctionIcon(this.getPlusIcon());this.repaint();this.queueEvent(this.elm.id,"expand","false");}};OtrixTreeNode.prototype.expand=function(){if(this.hasChild()){this.OtrixCommand_expand();otrix.webtree.onExpandNode(this.elm);}
else{this.fireEvent("expand",null,this.elm.id,otrix.webtree.onExpandedOnDemand);}};OtrixTreeNode.prototype.getExpandedIcon=function(){var tmp=this.getAttribute(otrix.webtree.PROP_EXPANDED_ICON,false);if(otrix.isNull(tmp)){tmp=this.getAttribute(otrix.webtree.PROP_COLLAPSED_ICON,false);if(!otrix.isNull(tmp)){tmp=null;}
else{tmp=this.getAttribute(otrix.webtree.PROP_EXPANDED_ICON,true);}}
return tmp;};OtrixTreeNode.prototype.getCollapsedIcon=function(){return this.getAttribute(otrix.webtree.PROP_COLLAPSED_ICON,true);};OtrixTreeNode.prototype.getPlusIcon=function(){return this.getAttribute(otrix.webtree.PROP_PLUS_ICON,true);};OtrixTreeNode.prototype.getMinusIcon=function(){return this.getAttribute(otrix.webtree.PROP_MINUS_ICON,true);};OtrixTreeNode.prototype.getNextNode=function(){var nextNode=null;if(this.isExpanded()){nextNode=this.getFirstChild();}
if(otrix.isNull(nextNode)){nextNode=this.getNextSibling();}
var pn=this.getParentNode();while(otrix.isNull(nextNode)&&!otrix.isNull(pn)){nextNode=pn.getNextSibling();pn=pn.getParentNode();}
return nextNode;};OtrixTreeNode.prototype.getParentNode=function(){var pn=this.getParent();if(!otrix.isOfType(pn,otrix.webtree.TYPE_TREE_NODE)){pn=null;}
return pn;};OtrixTreeNode.prototype.getPreviousNode=function(){var prevNode=this.getPreviousSibling();var lastChild=(!otrix.isNull(prevNode))?prevNode.getLastChild():null;while(!otrix.isNull(prevNode)&&prevNode.isExpanded()&&!otrix.isNull(lastChild)){prevNode=lastChild;lastChild=prevNode.getLastChild();}
if(otrix.isNull(prevNode)){prevNode=this.getParentNode();}
return prevNode;};OtrixTreeNode.prototype.onClick=function(evt){if(otrix.NaES(otrix.getAttribute(evt.target,otrix.webtree.FLAG_PLUS_MINUS_ICON))){this.toggle();}
else{this.select();this.click();}};OtrixTreeNode.prototype.updateIcon=function(anIcon){if(otrix.NaES(anIcon)){var icon=this.elm.cells[1].childNodes[0];if(!otrix.isNull(icon)&&icon.src!=anIcon){icon.src=anIcon;}}};OtrixTreeNode.prototype.updateJunctionIcon=function(anIcon){if(otrix.NaES(anIcon)){var icon=this.elm.cells[0].childNodes[0];if(!otrix.isNull(icon)&&icon.src!=anIcon){icon.src=anIcon;}}};OtrixTreeNode._extends(OtrixCommand);otrix.webtree.init();