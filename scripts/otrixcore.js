
Function.prototype._extends=function(sc){var prop;if(this==sc){return;}
var scName=sc.toString();scName=scName.substring(scName.indexOf(" ")+1,scName.indexOf("("));this.prototype[scName]=sc;for(prop in sc.prototype){if(typeof(sc.prototype[prop])=="function"){if(!this.prototype[prop]){this.prototype[prop]=sc.prototype[prop];}
else{this.prototype[scName+"_"+prop]=sc.prototype[prop];}}}};function Otrix(){this.components=[];this.PROP_ALLOW_MULTI_SELECT="oams";this.PROP_TYPE="otype";this.PROP_CHECKBOX="ocb";this.PROP_COMPONENT="ocontrol";this.PROP_DATA_ON_DEMAND="odod";this.PROP_PARENT="oParent";this.PROP_PARENT_COMPONENT="oParentComponent";this.PROP_DISABLED="odsbld";this.PROP_EXPANDED="oex";this.PROP_DEFAULT_CLASS="odc";this.PROP_DISABLED_CLASS="odsc";this.PROP_HOVER_CLASS="ohc";this.PROP_SELECTED_CLASS="osc";this.PROP_FIRE_EVENT="ofe";this.PROP_HREF="ohref";this.PROP_SELECTED="os";this.PROP_TARGET="otarget";this.PROP_FOR_COMPONENT_ID="ofcid";this.TYPE_RPC_CONTEXT="orpcContext";this.TYPE_PAGER_COMMAND="PagerCommand";var agt=navigator.userAgent.toLowerCase();this.isMozilla=(agt.indexOf('mozilla')!=-1);this.isIE=((agt.indexOf("msie")!=-1)&&(agt.indexOf("opera")==-1));this.isOpera=(agt.indexOf("opera")!=-1);this.areNodeEqual=function(n1,n2){return(!(otrix.isNull(n1)||otrix.isNull(n2))&&(n1.elm==n2.elm));};this.debug=function(tmp){var out=otrix.getElementById("otrixDebug");if(!out){return;}
var s=tmp+"\n"+out.value;s=s.substring(0,500);out.value=s;};this.addEventListener=function(e,anEvent,aListener,useCapture){if(e.addEventListener){e.addEventListener(anEvent,aListener,useCapture);}
else if(e.attachEvent){e.attachEvent("on"+anEvent,aListener);}};this.getAttribute=function(n,attrn){return(n&&n.getAttribute)?n.getAttribute(attrn):null;};this.getClientHeight=function(){var tmp=0;if(document.documentElement&&document.documentElement.clientHeight){tmp=document.documentElement.clientHeight;}
else if(document.body&&document.body.clientHeight){tmp=document.body.clientHeight;}
else if(self.innerHeight){tmp=self.innerHeight;}
return tmp;};this.getClientWidth=function(){var tmp=0;if(document.documentElement&&document.documentElement.clientWidth){tmp=document.documentElement.clientWidth;}
else if(document.body&&document.body.clientWidth){tmp=document.body.clientWidth;}
else if(self.innerWidth){tmp=self.innerWidth;}
return tmp;};this.getComponentsByType=function(type){return this.components[type];};this.getElementById=function(id){var r=null;if(typeof(id)!="string"){r=id;}
else if(otrix.NaES(id)&&document.getElementById){r=document.getElementById(id);}
return r;};this.getScrollTop=function(){var tmp=0;if(self.pageYOffset){tmp=self.pageYOffset;}
else if(document.documentElement&&document.documentElement.scrollTop){tmp=document.documentElement.scrollTop;}
else if(document.body&&document.body.scrollTop){tmp=document.body.scrollTop;}
return tmp;};this.getScrollLeft=function(){var tmp=0;if(self.pageXOffset){tmp=self.pageXOffset;}
else if(document.documentElement&&document.documentElement.scrollLeft){tmp=document.documentElement.scrollLeft;}
else if(document.body&&document.body.scrollLeft){tmp=document.body.scrollLeft;}
return tmp;};this.getType=function(id){return this.getAttribute(id,otrix.PROP_TYPE);};this.executeJSFunction=function(fname,arg){var res=true;if(otrix.NaES(fname)){if(otrix.isString(arg)){fname=otrix.stringInsertAfter(fname,arg,"(");}
res=eval(fname);}
return res;};this.executeMethod=function(obj,fname,arg){var res=true;var tmp=fname;if(otrix.NaES(tmp)){if(!otrix.isNull(obj)){tmp="obj."+tmp;}
if(!otrix.isNull(arg)){tmp=otrix.stringInsertAfter(tmp,arg,"(");}
try{res=eval(tmp);}
catch(e){}}
return res;};this.findAncestor=function(e,attrn,attrv,tag){var node=otrix.getElementById(e);while(node){if(otrix.isNull(tag)||(!otrix.isNull(tag)&&(node.nodeName.toLowerCase()==tag.toLowerCase()))){if(otrix.isNull(attrn)){break;}
if(!otrix.isNull(attrn)&&otrix.isNull(attrv)&&this.NaES(this.getAttribute(node,attrn))){break;}
if(!otrix.isNull(attrn)&&!otrix.isNull(attrv)&&(this.getAttribute(node,attrn)==attrv)){break;}}
node=node.parentNode;}
return node;};this.findDescendant=function(e,attrn,attrv,tag){var node=otrix.getElementById(e);if(!otrix.isNull(node)){node=node.firstChild;}
while(node){if(otrix.isNull(tag)||(!otrix.isNull(tag)&&(node.nodeName.toLowerCase()==tag.toLowerCase()))){if(otrix.isNull(attrn)){break;}
if(!otrix.isNull(attrn)&&otrix.isNull(attrv)&&this.NaES(this.getAttribute(node,attrn))){break;}
if(!otrix.isNull(attrn)&&!otrix.isNull(attrv)&&(this.getAttribute(node,attrn)==attrv)){break;}
var tmp=otrix.findDescendant(node,attrn,attrv,tag);if(!otrix.isNull(tmp)){node=tmp;break;}}
node=node.nextSibling;}
return node;};this.importNode=function(node,doc){if(doc.importNode){return doc.importNode(node,true);}
var clone=null;if(!otrix.isNull(node)&&!otrix.isNull(doc)){clone=doc.createElement(node.tagName);clone.id=node.id;var attributes=node.attributes;for(var i=0;i<attributes.length;i++){var attribute=attributes[i];if(!(typeof attribute.nodeName=='string'&&attribute.nodeValue==='')&&!(typeof attribute.nodeValue=='object'&&attribute.nodeValue==null&&!(attribute.nodeName=='style'))){clone.setAttribute(attribute.nodeName,attribute.nodeValue);}}
otrixClone=new OtrixDomNode(clone);if(otrixClone.canSetInnerHTML()){otrixClone.elm.innerHTML=node.innerHTML;}else{for(var i2=0;i2<node.childNodes.length;i2++){clone.appendChild(otrix.importNode(node.childNodes[i2],doc));}}}
return clone;};this.init=function(){otrix.factory.addClass(otrix.TYPE_RPC_CONTEXT,OtrixRPCContext);otrix.factory.addClass(otrix.TYPE_PAGER_COMMAND,OtrixPagerCommand);otrix.addEventListener(document,otrix.event.EVENT_MOUSE_OVER,otrix.onMouseOver,false);otrix.addEventListener(document,otrix.event.EVENT_CLICK,otrix.onClick,false);otrix.addEventListener(document,otrix.event.EVENT_DOUBLECLICK,otrix.onDoubleClick,false);otrix.addEventListener(otrix.event.EVENT_FOCUS,otrix.onFocus,false);otrix.addEventListener(otrix.event.EVENT_BLUR,otrix.onBlur,false);if(otrix.isIE){otrix.addEventListener(document,otrix.event.EVENT_KEY_DOWN,otrix.onKeyDown,false);}
else{otrix.addEventListener(document,otrix.event.EVENT_KEY_PRESS,otrix.onKeyDown,false);}
otrix.registerComponents();};this.isComponent=function(n){return this.getAttribute(n,otrix.PROP_COMPONENT);};this.isNull=function(v){return(typeof v=='object'&&!v)||(typeof v=='undefined');};this.isNumber=function(v){return typeof v=='number'&&isFinite(v);};this.isOfType=function(n,t){return this.getAttribute(n,otrix.PROP_TYPE)==t;};this.isString=function(v){return typeof v=='string';};this.onPartialRefresh=function(rpcContextId){var ctx=otrix.rpcpool.getRPCContextById(rpcContextId);if(!otrix.isNull(ctx)){var callback=ctx.getCallback();if(!otrix.isNull(callback)){callback(ctx);}
ctx.release();}};this.onPaging=function(ctx){if(!otrix.isNull(ctx)){var srcPagerCmd=otrix.getUIElementById(ctx.getTarget());if(!otrix.isNull(srcPagerCmd)){var inputForComponent=ctx.getElementById("ofcid");if(!otrix.isNull(inputForComponent)){var childrenContainerId=inputForComponent.value+"_cc";var hasResultForComponent=!otrix.isNull(ctx.getElementById("ofcod",document));if(hasResultForComponent){var srcChildrenContainer=otrix.getElementById(childrenContainerId);if(!otrix.isNull(srcChildrenContainer)){var newChildrenContainer=ctx.importElementById(childrenContainerId,document);if(!otrix.isNull(newChildrenContainer)){var pn=srcChildrenContainer.parentNode;if(!otrix.isNull(pn)){pn.replaceChild(newChildrenContainer,srcChildrenContainer);}}}}
var hasResultForPager=!otrix.isNull(ctx.getElementById("opod",document));if(hasResultForPager){var srcPager=srcPagerCmd.getPager();if(!otrix.isNull(srcPager)){var pager=ctx.getElementById(srcPager.elm.id);if(!otrix.isNull(pager)){srcPager.setOuterHTML(pager);}}}}}}};this.pad=function(str,len,pad){var result=str+"";var tmp="";for(var i=0;i<(len-result.length);i++){tmp+=pad;}
return tmp+result;};this.registerComponent=function(comp){if(otrix.isNull(comp)){return;}
var type=comp.getType();var comps=this.components[type];if(otrix.isNull(comps)){this.components[type]=new Array();comps=this.components[type];}
comps[comps.length]=comp.elm.id;};this.registerComponents=function(){var controls=otrix.searchComponents();for(var i=0;i<controls.length;i++){otrix.registerComponent(controls[i]);}};this.searchComponents=function(){var divs=document.getElementsByTagName("div");var controls=new Array();for(var i=0;i<divs.length;i++){if(otrix.isComponent(divs[i])){controls[controls.length]=otrix.getUIElementById(divs[i]);}}
return controls;};this.setAttribute=function(e,n,v){if(e&&e.setAttribute&&this.getAttribute(n)!=v){e.setAttribute(n,v+"");return true;}
return false;};this.stringInsert=function(str1,str2,pos){if(!pos){pos=0;}
return str1.substring(0,pos)+str2+str1.substring(pos,str1.length);};this.fireEvent=function(frm,eventSource,eventName,target,partialRefreshSrc,partialRefreshCallback,params){frm["com.otrix.faces.EVENT_SOURCE"].value=eventSource;frm["com.otrix.faces.EVENT_NAME"].value=eventName;if(otrix.NaES(partialRefreshSrc)){var iframe=otrix.rpcpool.getRPCContext();iframe.submit(frm,partialRefreshSrc,partialRefreshCallback,params);}else{var ot=frm.target;if(otrix.NaES(target)){frm.target=target;}
frm.submit();if(!otrix.isNull(target)){frm.target=ot;}}};this.formToQueryString=function(frm){var qs="";if(!otrix.isNull(frm)){qs=frm.action;for(var i=0;i<frm.elements.length;i++){var anElm=frm.elements[i];if(anElm.type!="submit"&&anElm.type!="reset"&&anElm.type!="image"){qs=qs+"&"+anElm.name+"="+anElm.value;}}
if(qs.length>0){qs="?"+qs.substr(1);}}
return qs;};this.NaES=function(str1){return(otrix.isString(str1)&&!otrix.isNull(str1.length)&&str1.length>0)?true:false;};this.stringInsertAfter=function(str1,str2,str3){var pos=0;str3+="";pos=str1.indexOf(str3);return otrix.stringInsert(str1,str2,pos+1);};this.toEvent=function(e){return new OtrixEvent(e);};}
var otrix=new Otrix();function OtrixEventQueue(e){this.elm=otrix.getElementById(e+"_eventQueue");this.SEPARATOR="::";this.queueEvent=function(evtSrc,evtName,evtValue){this.removeEvent(evtSrc,evtName);var tmp="";if(!otrix.isNull(evtSrc)){tmp=evtSrc;}
if(!otrix.isNull(evtName)){tmp+=(this.SEPARATOR+evtName);}
if(!otrix.isNull(evtValue)){tmp+=(this.SEPARATOR+evtValue);}
tmp+=";";tmp+=this.getValue();this.setValue(tmp);};this.findEvent=function(evtSrc,evtName){var tmp=this.getValue();return tmp.indexOf(evtSrc+this.SEPARATOR+evtName);};this.getValue=function(){return this.elm.value;};this.removeEvent=function(evtSrc,evtName){var idx=this.findEvent(evtSrc,evtName);if(idx>-1){var tmp=this.getValue();var idxSep=tmp.indexOf(";",idx);if(idxSep>-1){tmp=tmp.substring(0,idx)+tmp.substring(idxSep+1,tmp.length);this.setValue(tmp);}}};this.setValue=function(val){this.elm.value=val;};}
function OtrixEventPackage(){this.EVENT_BLUR="blur";this.EVENT_CLICK="click";this.EVENT_CONTEXT_MENU="contextmenu";this.EVENT_DOUBLECLICK="dblclick";this.EVENT_FOCUS="focus";this.EVENT_KEY_DOWN="keydown";this.EVENT_KEY_PRESS="keypress";this.EVENT_KEY_UP="keyup";this.EVENT_MOUSE_DOWN="mousedown";this.EVENT_MOUSE_OVER="mouseover";this.EVENT_MOUSE_OUT="mouseout";this.EVENT_MOUSE_UP="mouseup";this.EVENT_SELECT_START="selectstart";this.EVENT_COLLAPSE="collapse";this.EVENT_EXPAND="expand";this.MOUSE_RIGHT_BUTTON=2;this.KEY_DOWN_ARROW=40;this.KEY_DOWN_ARROW_OPERA=57374;this.KEY_ESCAPE=27;this.KEY_LEFT_ARROW=37;this.KEY_LEFT_ARROW_OPERA=57375;this.KEY_MINUS=109;this.KEY_MINUS_OPERA=57381;this.KEY_PLUS=107;this.KEY_PLUS_OPERA=57380;this.KEY_RIGHT_ARROW=39;this.KEY_RIGHT_ARROW_OPERA=57376;this.KEY_RETURN=13;this.KEY_UP_ARROW=38;this.KEY_UP_ARROW_OPERA=57373;this.isDownArrow=function(key){return(key==this.KEY_DOWN_ARROW||key==this.KEY_DOWN_ARROW_OPERA);};this.isLeftArrow=function(key){return(key==this.KEY_LEFT_ARROW||key==this.KEY_LEFT_ARROW_OPERA);};this.isMinus=function(key){return(key==this.KEY_MINUS||key==this.KEY_MINUS_OPERA);};this.isPlus=function(key){return(key==this.KEY_PLUS||key==this.KEY_PLUS_OPERA);};this.isRightArrow=function(key){return(key==this.KEY_RIGHT_ARROW||key==this.KEY_RIGHT_ARROW_OPERA);};this.isUpArrow=function(key){return(key==this.KEY_UP_ARROW||key==this.KEY_UP_ARROW_OPERA);};}
otrix.event=new OtrixEventPackage();function OtrixEvent(evt){if(evt){this.event=evt;}
else if(window.event){this.event=window.event;}
if(!this.event){return;}
this.type=(this.event.type)?this.event.type:"";if(!otrix.isNull(this.type)&&this.type.indexOf("key")!=-1&&!otrix.isNull(this.event.rangeParent)){this.target=this.event.rangeParent;}
else if(this.event.target){this.target=this.event.target;}
else if(this.event.srcElement){this.target=this.event.srcElement;}
if(this.event.relatedTarget){this.relatedTarget=this.event.relatedTarget;}
else if(this.event.fromElement&&this.type=="mouseover"){this.relatedTarget=this.event.fromElement;}else if(this.event.toElement&&this.type=="mouseout"){this.relatedTarget=this.event.toElement;}
if(this.event.charCode&&this.event.charCode>0){this.keyCode=this.event.charCode;}
else if(this.event.keyCode){this.keyCode=this.event.keyCode;}
else if(this.event.which){this.keyCode=this.event.which;}
if(this.event.pageX||this.event.pageY){this.pageX=this.event.pageX;this.pageY=this.event.pageY;}
else if(this.event.clientX||this.event.clientY){this.pageX=this.event.clientX+document.body.scrollLeft;this.pageY=this.event.clientY+document.body.scrollTop;}
if(this.type=="mousedown"||this.type=="mouseup"){if(this.event.button){this.button=this.event.button;}
else if(this.event.which){this.button=this.event.which;}}
this.stopPropagation=function(){if(this.event.cancelBubble){this.event.cancelBubble=true;}
if(this.event.stopPropagation){this.event.stopPropagation();}};this.preventDefault=function(){if(this.event.returnValue){this.event.returnValue=false;}
if(this.event.preventDefault){this.event.preventDefault();}};}
function OtrixFactory(){this.addClass=function(cls,cp){this[cls]=cp;};this.getInstance=function(id){var r=null;var e=otrix.getElementById(id);if(!otrix.isNull(e)){var cn=this[otrix.getType(e)];if(!otrix.isNull(cn)){r=new cn(e);}
else{r=new OtrixDomNode(e);}}
return r;};}
otrix.factory=new OtrixFactory();function OtrixRPCPool(){this.RPC_CTX_NAME_PREFIX="otrixRPC";this.poolSize=0;this.getRPCContextById=function(id){return otrix.getUIElementById(id);};this.getRPCContext=function(){var result=null;for(i=0;i<otrix.rpcpool.poolSize;i++){var rpcCtx=this.getRPCContextById(otrix.rpcpool.RPC_CTX_NAME_PREFIX+i);if(!otrix.isNull(rpcCtx)){if(rpcCtx.isAvailable()){result=rpcCtx;}}}
if(otrix.isNull(result)){result=otrix.rpcpool.createRPCContext();result=otrix.getUIElementById(result);}
if(!otrix.isNull(result)){result.acquire();}
return result;};this.createRPCContext=function(){var name=otrix.rpcpool.RPC_CTX_NAME_PREFIX+otrix.rpcpool.poolSize;var iframe=document.createElement('IFRAME');iframe.setAttribute(otrix.PROP_TYPE,otrix.TYPE_RPC_CONTEXT);iframe.style.border='0px';iframe.style.width='0px';iframe.style.height='0px';iframe.name=name;iframe.id=name;document.body.appendChild(iframe);otrix.rpcpool.poolSize++;return iframe;};}
otrix.rpcpool=new OtrixRPCPool();otrix.addEventListener(window,"load",otrix.init,false);