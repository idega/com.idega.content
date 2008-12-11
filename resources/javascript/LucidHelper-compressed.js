
if(LucidHelper==null)var LucidHelper={};LucidHelper.thickBoxResources=null;LucidHelper.pagePermissionsWindowUri=null;LucidHelper.pagePropertiesWindowUri=null;var RESERVED_WIDTH=280;var SHOW_ELEMENT_TRANSITION_DURATION=500;var KEYWORDS=null;var IB_SOURCE_VIEW_CLASS='com.idega.builder.presentation.IBSourceView';function setValueToHiddentPageInfoElement(input){var hiddenInputId=input.getProperty('radioButtonCode');if(hiddenInputId==null){return false;}
var hiddenInput=$(hiddenInputId);if(hiddenInput==null){return false;}
var inputValue=input.getProperty('value');if(inputValue==null||inputValue==''){inputValue='-1';}
hiddenInput.setProperty('value',inputValue);}
function savePageInfoWithRadioButtonValue(id){var radio=$(id);if(radio==null){return false;}
setValueToHiddentPageInfoElement(radio);savePageInfo();}
function savePageInfo(){showLoadingMessage(getThemeSavingText());if(KEYWORDS==null){ThemesEngine.getPageInfoElements(getPageInfoElementsCallback);}
else{getPageInfoElementsCallback(KEYWORDS);}}
function getPageInfoElementsCallback(allKeywords){var pageId=getPageID();if(pageId==null||allKeywords==null){closeLoadingMessage();return;}
KEYWORDS=allKeywords;var keywords=new Array();var values=new Array();var element=null;var treeNode=null;var needReload=false;for(var i=0;i<allKeywords.length;i++){element=$(allKeywords[i]);if(element!=null){keywords.push(allKeywords[i]);var pageInfoValue=element.getProperty('value');values.push(pageInfoValue);if(allKeywords[i]=='pageTitle'&&pageInfoValue!=''){treeNode=$(pageId);if(treeNode==null){needReload=true;}
else{var nodeLinks=treeNode.getElements('a[class=pageTreeNames]');if(nodeLinks==null||nodeLinks.length==0){needReload=true;}
else{var nodeLink=$(nodeLinks[0]);nodeLink.empty();nodeLink.setText(pageInfoValue);}}}}}
ThemesEngine.savePageInfo(pageId,keywords,values,{callback:function(result){if(needReload){reloadPage();return;}
if(result!=null){var pageUri=$('pageUri');if(pageUri!=null){pageUri.value=result;}}
closeLoadingMessage();getPrewUrl(getPageID());}});}
function setButtonText(id,text){var button=$(id);if(button!=null){button.setText(text);}}
function newPages(containerId,buttonId,buttonText,positionFromLeft){var container=$(containerId);if(container==null){return false;}
var containerIsOpened=false;var displayValue=container.getStyle('display');if(displayValue!=null){containerIsOpened=displayValue=='block';}
if(containerIsOpened){closeNewPage(container,buttonId,buttonText);}
else{container.setStyle('left',positionFromLeft);container.setStyle('visibility','hidden');container.setStyle('display','block');var showNewPage=new Fx.Style(container,'opacity',{duration:SHOW_ELEMENT_TRANSITION_DURATION});showNewPage.start(0,1);}}
function resizeFrame(){var container=$('pagePreviewContainer');if(container==null){return;}
var availableWidth=getTotalWidth()-RESERVED_WIDTH-6;if(availableWidth>0){container.setStyle('width',availableWidth+'px');}
var availableHeight=getTotalHeight()-91;if(availableHeight>0){container.setStyle('height',availableHeight+'px');}}
function getPageInfoValues(){if(KEYWORDS==null){ThemesEngine.getPageInfoElements(getAvailableElements);}
else{getAvailableElements(KEYWORDS);}}
function getAvailableElements(allKeywords){if(allKeywords==null){return;}
KEYWORDS=allKeywords;var pageId=getPageID();ThemesEngine.getRenderedPageInfo(pageId,'customizePage','pageInfoStyle_accordion',{callback:function(component){var container=$('pageInfoToggle');container.empty();insertNodesToContainer(component,container);registerPageInfoActions();ThemesEngine.getPageInfoValues(pageId,allKeywords,showPageInfoValues);}});}
function showPageInfoValues(values){if(values==null){if(KEYWORDS!=null){for(var i=0;i<KEYWORDS.length;i++){element=$(KEYWORDS[i]);if(element!=null){element.value='';}}}
return false;}
if(KEYWORDS.length!=values.length){return;}
var element=null;for(var i=0;i<KEYWORDS.length;i++){element=$(KEYWORDS[i]);if(element==null||element.getProperty('type')=='hidden'){element=$(KEYWORDS[i]+'_'+values[i]);if(element!=null&&values[i]!=null&&values[i]!=''){element.setProperty('checked',true);element.checked=true;}}
else{element.value=values[i];}}}
function isStartPage(pageID){ThemesEngine.isStartPage(pageID,isStartPageCallback);}
function isStartPageCallback(isStart){var button=$('makeStartPage');if(button==null){return;}
button.disabled=isStart;if(isStart){button.value=getStartPageText();}
else{button.value=getMakeStartPageText();button.title=getMakeThisPageStartPageText();}}
function closeNewPage(container,buttonId,buttonText){if(container!=null){var hideNewPage=new Fx.Style(container,'opacity',{duration:SHOW_ELEMENT_TRANSITION_DURATION});hideNewPage.start(1,0);window.setTimeout("setDisplayPropertyToElement('"+container.id+"', 'none', null)",SHOW_ELEMENT_TRANSITION_DURATION);}}
function managePageInfoComponents(e){removeStyleOptions(e);}
function initializePages(){initScript(true,false,false);getGlobalPageId();resizeFrame();isStartPage(getPageID());checkIfNotEmptySiteTree(ALL_CURRENT_SITE_STRUCTURE_TREE_ID);document.addEvent('click',function(e){e=new Event(e);managePageInfoComponents(e);});resizeTreeContainerInThemes(RESERVED_HEIGHT_FOR_PAGES);resizeNewPageContainers(150);BuilderService.getClassNameForSourceView({callback:function(className){IB_SOURCE_VIEW_CLASS=className;}});}
function registerPageInfoActions(){$$('a.newPageButtonStyleClass').each(function(element){setHrefToVoidFunction(element);element.removeEvents('click');element.addEvent('click',function(){newPages('newPageContainer','newPageButton',getNewPageText(),element.getLeft());});});$$('a.newPagesButtonStyleClass').each(function(element){setHrefToVoidFunction(element);element.removeEvents('click');element.addEvent('click',function(){newPages('newPagesContainer','newPagesButton',NEW_PAGES_TEXT,element.getLeft());});});$$('a.pagePropertiesButtonStyleClass').each(function(element){setHrefToVoidFunction(element);element.removeEvents('click');element.addEvent('click',function(){showLoadingMessage(LOADING_TEXT);if(LucidHelper.thickBoxResources==null||LucidHelper.pagePropertiesWindowUri==null){LucidEngine.getPropertiesWindowResources({callback:function(resources){if(resources==null){return false;}
LucidHelper.pagePropertiesWindowUri=resources[0];LucidHelper.thickBoxResources=new Array();for(var i=1;i<resources.length;i++){LucidHelper.thickBoxResources.push(resources[i]);}
LucidHelper.showPagePropertiesWindow();}});}
else{LucidHelper.showPagePropertiesWindow();}});});$$('a.pagePermissionsButtonStyleClass').each(function(element){setHrefToVoidFunction(element);element.removeEvents('click');element.addEvent('click',function(){showLoadingMessage(LOADING_TEXT);if(LucidHelper.thickBoxResources==null||LucidHelper.pagePermissionsWindowUri==null){LucidEngine.getPermissionWindowResources({callback:function(resources){if(resources==null){return false;}
LucidHelper.pagePermissionsWindowUri=resources[0];LucidHelper.thickBoxResources=new Array();for(var i=1;i<resources.length;i++){LucidHelper.thickBoxResources.push(resources[i]);}
LucidHelper.showPagePermissionsWindow();}});}
else{LucidHelper.showPagePermissionsWindow();}});});$$('input.saveButtonStyleClass').each(function(element){element.removeEvents('click');element.addEvent('click',savePageInfo);});$$('a.showThemesButtonStyleClass').each(function(element){setHrefToVoidFunction(element);element.removeEvents('click');var manageSliderFunction=function(){manageSlider(element.id);};element.addEvent('click',function(){if(!SHOW_SOURCE_PAGES){manageSliderFunction();}
return false;});});$$('a.showSourcePagesButtonStyleClass').each(function(button){setHrefToVoidFunction(button);button.removeEvents('click');button.addEvent('click',function(){if(button.hasClass('viewingPageSourceInLucid')){SHOW_SOURCE_PAGES=false;SHOW_EDIT_PAGES=false;if(SHOW_SOURCE_PAGES){hideThemesSliderInPages($('themesSliderContainer'));}
jQuery('#showPageModules').removeClass('active');jQuery('#showEditPagesButton').removeClass('activeButtonInPages');jQuery('#showSourcePagesButton').removeClass('activeButtonInPages');if(!button.hasClass('activeButtonInPages')){button.addClass('activeButtonInPages');}
getPageUriByCheckedId();button.removeClass('viewingPageSourceInLucid');button.removeClass('activeButtonInPages');return false;}
button.addClass('viewingPageSourceInLucid');SHOW_SOURCE_PAGES=true;SHOW_EDIT_PAGES=false;if(SHOW_SOURCE_PAGES){hideThemesSliderInPages($('themesSliderContainer'));}
jQuery('#showPageModules').removeClass('active');jQuery('#showThemesButton').removeClass('active');jQuery('#showEditPagesButton').removeClass('activeButtonInPages');jQuery('#showPreviewPagesButton').removeClass('activeButtonInPages');if(!button.hasClass('activeButtonInPages')){button.addClass('activeButtonInPages');}
getPageUriByCheckedId();});});$$('img.closeNewPageOrPagesStyle').each(function(image){image.removeEvents('click');image.addEvent('click',function(){if(image.getProperty('id')=='closeNewPagesContainer'){closeNewPage($('newPagesContainer'),'newPagesButton',NEW_PAGES_TEXT);}
else{closeNewPage($('newPageContainer'),'newPageButton',getNewPageText());}});});registerActionsForSiteTree();boldCurrentTreeElement();}
LucidHelper.showPagePermissionsWindow=function(){LazyLoader.loadMultiple(LucidHelper.thickBoxResources,function(){tb_fullInit();var uri=LucidHelper.pagePermissionsWindowUri+'&ic_permissionobj_identifier='+getPageID()+'&ic_permission_category=3&TB_iframe=true&modal=true&height=300&width=400';tb_show('',uri,null,null);closeAllLoadingMessages();});}
LucidHelper.showPagePropertiesWindow=function(){LazyLoader.loadMultiple(LucidHelper.thickBoxResources,function(){tb_fullInit();var uri=LucidHelper.pagePropertiesWindowUri+'&ib_page='+getPageID()+'&TB_iframe=true&modal=true&height=500&width=600';tb_show('',uri,null,null);closeAllLoadingMessages();});}
LucidHelper.reloadFrame=function(){window.treePages.location.reload();}
LucidHelper.setSelectedLocale=function(){var locale=DWRUtil.getValue('lucidLocaleSwitcher');if(locale==null||locale==''||locale=='-1'){return false;}
showLoadingMessage(LOADING_TEXT);LucidEngine.setLocale(locale,{callback:function(success){if(success){window.location.reload();}
closeAllLoadingMessages();}});}
function setHrefToVoidFunction(element){element.setProperty('href','javascript:void(0)');}
function registerActionsForTemplatesInLucid(){$$('a.templateNameInLucidTemplatesTreeStyle').each(function(element){element.removeEvents('click');element.addEvent('click',function(){var templateId=element.getProperty('templateid');if(templateId==null){return false;}
WORKING_WITH_TEMPLATE=true;var allTemplates=$$('a.templateNameInLucidTemplatesTreeStyle');var template=null;for(var i=0;i<allTemplates.length;i++){template=allTemplates[i];if(template!=element){template.setStyle('font-weight','normal');}}
element.setStyle('font-weight','bold');TEMPLATE_ID=templateId;getPrewUrl(templateId);});});$$('input.createChildTemplateForCurrentTemplateButtonInLucidStyle').each(function(element){element.removeEvents('click');element.addEvent('click',function(){if(TEMPLATE_ID==null){alert(SELECT_TEMPLATE_FIRST_TEXT);return false;}
WORKING_WITH_TEMPLATE=true;showLoadingMessage(CREATING_TEXT);ThemesEngine.createChildTemplateForThisTemplate(TEMPLATE_ID,{callback:function(newTemplateId){closeAllLoadingMessages();if(newTemplateId==null){return false;}
TEMPLATE_ID=newTemplateId;var newTemplateElement=$(document.body).getElement('a[templateid='+newTemplateId+']');if(newTemplateElement!=null){newTemplateElement.setStyle('font-weight','bold');}
getPrewUrl(newTemplateId);}});});});}
function updateSiteTemplatesTree(tree){if(tree==null){return false;}
var container=$('templatesTreeToggle');if(container==null){return false;}
container.empty();insertNodesToContainer(tree,container);registerActionsForTemplatesInLucid();}
function getFixedPageIdFromElementId(id){if(id==null){return null;}
return id.replace('a','');}
function analyzeAndDeletePage(){hideContextMenu();var elementId=$('deletePageButtonCtxMn').getProperty('pageid');if(!JSTreeObj){return false;}
movingNode=true;JSTreeObj.deleteNodes=true;JSTreeObj.dragDropTimer=50;var containerOfElementBeingDeleted=null;if(elementId.indexOf('a')==-1){containerOfElementBeingDeleted=$(elementId);}
else{containerOfElementBeingDeleted=$(elementId).getParent();}
if(containerOfElementBeingDeleted==null){return false;}
JSTreeObj.dragNode_source=containerOfElementBeingDeleted;JSTreeObj.dragNode_parent=containerOfElementBeingDeleted.getParent();var parentElement=null;try{parentElement=containerOfElementBeingDeleted.getParent().getParent();}catch(e){}
if(parentElement==null){return false;}
var nodeBeingDeletedId=containerOfElementBeingDeleted.getProperty('id');if(nodeBeingDeletedId==null){return false;}
var parentNodeIdOfNodeBeingDeleted=parentElement.getProperty('id');if(parentNodeIdOfNodeBeingDeleted==null){return false;}
JSTreeObj.previousPlaceInLevel=JSTreeObj.getOrderInLevel(nodeBeingDeletedId,parentNodeIdOfNodeBeingDeleted);JSTreeObj.previousParentId=parentNodeIdOfNodeBeingDeleted;previousSiteTreeNode=containerOfElementBeingDeleted.getPrevious();nextSiteTreeNode=containerOfElementBeingDeleted.getNext()
JSTreeObj.dropDragableNodesCopy(nodeBeingDeletedId);movingNode=false;}
function analyzeAndMakePageAsStartPage(){hideContextMenu();var elementId=getFixedPageIdFromElementId($('makePageStartPageButtonCtxMn').getProperty('pageid'));showLoadingMessage(getChangingStructureText());ThemesEngine.setAsStartPage(elementId,{callback:function(result){closeAllLoadingMessages();}});}
function setPageAccessibilityProperty(id){hideContextMenu();var clickedElement=$(id);if(clickedElement==null){return false;}
var pageId=clickedElement.getProperty('pageid');var code=clickedElement.getProperty('pp_code');var value=clickedElement.getProperty('pp_value');var columnName=clickedElement.getProperty('pp_column');showLoadingMessage(SAVING_THEME);ThemesEngine.setPageAccessibilityProperty(pageId,code,value,columnName,{callback:function(result){closeAllLoadingMessages();if(result!=null){if(pageId==getPageID()){getPrewUrl(pageId);getPageInfoValues();}}}});}