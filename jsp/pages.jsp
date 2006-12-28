<?xml version="1.0"?>
<jsp:root xmlns:jsp="http://java.sun.com/JSP/Page"
        xmlns:h="http://java.sun.com/jsf/html"
        xmlns:jsf="http://java.sun.com/jsf/core"
        xmlns:wf="http://xmlns.idega.com/com.idega.webface"
        xmlns:ws="http://xmlns.idega.com/com.idega.workspace"
 		xmlns:c="http://xmlns.idega.com/com.idega.content"
        xmlns:x="http://myfaces.apache.org/tomahawk"
version="1.2">
	<jsp:directive.page contentType="text/html;charset=UTF-8" pageEncoding="UTF-8"/>
	<jsf:view>
		<ws:page id="pages" javascripturls="/dwr/engine.js,
        				/dwr/interface/ThemesEngine.js,
        				/dwr/interface/BuilderService.js,
						/dwr/interface/PagePreview.js,
						/idegaweb/bundles/com.idega.content.bundle/resources/javascript/drag-drop-folder-tree.js,
        				/idegaweb/bundles/com.idega.block.web2.0.bundle/resources/javascript/scriptaculous-js-1.6.2/lib/prototype.js,
        				/idegaweb/bundles/com.idega.block.web2.0.bundle/resources/javascript/scriptaculous-js-1.6.2/src/scriptaculous.js,
        				/idegaweb/bundles/com.idega.content.bundle/resources/javascript/ThemesHelper.js,
        				/idegaweb/bundles/com.idega.content.bundle/resources/javascript/ThemesManagerHelper.js,
        				/idegaweb/bundles/com.idega.content.bundle/resources/javascript/PageInfoHelper.js,
						/idegaweb/bundles/com.idega.block.web2.0.bundle/resources/javascript/reflection/reflection.js,
						/idegaweb/bundles/com.idega.content.bundle/resources/javascript/tree.js">
			<h:form id="pagesForm">
				<jsf:verbatim>
                	<script type="text/javascript">initScript(true, false, false);</script>
                	<script type="text/javascript">getGlobalPageId();</script>
                </jsf:verbatim>
                
                <wf:wfblock maximizedVertically="true" id="pagesBlock" title="#{localizedStrings['com.idega.content']['pages']}">
                
                	<x:div id="site_tree_container" forceId="true">
	                	<wf:wfblock id="page_tree_div" styleClass="site_tree_container_pages" title="#{localizedStrings['com.idega.content']['current_site_structure']}">
							<h:outputLink value="#" onclick="if (treeObj != null) {treeObj.collapseAll()}"><h:outputText value="#{localizedStrings['com.idega.content']['collapse_all']}"/><h:outputText value=" "/></h:outputLink>
							<h:outputLink value="#" onclick="if (treeObj != null) {treeObj.expandAll()}"><h:outputText value="#{localizedStrings['com.idega.content']['expand_all']}"/></h:outputLink>								
	 						<x:div>
	 							<x:graphicImage id="trash" title="#{localizedStrings['com.idega.content']['drag_to_delete']}" forceId="true" value="/idegaweb/bundles/com.idega.content.bundle/resources/images/user-trash2.png" styleClass="recycleBin" onmouseover="treeObj.prepareToDelete();" onmouseout="treeObj.prepareToDelete();"/> 
							</x:div>
							<wf:iwtree value="#{pageCreationBean.pageSelectorTopNode}" id="current_structure_tree" var="node" varNodeToggler="t" clientSideToggle="true"	showRootNode="false">
								<jsf:facet name="PageTreeNode">
									<h:outputLink onclick="setPageID(this.parentNode.id);getPrewUrl(this.parentNode.id);getPageInfoValues();isStartPage(this.parentNode.id);return false;">
										<h:outputText value="#{node.description}" title="#{node.description}" />
									</h:outputLink>
								</jsf:facet>
							</wf:iwtree>				
						</wf:wfblock>
					</x:div>
					
					<x:div>
						<jsf:verbatim>
							<iframe id="treePages" class="pagePreviewFrame" />
						</jsf:verbatim>
					</x:div>
                
	                <x:div id="themesSliderContainer" forceId="true" styleClass="theme_slider" style="display: none">
						<x:div id="leftScrollerContainer" forceId="true" styleClass="leftThemeScroller">	
							<x:graphicImage url="/idegaweb/bundles/com.idega.content.bundle/resources/images/left.gif" onclick="scroll(this.id)" id="leftScroller" forceId="true" title="#{localizedStrings['com.idega.content']['scroll_left']}"></x:graphicImage>
						</x:div>	
								
						<x:div id="themesTickerContainer" forceId="true" styleClass="themesTicker">
							<x:div id="themes" forceId="true" styleClass="multiImageGallery"></x:div>
						</x:div>
								
						<x:div id="rightScrollerContainer" forceId="true" styleClass="rightThemeScroller">
							<x:graphicImage url="/idegaweb/bundles/com.idega.content.bundle/resources/images/right.gif" onclick="scroll(this.id)" id="rightScroller" forceId="true" title="#{localizedStrings['com.idega.content']['scroll_right']}"></x:graphicImage>
						</x:div>
					</x:div>
					
					<x:div id="pageInfoContainer" forceId="true">
						<c:PageInfo id="customizePage" styleClass="pageInfoStyle"></c:PageInfo>
					</x:div>
					
                </wf:wfblock>
                
                <x:div id="newPageContainer" forceId="true" styleClass="newPageContainerStyle" style="display: none; left: 8px;">
					<wf:iwtree value="#{siteTemplateBean.pageTree}" id="new_page_tree" var="node" varNodeToggler="t" clientSideToggle="true"	sourceTree="true"	showRootNode="false">
						<jsf:facet name="IWTreeNode">						
							<h:panelGroup>
								<h:outputLink>
									<h:outputText value="#{node.description}" />									
								</h:outputLink>
							</h:panelGroup>
						</jsf:facet>
					</wf:iwtree>
				</x:div>
				
				<jsf:verbatim>
					<script type="text/javascript">	
						treeObj = new JSDragDropTree();
						treeObj.setTreeId('current_structure_tree');
						treeObj.setMaximumDepth(7);
						treeObj.setMessageMaximumDepthReached('Maximum depth reached');
						treeObj.initTree();
						treeObj.checkIfOverTree('page_tree_div');
						treeObj.getNodeOrders();
						treeObj.expandAll();
							
						treeObj22 = new JSDragDropTree();
						treeObj22.setTreeId('new_page_tree');
						treeObj22.initTree(); 
						treeObj22.expandAll();
					</script>
				</jsf:verbatim>
                
                <x:div styleClass="pageInfoButtonsContainer">
					<x:commandButton id="newPageButton" forceId="true" type="button" onclick="newPage()" value="#{localizedStrings['com.idega.content']['new_page']}"></x:commandButton>
				</x:div>
				<x:div styleClass="rightButtonStyle">
					<x:commandButton id="saveButton" forceId="true" type="button" onclick="savePageInfo()" value="#{localizedStrings['com.idega.content']['save']}"></x:commandButton>
					<x:commandButton id="showThemesButton" forceId="true" type="button" onclick="manageSlider(this.id)" value="#{localizedStrings['com.idega.content']['hide_themes']}"></x:commandButton>
				</x:div>
			</h:form>
		</ws:page>
		<jsf:verbatim>
			<script type="text/javascript">showSlider(document.getElementById("themesSliderContainer"));resizeFrame();</script>
			<script type="text/javascript">getPageInfoValues();</script>
			<script type="text/javascript">isStartPage(getPageID());</script>
		</jsf:verbatim>
	</jsf:view>
</jsp:root>