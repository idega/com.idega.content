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
        				/idegaweb/bundles/com.idega.content.bundle/resources/javascript/ThemesManagerHelper.js,
        				/idegaweb/bundles/com.idega.content.bundle/resources/javascript/PageInfoHelper.js,
						/idegaweb/bundles/com.idega.block.web2.0.bundle/resources/javascript/reflection/reflection.js,
						/idegaweb/bundles/com.idega.content.bundle/resources/javascript/tree.js">
			<h:form id="pagesForm">
				<jsf:verbatim>
                	<script type="text/javascript">initScript(true, false, false);</script>
                </jsf:verbatim>
                
                <wf:wfblock maximizedVertically="true" id="pagesBlock" title="#{localizedStrings['com.idega.content']['pages']}">
                
                	<wf:container id="page_tree_div" styleClass="current_structure">
							<jsf:verbatim>
								<h3>Current Structure</h3>
								<a href="#" onclick="treeObj.collapseAll()">Collapse all </a>
								<a href="#" onclick="treeObj.expandAll()">Expand all</a>
							</jsf:verbatim>
			
							<wf:iwtree value="#{pageCreationBean.pageSelectorTopNode}" id="page_chooser" var="node" varNodeToggler="t" clientSideToggle="true"	showRootNode="false">
								<jsf:facet name="PageTreeNode">
									<h:outputLink onclick="getPrewUrl(this.parentNode.id);return false;">
										<h:outputText value="#{node.description}" />
									</h:outputLink>
								</jsf:facet>
							</wf:iwtree>				
						</wf:container>
					
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
					
						<x:div styleClass="leftButtonStyle">
							<x:commandButton id="newPageButton" forceId="true" type="button" onclick="newPage()" value="#{localizedStrings['com.idega.content']['create_page']}"></x:commandButton>
						</x:div>
						<x:div styleClass="rightButtonStyle">
							<x:commandButton id="saveButton" forceId="true" type="button" onclick="savePageInfo()" value="#{localizedStrings['com.idega.content']['save']}"></x:commandButton>
							<x:commandButton id="showThemesButton" forceId="true" type="button" onclick="manageSlider(this.id)" value="#{localizedStrings['com.idega.content']['hide_themes']}"></x:commandButton>
						</x:div>
					
					<jsf:verbatim>
						<script type="text/javascript">	
							treeObj = new JSDragDropTree();
							treeObj.setTreeId('page_tree_div');
							treeObj.setMaximumDepth(7);
							treeObj.setMessageMaximumDepthReached('Maximum depth reached'); // If you want to show a message when maximum depth is reached, i.e. on drop.
							treeObj.initTree();
							treeObj.getNodeOrders();
							treeObj.expandAll();
						</script>
					</jsf:verbatim>
                </wf:wfblock>
                <jsf:verbatim>
                	<script type="text/javascript">showSlider(document.getElementById("themesSliderContainer"));resizeFrame();</script>
                </jsf:verbatim>
			</h:form>
		</ws:page>
	</jsf:view>
</jsp:root>