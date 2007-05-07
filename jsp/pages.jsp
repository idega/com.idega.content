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
						/idegaweb/bundles/com.idega.block.web2.0.bundle/resources/javascript/prototype/1.5.0/prototype.js,
						/idegaweb/bundles/com.idega.block.web2.0.bundle/resources/javascript/behaviour.js,
        				/idegaweb/bundles/com.idega.block.web2.0.bundle/resources/javascript/mootools/1.0.0/mootools-all-compressed.js,
        				/idegaweb/bundles/com.idega.block.web2.0.bundle/resources/javascript/reflection/reflection.js,
        				/idegaweb/bundles/com.idega.content.bundle/resources/javascript/drag-drop-folder-tree.js,
        				/idegaweb/bundles/com.idega.content.bundle/resources/javascript/ThemesHelper.js,
        				/idegaweb/bundles/com.idega.content.bundle/resources/javascript/ThemesManagerHelper.js,
        				/idegaweb/bundles/com.idega.content.bundle/resources/javascript/PageInfoHelper.js,
						/idegaweb/bundles/com.idega.content.bundle/resources/javascript/tree.js">
			<h:form id="pagesForm">
				<jsf:verbatim>
                	<script type="text/javascript">
                		registerEvent(window, "load", startBuilderApplication);
                		registerEvent(window, "load", getLocalizedTextForThemes);
                		registerEvent(window, "load", initializePages);
                		registerEvent(window, "load", getPathToImageFolder);
                		registerEvent(window, "load", registerPageInfoActions);
                	</script>
                </jsf:verbatim>
                
                <wf:wfblock maximizedVertically="true" id="pagesBlock" title="#{localizedStrings['com.idega.content']['pages']}">
 
					<x:div id="site_tree_container" forceId="true">
	                	<c:block_with_toolbar id="page_tree_div" styleClass="site_tree_container_pages" title="#{localizedStrings['com.idega.content']['current_site_structure']}" collapseAllValue="#{localizedStrings['com.idega.content']['collapse_all']}" expandAllValue="#{localizedStrings['com.idega.content']['expand_all']}" trashCanImage="/idegaweb/bundles/com.idega.content.bundle/resources/images/user-trash.png">
							<wf:iwtree value="#{pageCreationBean.pageSelectorTopNode}" id="current_structure_tree" var="node" varNodeToggler="t" clientSideToggle="true"	showRootNode="false">
								<jsf:facet name="PageTreeNode">
									<h:outputLink styleClass="pageTreeNames">
										<h:outputText value="#{node.description}"/>
									</h:outputLink>
								</jsf:facet>
							</wf:iwtree>
						</c:block_with_toolbar>
					</x:div>
					
					<x:div styleClass="pagePreviewContainer">
						<jsf:verbatim>
							<iframe id="treePages" class="pagePreviewFrame">iframe</iframe>
						</jsf:verbatim>
					</x:div>
                
                	<x:inputHidden id="defaultThemeLabel" forceId="true" value="#{localizedStrings['com.idega.content']['theme_is_default']}"></x:inputHidden>
					<x:inputHidden id="notDefaultThemeLabel" forceId="true" value="#{localizedStrings['com.idega.content']['theme_is_not_default']}"></x:inputHidden>
	                <x:div id="themesSliderContainer" forceId="true" styleClass="theme_slider" style="display: none">
						<x:div id="leftScrollerContainer" forceId="true" styleClass="leftThemeScroller">	
							<x:graphicImage url="/idegaweb/bundles/com.idega.content.bundle/resources/images/left.gif" onclick="scroll(this.id);" id="leftScroller" forceId="true" title="#{localizedStrings['com.idega.content']['scroll_left']}"></x:graphicImage>
						</x:div>	
								
						<x:div id="themesTickerContainer" forceId="true" styleClass="themesTicker">
							<x:div id="themes" forceId="true" styleClass="multiImageGallery"></x:div>
						</x:div>
								
						<x:div id="rightScrollerContainer" forceId="true" styleClass="rightThemeScroller">
							<x:graphicImage url="/idegaweb/bundles/com.idega.content.bundle/resources/images/right.gif" onclick="scroll(this.id);" id="rightScroller" forceId="true" title="#{localizedStrings['com.idega.content']['scroll_right']}"></x:graphicImage>
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
				
				<jsf:verbatim><script type="text/javascript">setIsSiteMap(false);</script></jsf:verbatim>
				<jsf:verbatim>
					<script type="text/javascript">	
						appendIdOfTree('new_page_tree');
						appendIdOfAdvancedTree('current_structure_tree');							
					</script>
				</jsf:verbatim>
                
                <x:div styleClass="pageInfoButtonsContainer">
					<x:commandButton id="newPageButton" forceId="true" styleClass="newPageButtonStyleClass" type="button" value="#{localizedStrings['com.idega.content']['new_page']}"></x:commandButton>
				</x:div>
				<x:div styleClass="rightButtonStyle">
					<x:commandButton id="saveButton" forceId="true" styleClass="saveButtonStyleClass" type="button" value="#{localizedStrings['com.idega.content']['save']}"></x:commandButton>
					<x:commandButton id="showThemesButton" forceId="true" styleClass="showThemesButtonStyleClass" type="button" value="#{localizedStrings['com.idega.content']['show_themes']}"></x:commandButton>
				</x:div>
			</h:form>
		</ws:page>
	</jsf:view>
</jsp:root>