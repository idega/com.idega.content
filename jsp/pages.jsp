<?xml version="1.0"?>
<jsp:root xmlns:jsp="http://java.sun.com/JSP/Page"
        xmlns:h="http://java.sun.com/jsf/html"
        xmlns:jsf="http://java.sun.com/jsf/core"
        xmlns:wf="http://xmlns.idega.com/com.idega.webface"
        xmlns:ws="http://xmlns.idega.com/com.idega.workspace"
 		xmlns:c="http://xmlns.idega.com/com.idega.content"
        xmlns:x="http://myfaces.apache.org/tomahawk"
        xmlns:web2="http://xmlns.idega.com/com.idega.block.web2.0"
        xmlns:f="http://java.sun.com/jsf/core"
version="1.2">
	<jsp:directive.page contentType="text/html;charset=UTF-8" pageEncoding="UTF-8"/>
	<jsf:view>
		<ws:page id="pages" javascripturls="
						/dwr/engine.js,
        				/dwr/interface/ThemesEngine.js,
        				/dwr/interface/BuilderService.js,
						/dwr/interface/PagePreview.js,
						
						/idegaweb/bundles/com.idega.block.web2.0.bundle/resources/javascript/mootools/1.11/mootools-all-compressed.js,
        				/idegaweb/bundles/com.idega.block.web2.0.bundle/resources/javascript/reflection/for_mootools/1.2/reflection.js,
        				
        				/idegaweb/bundles/com.idega.content.bundle/resources/javascript/drag-drop-folder-tree.js,
        				/idegaweb/bundles/com.idega.content.bundle/resources/javascript/ThemesHelper.js,
        				/idegaweb/bundles/com.idega.content.bundle/resources/javascript/ThemesManagerHelper.js,
        				/idegaweb/bundles/com.idega.content.bundle/resources/javascript/PageInfoHelper.js,
						/idegaweb/bundles/com.idega.content.bundle/resources/javascript/tree.js">
			<h:form id="pagesForm">
				<jsf:verbatim>
                	<script type="text/javascript">
                		window.addEvent('domready', startBuilderApplication);
                		window.addEvent('domready', getLocalizedTextForThemes);
                		window.addEvent('domready', initializePages);
                		window.addEvent('domready', getPathToImageFolder);
                		window.addEvent('domready', registerPageInfoActions);
                	</script>
                </jsf:verbatim>
                
                <wf:wfblock maximizedVertically="true" id="pagesBlock" title="#{localizedStrings['com.idega.content']['pages']}">
                	<x:div id="leftSide" forceId="true" styleClass="accordionInPages">
						<web2:accordion accordionId="myAccordion" includeJavascript="true">
							<f:facet name="PANELS">
								<x:div id="accordion" forceId="true">
									<x:div id="siteMapInformation" forceId="true" styleClass="acTogglemyAccordion">
										<h:outputText value="#{localizedStrings['com.idega.content']['current_site_structure']}"/>
									</x:div>
									
									<x:div id="sitemap" forceId="true" styleClass="acStretchmyAccordion">
	 									<x:div id="site_tree_container" forceId="true">
	 										<c:block_with_toolbar id="page_tree_div" styleClass="site_tree_container_site_accordion" title="#{localizedStrings['com.idega.content']['current_site_structure']}" collapseAllValue="#{localizedStrings['com.idega.content']['collapse_all']}" expandAllValue="#{localizedStrings['com.idega.content']['expand_all']}" trashCanImage="/idegaweb/bundles/com.idega.content.bundle/resources/images/user-trash.png" addStartPageButton="true">
												<wf:iwtree value="#{pageCreationBean.pageSelectorTopNode}" id="current_structure_tree" var="node" varNodeToggler="t" clientSideToggle="true"	showRootNode="false">
													<jsf:facet name="PageTreeNode">
														<h:outputLink styleClass="pageTreeNames">
															<h:outputText value="#{node.description}"/>
														</h:outputLink>
													</jsf:facet>
												</wf:iwtree>
											</c:block_with_toolbar>
										</x:div>
									</x:div>
																					
									<x:div id="pageInfo" forceId="true" styleClass="acTogglemyAccordion">
										<h:outputText value="#{localizedStrings['com.idega.content']['page_info']}"/>
									</x:div>
									
									<x:div id="pageInfoToggle" forceId="true" styleClass="acStretchmyAccordion">
										<c:PageInfo id="customizePage" styleClass="pageInfoStyle_accordion"></c:PageInfo>
									</x:div>
								</x:div>
							</f:facet>
						</web2:accordion>     
					</x:div>           
	
					<x:div styleClass="pagePreviewContainer">
						<jsf:verbatim>
							<iframe name="treePages" id="treePages" class="pagePreviewFrame">iframe</iframe>
						</jsf:verbatim>
					</x:div>
                
                	<x:inputHidden id="defaultThemeLabel" forceId="true" value="#{localizedStrings['com.idega.content']['theme_is_default']}"></x:inputHidden>
					<x:inputHidden id="notDefaultThemeLabel" forceId="true" value="#{localizedStrings['com.idega.content']['theme_is_not_default']}"></x:inputHidden>
					<x:div id="themesSliderContainer" forceId="true" styleClass="theme_slider" style="display: none">
						<x:div id="leftScrollerContainer" forceId="true" styleClass="leftThemeScroller">	
							<x:graphicImage url="/idegaweb/bundles/com.idega.content.bundle/resources/images/left.gif" onclick="scroll(this.id)" id="leftScroller" forceId="true" title="#{localizedStrings['com.idega.content']['scroll_left']}"></x:graphicImage>
						</x:div>	
						
						<x:div id="themesTickerContainer" forceId="true" styleClass="themesTicker">
							<x:div id="themes" forceId="true" styleClass="multiImageGallery" style="left: 0px;"></x:div>
						</x:div>
						
						<x:div id="rightScrollerContainer" forceId="true" styleClass="rightThemeScroller">
							<x:graphicImage url="/idegaweb/bundles/com.idega.content.bundle/resources/images/right.gif" onclick="scroll(this.id)" id="rightScroller" forceId="true" title="#{localizedStrings['com.idega.content']['scroll_right']}"></x:graphicImage>
						</x:div>
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
						window.addEvent('domready', function() {
							setIsSiteMap(false);
						});
					</script>
				</jsf:verbatim>
				<jsf:verbatim>
					<script type="text/javascript">
						window.addEvent('domready', function() {
							appendIdOfTree('new_page_tree');
							appendIdOfAdvancedTree('current_structure_tree');
						});
					</script>
				</jsf:verbatim>
                
                <x:div styleClass="pageInfoButtonsContainer">
					<x:commandButton id="newPageButton" forceId="true" styleClass="newPageButtonStyleClass" type="button" value="#{localizedStrings['com.idega.content']['new_page']}"></x:commandButton>
				</x:div>
				<x:div styleClass="rightButtonStyle">
					<x:commandButton id="showPageModules" forceId="true" styleClass="showPageModulesStyleClass" type="button" value="#{localizedStrings['com.idega.content']['show_modules']}"></x:commandButton>
					<x:commandButton id="showThemesButton" forceId="true" styleClass="showThemesButtonStyleClass" type="button" value="#{localizedStrings['com.idega.content']['show_themes']}"></x:commandButton>
				</x:div>
			</h:form>
		</ws:page>
	</jsf:view>
</jsp:root>