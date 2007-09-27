<?xml version="1.0"?>
<jsp:root xmlns:jsp="http://java.sun.com/JSP/Page"
        xmlns:h="http://java.sun.com/jsf/html"
        xmlns:f="http://java.sun.com/jsf/core"
        xmlns:t="http://myfaces.apache.org/tomahawk"
        xmlns:wf="http://xmlns.idega.com/com.idega.webface"
        xmlns:ws="http://xmlns.idega.com/com.idega.workspace"
 		xmlns:c="http://xmlns.idega.com/com.idega.content"
        xmlns:web2="http://xmlns.idega.com/com.idega.block.web2.0"
version="1.2">
	<jsp:directive.page contentType="text/html;charset=UTF-8" pageEncoding="UTF-8"/>
	<f:view>
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
				<f:verbatim>
                	<script type="text/javascript">
                		window.addEvent('domready', function() {
							var errorHanlder = function() {
								closeAllLoadingMessages();
								window.location.href = window.location.href;
							}
							DWREngine.setErrorHandler(errorHanlder);
						});
                		window.addEvent('domready', startBuilderApplication);
                		window.addEvent('domready', getLocalizedTextForThemes);
                		window.addEvent('domready', initializePages);
                		window.addEvent('domready', getPathToImageFolder);
                		window.addEvent('domready', registerPageInfoActions);
                	</script>
                </f:verbatim>
                
                <wf:wfblock maximizedVertically="true" id="pagesBlock" title="#{localizedStrings['com.idega.content']['pages']}">
                	<t:div id="leftSide" forceId="true" styleClass="accordionInPages">
						<web2:accordion accordionId="myAccordion" includeJavascript="true">
							<f:facet name="PANELS">
								<t:div id="accordion" forceId="true">
									<t:div id="siteMapInformation" forceId="true" styleClass="acTogglemyAccordion">
										<h:outputText value="#{localizedStrings['com.idega.content']['current_site_structure']}"/>
									</t:div>
									
									<t:div id="sitemap" forceId="true" styleClass="acStretchmyAccordion">
	 									<t:div id="site_tree_container" forceId="true">
	 										<c:block_with_toolbar id="page_tree_div" styleClass="site_tree_container_site_accordion" title="#{localizedStrings['com.idega.content']['current_site_structure']}" collapseAllValue="#{localizedStrings['com.idega.content']['collapse_all']}" expandAllValue="#{localizedStrings['com.idega.content']['expand_all']}" trashCanImage="/idegaweb/bundles/com.idega.content.bundle/resources/images/user-trash.png" addStartPageButton="true">
												<wf:iwtree value="#{pageCreationBean.pageSelectorTopNode}" id="current_structure_tree" var="node" varNodeToggler="t" clientSideToggle="true"	showRootNode="false">
													<f:facet name="PageTreeNode">
														<h:outputLink styleClass="pageTreeNames">
															<h:outputText value="#{node.description}"/>
														</h:outputLink>
													</f:facet>
												</wf:iwtree>
											</c:block_with_toolbar>
										</t:div>
									</t:div>
																					
									<t:div id="pageInfo" forceId="true" styleClass="acTogglemyAccordion">
										<h:outputText value="#{localizedStrings['com.idega.content']['page_info']}"/>
									</t:div>
									
									<t:div id="pageInfoToggle" forceId="true" styleClass="acStretchmyAccordion">
										<c:PageInfo id="customizePage" styleClass="pageInfoStyle_accordion"></c:PageInfo>
									</t:div>
								</t:div>
							</f:facet>
						</web2:accordion>     
					</t:div>           
	
					<t:div styleClass="pagePreviewContainer">
						<f:verbatim>
							<iframe name="treePages" id="treePages" class="pagePreviewFrame">iframe</iframe>
						</f:verbatim>
					</t:div>
                
                	<t:inputHidden id="defaultThemeLabel" forceId="true" value="#{localizedStrings['com.idega.content']['theme_is_default']}"></t:inputHidden>
					<t:inputHidden id="notDefaultThemeLabel" forceId="true" value="#{localizedStrings['com.idega.content']['theme_is_not_default']}"></t:inputHidden>
					
					<c:ThemesSliderViewer hiddenOnLoad="true" />
                </wf:wfblock>
                
                <t:div id="newPageContainer" forceId="true" styleClass="newPageContainerStyle" style="display: none; left: 8px;">
					<wf:iwtree value="#{siteTemplateBean.pageTree}" id="new_page_tree" var="node" varNodeToggler="t" clientSideToggle="true"	sourceTree="true"	showRootNode="false">
						<f:facet name="IWTreeNode">						
							<h:panelGroup>
								<h:outputLink>
									<h:outputText value="#{node.description}" />									
								</h:outputLink>
							</h:panelGroup>
						</f:facet>
					</wf:iwtree>
				</t:div>
				
				<f:verbatim>
					<script type="text/javascript">
						window.addEvent('domready', function() {
							setIsSiteMap(false);
						});
					</script>
				</f:verbatim>
				<f:verbatim>
					<script type="text/javascript">
						window.addEvent('domready', function() {
							appendIdOfTree('new_page_tree');
							appendIdOfAdvancedTree('current_structure_tree');
						});
					</script>
				</f:verbatim>
                
                <t:div styleClass="pageInfoButtonsContainer">
					<t:commandButton id="newPageButton" forceId="true" styleClass="newPageButtonStyleClass" type="button" value="#{localizedStrings['com.idega.content']['new_page']}"></t:commandButton>
				</t:div>
				<t:div styleClass="rightButtonStyle">
					<t:commandButton id="showPageModules" forceId="true" styleClass="showPageModulesStyleClass" type="button" value="#{localizedStrings['com.idega.content']['show_modules']}"></t:commandButton>
					<t:commandButton id="showThemesButton" forceId="true" styleClass="showThemesButtonStyleClass" type="button" value="#{localizedStrings['com.idega.content']['show_themes']}"></t:commandButton>
					<t:commandButton id="showEditPagesButton" forceId="true" styleClass="showEditPagesButtonStyleClass" type="button" value="#{localizedStrings['com.idega.content']['edit']}"></t:commandButton>
					<t:commandButton id="showPreviewPagesButton" forceId="true" styleClass="showPreviewPagesButton" type="button" value="#{localizedStrings['com.idega.content']['preview']}"></t:commandButton>
				</t:div>
			</h:form>
		</ws:page>
	</f:view>
</jsp:root>