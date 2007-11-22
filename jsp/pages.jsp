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
						
						/idegaweb/bundles/com.idega.block.web2.0.bundle/resources/javascript/mootools/1.11/mootools-all-compressed.js,
        				/idegaweb/bundles/com.idega.block.web2.0.bundle/resources/javascript/reflection/for_mootools/1.2/reflection.js,
        				
        				/idegaweb/bundles/com.idega.content.bundle/resources/javascript/drag-drop-folder-tree.js,
        				/idegaweb/bundles/com.idega.content.bundle/resources/javascript/ThemesHelper.js,
        				/idegaweb/bundles/com.idega.content.bundle/resources/javascript/ThemesManagerHelper.js,
        				/idegaweb/bundles/com.idega.content.bundle/resources/javascript/PageInfoHelper.js,
						/idegaweb/bundles/com.idega.content.bundle/resources/javascript/tree.js,
						/idegaweb/bundles/com.idega.content.bundle/resources/javascript/SiteManagerHelper.js">
			<h:form id="pagesForm" onsubmit="return false;">
				<f:verbatim>
                	<script type="text/javascript">
                		window.addEvent('domready', function() {
							var errorHanlder = function() {
								reloadPage();
							}
							DWREngine.setErrorHandler(errorHanlder);
						});
						window.addEvent('domready', enableReverseAjaxInThemes);
                		window.addEvent('domready', startBuilderApplication);
                		window.addEvent('domready', getLocalizedTextForThemes);
                		window.addEvent('domready', initializePages);
                		window.addEvent('domready', getPathToImageFolder);
                		window.addEvent('domready', registerPageInfoActions);
                		window.addEvent('domready', initialiazeSiteManager);
                		window.addEvent('domready', getPathToImageFolder);
 	              		window.addEvent('domready', registerSiteActions);
 	              		window.addEvent('domready', createAccordionForLucid);
 	              		window.addEvent('domready', createAccordionForTemplates);
 	              		window.addEvent('resize', controlLucidAppWindow);
                	</script>
                </f:verbatim>
                
                <t:div styleClass="mainPagesContentStyle">
                	<t:div id="leftSide" forceId="true" styleClass="accordionInPages">
						<t:div id="accordion" forceId="true">
							<t:htmlTag styleClass="toggler atStart firstToggler" value="span">
								<h:outputText styleClass="title" value="#{localizedStrings['com.idega.content']['current_site_structure']}"/>
							</t:htmlTag>
							<t:div id="sitemap" forceId="true" styleClass="element atStart">
		 						<t:div id="site_tree_container" forceId="true">
		 							<c:block_with_toolbar id="page_tree_div" addStartPageButton="true" styleClass="site_tree_container_site_accordion" title="#{localizedStrings['com.idega.content']['current_site_structure']}" collapseAllValue="#{localizedStrings['com.idega.content']['collapse_all']}" expandAllValue="#{localizedStrings['com.idega.content']['expand_all']}" trashCanImage="/idegaweb/bundles/com.idega.content.bundle/resources/images/user-trash.png">
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
							
							<t:htmlTag styleClass="toggler atStart" value="span">
								<h:outputText styleClass="title" value="#{localizedStrings['com.idega.content']['page_info']}"/>
							</t:htmlTag>
							<t:div id="pageInfoToggle" forceId="true" styleClass="element atStart">
								<c:PageInfo id="customizePage" styleClass="pageInfoStyle_accordion"></c:PageInfo>
							</t:div>
					
							<t:htmlTag styleClass="toggler atStart" value="span">
								<h:outputText styleClass="title" value="#{localizedStrings['com.idega.content']['site_info']}"/>
							</t:htmlTag>
							<t:div id="siteInfoToggle" forceId="true" styleClass="element atStart">
								<c:SiteInfo id="siteInfo" styleClass="siteInfoStyle_accordion"></c:SiteInfo>
							</t:div>
						</t:div>
					</t:div>
	
					<t:div id="pagePreviewContainer" forceId="true" styleClass="pagePreviewContainer">
						<f:verbatim>
							<iframe name="treePages" id="treePages">iframe</iframe>
						</f:verbatim>
					</t:div>
                
                	<t:inputHidden id="defaultThemeLabel" forceId="true" value="#{localizedStrings['com.idega.content']['theme_is_default']}"></t:inputHidden>
					<t:inputHidden id="notDefaultThemeLabel" forceId="true" value="#{localizedStrings['com.idega.content']['theme_is_not_default']}"></t:inputHidden>
					
					<c:ThemesSliderViewer hiddenOnLoad="true" />
				</t:div>
				
				<t:div id="newPageContainer" forceId="true" styleClass="newPageContainerStyle" style="display: none;">
					<t:div id="pagesAccordion" forceId="true" styleClass="pagesAccordionStyle">
						<t:htmlTag styleClass="accordionHeading" value="span">
							<t:graphicImage id="closeNewPageContainer" forceId="true" url="/idegaweb/bundles/com.idega.content.bundle/resources/images/close_16.png" title="#{localizedStrings['com.idega.content']['close']}" styleClass="closeNewPageOrPagesStyle" />
							<t:outputText value="#{localizedStrings['com.idega.content']['page_types']}" styleClass="title"> </t:outputText>
						</t:htmlTag>
						<t:div id="pageTemplatesStretcher" forceId="true" styleClass="element templatesContainer">
							<wf:iwtree value="#{siteTemplateBean.pageTree}" id="new_page_tree" var="node" varNodeToggler="t" clientSideToggle="true"	sourceTree="true"	showRootNode="false">
								<f:facet name="IWTreeNode">
									<h:panelGroup>
										<h:outputLink>
											<h:outputText value="#{node.description}"/>									
										</h:outputLink>
									</h:panelGroup>
								</f:facet>
							</wf:iwtree>
						</t:div>
					</t:div>
				</t:div>
				
				<t:div id="newPagesContainer" forceId="true" styleClass="newPagesContainerStyle" style="display: none;">
					<t:div id="templatesAccordion" forceId="true" styleClass="templatesAccordionStyle">
						<t:htmlTag styleClass="accordionHeading" value="span">
							<t:graphicImage id="closeNewPagesContainer" forceId="true" url="/idegaweb/bundles/com.idega.content.bundle/resources/images/close_16.png" title="#{localizedStrings['com.idega.content']['close']}" styleClass="closeNewPageOrPagesStyle" />
							<t:outputText value="#{localizedStrings['com.idega.content']['site_templates']}" styleClass="title"></t:outputText>
						</t:htmlTag>
						<c:siteTemplatesViewer id="siteTemplates"/>
					</t:div>
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
                	<t:div style="float: left; width: 255px; visibility: hidden;">
                		<t:commandLink value="none" />
                	</t:div>
                	<t:div styleClass="leftButtonStyle">
                		<t:commandLink id="newPageButton" forceId="true" styleClass="newPageButtonStyleClass" value="#{localizedStrings['com.idega.content']['new_page']}"></t:commandLink>
                		<t:commandLink id="newPagesButton" forceId="true" styleClass="newPagesButtonStyleClass" value="#{localizedStrings['com.idega.content']['new_pages']}"></t:commandLink>
                	</t:div>
					<t:div styleClass="rightButtonStyle">
						<t:commandLink id="showPageModules" forceId="true" styleClass="showPageModulesStyleClass" value="#{localizedStrings['com.idega.content']['show_modules']}"></t:commandLink>
						<t:commandLink id="showThemesButton" forceId="true" styleClass="showThemesButtonStyleClass" value="#{localizedStrings['com.idega.content']['show_themes']}"></t:commandLink>
						<t:commandLink id="showEditPagesButton" forceId="true" styleClass="showEditPagesButtonStyleClass" value="#{localizedStrings['com.idega.content']['edit']}"></t:commandLink>
						<t:commandLink id="showSourcePagesButton" forceId="true" styleClass="showSourcePagesButtonStyleClass" value="#{localizedStrings['com.idega.content']['page_source']}"></t:commandLink>
						<t:commandLink id="showPreviewPagesButton" forceId="true" styleClass="showPreviewPagesButtonStyleClass" value="#{localizedStrings['com.idega.content']['preview']}"></t:commandLink>
					</t:div>
				</t:div>
			</h:form>
		</ws:page>
	</f:view>
</jsp:root>