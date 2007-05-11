<?xml version="1.0"?>
<jsp:root xmlns:jsp="http://java.sun.com/JSP/Page"
	xmlns:h="http://java.sun.com/jsf/html"
	xmlns:f="http://java.sun.com/jsf/core"
	xmlns:wf="http://xmlns.idega.com/com.idega.webface"
	xmlns:ws="http://xmlns.idega.com/com.idega.workspace"
	xmlns:c="http://xmlns.idega.com/com.idega.content"
	xmlns:jsf="http://java.sun.com/jsf/core"
	xmlns:web2="http://xmlns.idega.com/com.idega.block.web2.0"
	xmlns:x="http://myfaces.apache.org/tomahawk" version="1.2">

	<jsp:directive.page contentType="text/html;charset=UTF-8" pageEncoding="UTF-8" />

	<f:view>
		<ws:page id="createpage" javascripturls="/dwr/engine.js,
        			/dwr/interface/ThemesEngine.js,
					/dwr/interface/BuilderService.js,
					/dwr/interface/PagePreview.js,
					/idegaweb/bundles/com.idega.block.web2.0.bundle/resources/javascript/behaviour-mootools.js,	

					/idegaweb/bundles/com.idega.content.bundle/resources/javascript/drag-drop-folder-tree.js,
					/idegaweb/bundles/com.idega.content.bundle/resources/javascript/tree.js,
					/idegaweb/bundles/com.idega.content.bundle/resources/javascript/ThemesHelper.js,
        			/idegaweb/bundles/com.idega.content.bundle/resources/javascript/SiteManagerHelper.js">        			
			<h:form id="createpageform">
				<f:verbatim>
                	<script type="text/javascript">
                		getGlobalPageId();
//                		registerEvent(window, "load", getGlobalPageId);                	
                		registerEvent(window, "load", getLocalizedTextForThemes);
                		registerEvent(window, "load", initialiazeSiteManager);
                		registerEvent(window, "load", getPathToImageFolder);
 	              		registerEvent(window, "load", registerSiteActions);                		
                	</script>
                </f:verbatim>
			
				<wf:wfblock maximizedVertically="true" id="siteMapBlock" title="#{localizedStrings['com.idega.content']['site_map']}">
                <x:div id="leftSide" forceId="true" styleClass="accordionInPages">
					<web2:accordion accordionId="myAccordion" includeJavascript="true">
							<f:facet name="PANELS">
								<x:div id="structureAccordion" forceId="true">
										<x:div id="siteMapInformation" forceId="true" styleClass="acTogglemyAccordion"><h:outputText value="Site map info"/></x:div>
										<x:div id="sitemap" forceId="true" styleClass="acStretchmyAccordion">
 										
											<x:div id="site_tree_container" forceId="true">
							                	<c:block_with_toolbar id="page_tree_div" styleClass="site_tree_container_site_accordion" title="#{localizedStrings['com.idega.content']['current_site_structure']}" collapseAllValue="#{localizedStrings['com.idega.content']['collapse_all']}" expandAllValue="#{localizedStrings['com.idega.content']['expand_all']}" trashCanImage="/idegaweb/bundles/com.idega.content.bundle/resources/images/user-trash.png">
													<wf:iwtree value="#{pageCreationBean.pageSelectorTopNode}" id="current_structure_tree" var="node" varNodeToggler="t" clientSideToggle="true"	showRootNode="false">
														<jsf:facet name="PageTreeNode">
															<h:outputLink onclick="setPageID(this.parentNode.id);return false;">
																<h:outputText value="#{node.description}"/>
															</h:outputLink>
														</jsf:facet>
													</wf:iwtree>
												</c:block_with_toolbar>
											</x:div>										

										</x:div>
																				
									<x:div id="siteinformation" forceId="true" styleClass="acTogglemyAccordion"><h:outputText value="Site info"/></x:div>
									<x:div id="siteInfoToggle" forceId="true" styleClass="acStretchmyAccordion">
						<c:PageInfo id="customizePage" styleClass="pageInfoStyle_accordion"></c:PageInfo>
									</x:div>
								</x:div>
							</f:facet>
					</web2:accordion>     
					</x:div>     
<!--  
					<x:div id="site_tree_container" forceId="true">
	                	<c:block_with_toolbar id="page_tree_div" styleClass="site_tree_container_site" title="#{localizedStrings['com.idega.content']['current_site_structure']}" collapseAllValue="#{localizedStrings['com.idega.content']['collapse_all']}" expandAllValue="#{localizedStrings['com.idega.content']['expand_all']}" trashCanImage="/idegaweb/bundles/com.idega.content.bundle/resources/images/user-trash.png">
							<wf:iwtree value="#{pageCreationBean.pageSelectorTopNode}" id="current_structure_tree" var="node" varNodeToggler="t" clientSideToggle="true"	showRootNode="false">
								<f:facet name="PageTreeNode">

	 								<h:outputLink styleClass="pageTreeNames">
										<h:outputText value="#{node.description}"/>
									</h:outputLink>

								</f:facet>
							</wf:iwtree>
						</c:block_with_toolbar>
					</x:div>
 --> 
					<x:div id="pagesTypesContainer" forceId="true">
						<wf:wfblock id="dhtmlgoodies_tree2" title="#{localizedStrings['com.idega.content']['page_types']}" styleClass="pagesTypesContainer">
							<wf:iwtree value="#{siteTemplateBean.pageTree}" id="page_chooser22"	var="node" varNodeToggler="t" clientSideToggle="true"	sourceTree="true"	showRootNode="false">
								<f:facet name="IWTreeNode">
									<h:panelGroup>
										<h:outputLink>
											<h:outputText value="#{node.description}"/>									
										</h:outputLink>
									</h:panelGroup>
								</f:facet>
							</wf:iwtree>
						</wf:wfblock>
					</x:div>
					
					<x:div id="siteTemplatesContainer" forceId="true">
						<wf:wfblock id="siteTemplatesBlock" title="#{localizedStrings['com.idega.content']['site_templates']}" styleClass="siteTemplatesContainer">
							<c:siteTemplatesViewer id="siteTemplates"/>
						</wf:wfblock>
					</x:div>

					<f:verbatim>
						<script type="text/javascript">	
							appendIdOfTree('page_chooser22');
							appendIdOfAdvancedTree('current_structure_tree');							
						</script>
					</f:verbatim>
<!-- 
					<x:div id="siteInfoContainer" forceId="true">
						<c:SiteInfo id="siteInfo" styleClass="siteInfoStyle"></c:SiteInfo>
					</x:div>
 --> 				
				</wf:wfblock>
			</h:form>
		</ws:page>
	</f:view>
</jsp:root>