<?xml version="1.0"?>
<jsp:root xmlns:jsp="http://java.sun.com/JSP/Page"
	xmlns:h="http://java.sun.com/jsf/html"
	xmlns:f="http://java.sun.com/jsf/core"
	xmlns:wf="http://xmlns.idega.com/com.idega.webface"
	xmlns:ws="http://xmlns.idega.com/com.idega.workspace"
	xmlns:c="http://xmlns.idega.com/com.idega.content"
	xmlns:x="http://myfaces.apache.org/tomahawk" version="1.2">

	<jsp:directive.page contentType="text/html;charset=UTF-8" pageEncoding="UTF-8" />

	<f:view>
		<ws:page id="createpage" javascripturls="/dwr/engine.js,
        			/dwr/interface/ThemesEngine.js,
					/dwr/interface/BuilderService.js,
					/dwr/interface/PagePreview.js,
					/idegaweb/bundles/com.idega.content.bundle/resources/javascript/drag-drop-folder-tree.js,
        			/idegaweb/bundles/com.idega.block.web2.0.bundle/resources/javascript/scriptaculous-js-1.6.2/lib/prototype.js,
        			/idegaweb/bundles/com.idega.block.web2.0.bundle/resources/javascript/scriptaculous-js-1.6.2/src/scriptaculous.js,
					/idegaweb/bundles/com.idega.content.bundle/resources/javascript/tree.js,
					/idegaweb/bundles/com.idega.content.bundle/resources/javascript/ThemesHelper.js,
        			/idegaweb/bundles/com.idega.content.bundle/resources/javascript/ThemesManagerHelper.js,
        			/idegaweb/bundles/com.idega.content.bundle/resources/javascript/SiteInfoHelper.js">
			<h:form id="createpageform">			
				<wf:wfblock maximizedVertically="true" id="siteMapBlock" title="#{localizedStrings['com.idega.content']['site_map']}">
					
					<x:div id="site_tree_container" forceId="true">
	                	<wf:wfblock id="page_tree_div" styleClass="site_tree_container_site" title="#{localizedStrings['com.idega.content']['current_site_structure']}">
							<h:outputLink value="#" onclick="if (treeObj != null) {treeObj.collapseAll()}"><h:outputText value="#{localizedStrings['com.idega.content']['collapse_all']}"/><h:outputText value=" "/></h:outputLink>
							<h:outputLink value="#" onclick="if (treeObj != null) {treeObj.expandAll()}"><h:outputText value="#{localizedStrings['com.idega.content']['expand_all']}"/></h:outputLink>								
	 						<x:div>
	 							<x:graphicImage id="trash" title="#{localizedStrings['com.idega.content']['drag_to_delete']}" forceId="true" value="/idegaweb/bundles/com.idega.content.bundle/resources/images/user-trash2.png" styleClass="recycleBin" onmouseover="treeObj.prepareToDelete();" onmouseout="treeObj.prepareToDelete();"/> 
							</x:div>
							<wf:iwtree value="#{pageCreationBean.pageSelectorTopNode}" id="current_structure_tree" var="node" varNodeToggler="t" clientSideToggle="true"	showRootNode="false">
								<f:facet name="PageTreeNode">
									<h:outputLink onclick="setPageID(this.parentNode.id);getPrewUrl(this.parentNode.id);getPageInfoValues();return false;">
										<h:outputText value="#{node.description}" title="#{node.description}" />
									</h:outputLink>
								</f:facet>
							</wf:iwtree>				
						</wf:wfblock>
					</x:div>
					
					<x:div id="pagesTypesContainer" forceId="true">
						<wf:wfblock id="dhtmlgoodies_tree2" title="#{localizedStrings['com.idega.content']['page_types']}" styleClass="pagesTypesContainer">
							<wf:iwtree value="#{siteTemplateBean.pageTree}" id="page_chooser22"	var="node" varNodeToggler="t" clientSideToggle="true"	sourceTree="true"	showRootNode="false">
								<f:facet name="IWTreeNode">
									<h:panelGroup>
										<h:outputLink>
											<h:outputText value="#{node.description}" title="#{node.description}" />									
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
							treeObj = new JSDragDropTree();
							treeObj.setTreeId('page_tree_div');
							treeObj.setMaximumDepth(7);
							treeObj.setMessageMaximumDepthReached('Maximum depth reached'); // If you want to show a message when maximum depth is reached, i.e. on drop.
							treeObj.initTree();
							treeObj.getNodeOrders();
							treeObj.expandAll();								
	
							treeObj22 = new JSDragDropTree();
							treeObj22.setTreeId('page_chooser22');
							treeObj22.initTree(); 
							treeObj22.expandAll();
						</script>
					</f:verbatim>
 				
					<x:div id="siteInfoContainer" forceId="true">
						<c:SiteInfo id="siteInfo" styleClass="siteInfoStyle"></c:SiteInfo>
					</x:div>

					
				</wf:wfblock>
				<f:verbatim><script type="text/javascript">setActiveLanguage();</script></f:verbatim>
				<f:verbatim>
					<script type="text/javascript">
						resizeContainer("site_tree_container", "site_tree_container_site", 482, true);
						resizeContainer("pagesTypesContainer", "pagesTypesContainer", 502, false);
						resizeContainer("siteTemplatesContainer", "siteTemplatesContainer", 502, false);
						resizeContainer("siteTemplatesContainer", "siteTemplatesContainer", 287, true);
						checkIfNotEmptySiteTree("div_id_current_structure_tree");
					</script>
				</f:verbatim>
			</h:form>
		</ws:page>
	</f:view>
</jsp:root>