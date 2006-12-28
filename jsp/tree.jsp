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
				<wf:wfblock maximizedVertically="true" id="siteMapBlock">
					
					<x:div styleClass="currentStructureStyle">
						<wf:container id="page_tree_div" styleClass="current_structure">
							<x:div styleClass="siteTreeTopic">
								<h:outputText value="Current Structure"/>
							</x:div>	
							<h:outputLink value="#" onclick="treeObj.collapseAll()"><h:outputText value="Collapse all "/></h:outputLink>
							<h:outputLink value="#" onclick="treeObj.expandAll()"><h:outputText value="Expand all"/></h:outputLink>								
							<x:div styleClass="siteTree">
								<wf:iwtree value="#{pageCreationBean.pageSelectorTopNode}" id="page_chooser" var="node" varNodeToggler="t" clientSideToggle="true"	showRootNode="false">
									<f:facet name="PageTreeNode">
										<h:outputLink onclick="getPrewUrl(this.parentNode.id);return false;">
											<h:outputText value="#{node.description}" />
										</h:outputLink>
									</f:facet>
								</wf:iwtree>				
							</x:div>
 							<x:div>
 								<x:graphicImage id="trash" forceId="true" styleClass="recycleBin2" value="/idegaweb/bundles/com.idega.content.bundle/resources/images/user-trash2.png" onmouseover="treeObj.prepareToDelete();" onmouseout="treeObj.prepareToDelete();"/> 
							</x:div>
						</wf:container>
					</x:div>

					<wf:container id="dhtmlgoodies_tree2" styleClass="template_tree">
						<x:div styleClass="siteTreeTopic">
							<h:outputText value="Page types"/>
						</x:div>
	 
						<wf:iwtree value="#{siteTemplateBean.pageTree}" id="page_chooser22"	var="node" varNodeToggler="t" clientSideToggle="true"	sourceTree="true"	showRootNode="false">
							<f:facet name="IWTreeNode">
								<h:panelGroup>
									<h:outputLink>
										<h:outputText value="#{node.description}" />									
									</h:outputLink>
								</h:panelGroup>
							</f:facet>
						</wf:iwtree>
	
						<c:siteTemplatesViewer id="siteTemplates"/>
					</wf:container>

					<f:verbatim>
						<script type="text/javascript">	
							treeObj = new JSDragDropTree();
							treeObj.setTreeId('page_tree_div');					
							treeObj.initTree(); 
							treeObj.checkIfOverTree('page_tree_div');
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
					<script type="text/javascript">resizeSiteTree("site_tree_container", "site_tree_container_site", 482)</script>
				</f:verbatim>
			</h:form>
		</ws:page>
	</f:view>
</jsp:root>
