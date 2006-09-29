<?xml version="1.0"?>
<jsp:root xmlns:jsp="http://java.sun.com/JSP/Page"
        xmlns:h="http://java.sun.com/jsf/html"
        xmlns:f="http://java.sun.com/jsf/core"
        xmlns:wf="http://xmlns.idega.com/com.idega.webface"	
        xmlns:ws="http://xmlns.idega.com/com.idega.workspace"
        xmlns:b="http://xmlns.idega.com/com.idega.builder"
        xmlns:x="http://myfaces.apache.org/tomahawk"
               xmlns:builder="http://xmlns.idega.com/com.idega.builder"
        xmlns:tr="http://xmlns.idega.com/com.idega.content"
version="1.2">	
<jsp:directive.page contentType="text/html;charset=UTF-8" pageEncoding="UTF-8"/>
        	
    <f:view>
        <ws:page id="createpage" javascripturls="/idegaweb/bundles/com.idega.content.bundle/resources/javascript/ajax.js,/idegaweb/bundles/com.idega.content.bundle/resources/javascript/drag-drop-folder-tree.js,/idegaweb/bundles/com.idega.builder.bundle/scripts/BuilderService.js,/idegaweb/bundles/com.idega.builder.bundle/scripts/DWREngine.js,/idegaweb/bundles/com.idega.content.bundle/scripts/NodeID.js,/idegaweb/bundles/com.idega.content.bundle/scripts/DWREngine.js">
                <h:form id="createpageform">

                <f:verbatim>
                	<script type="text/javascript">

	//--------------------------------

	// Save functions

	//--------------------------------

	var ajaxObjects = new Array();

	 

	// Use something like this if you want to save data by Ajax.

	function saveComplete(index)

	{

//		alert(ajaxObjects[index].response);			

	}

	// Call this function if you want to save it by a form.
	
	function saveMyTree(newParentNodeId, sourceNodeId) {
		BuilderService.moveTreeNodes(newParentNodeId, sourceNodeId, empty);
	}

	function empty(result) {}
	
	function saveMyTree_byForm() {
		document.myForm.elements['saveString'].value = treeObj.getNodeOrders();
		document.myForm.submit();		
	}

	</script>

		<a href="#" onclick="treeObj.collapseAll()">Collapse all</a> | 
		<a href="#" onclick="treeObj.expandAll()">Expand all</a>


	</f:verbatim>
	                <!-- 		<wf:container id="page_tree_div" styleClass="page_tree">   
	                <wf:container id="page_tree_div">          page_chooser      		-->
	                <wf:container id="page_tree_div">
	                
 
			 			<x:tree2 value="#{pageCreationBean.pageSelectorTopNode}" id="page_chooser" var="node" varNodeToggler="t" clientSideToggle="true">
 							<f:facet name="PageTreeNode"> 							
			                  <h:panelGroup>	
			                     <!--  <h:commandButton image="#{pageCreationBean.coreBundle.resourcesPath}/treeviewer/ui/iw/treeviewer_node_leaf.gif"/>  -->
							   <h:commandLink onclick="document.forms['createpageform'].elements['createpageform:selectedPageLocationIdentifier'].value='#{node.identifier}';document.forms['createpageform'].elements['createpageform:selectedPageLocationName'].value='#{node.description}';document.forms['createpageform'].elements['createpageform:pageLocation'].value='#{node.description}'">
			             		<h:outputText value="#{node.description}" styleClass="nodeFolder"/>   
							   </h:commandLink>
			                  </h:panelGroup>
			              	</f:facet>
			              </x:tree2>    
	 	                 
						</wf:container>
						<f:verbatim>
								<script type="text/javascript">	
								treeObj = new JSDragDropTree();
								treeObj.setTreeId('page_tree_div');

	treeObj.setMaximumDepth(7);

	treeObj.setMessageMaximumDepthReached('Maximum depth reached'); // If you want to show a message when maximum depth is reached, i.e. on drop.
	treeObj.initTree(); 
	treeObj.getNodeOrders();
//alert(treeObj.getNodeOrders());
	</script>
						</f:verbatim>
					<!-- 	
						<wf:container id="page_tree_div" styleClass="page_tree">                		
						<x:tree2 value="#{pageCreationBean.pageSelectorTopNode}" id="page_chooser" var="node" varNodeToggler="t" clientSideToggle="false">
 							<f:facet name="PageTreeNode"> 							
			                  <h:panelGroup>			                  
			                    <h:commandLink immediate="true" action="#{t.toggleExpanded}">	
			                      <h:graphicImage value="#{pageCreationBean.coreBundle.resourcesPath}/treeviewer/ui/iw/treeviewer_node_leaf.gif"/>
			                    </h:commandLink>
							   <h:commandLink  onclick="document.forms['createpageform'].elements['createpageform:selectedPageLocationIdentifier'].value='#{node.identifier}';document.forms['createpageform'].elements['createpageform:selectedPageLocationName'].value='#{node.description}';document.forms['createpageform'].elements['createpageform:pageLocation'].value='#{node.description}'">
			                    	 <h:outputText value="#{node.description}" styleClass="nodeFolder"/>
							   </h:commandLink>
			                  </h:panelGroup>
			              	</f:facet>
			              </x:tree2>
						</wf:container> -->
                </h:form>
    
						 </ws:page>

	</f:view>
</jsp:root>