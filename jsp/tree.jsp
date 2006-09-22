<?xml version="1.0"?>
<jsp:root xmlns:jsp="http://java.sun.com/JSP/Page"
        xmlns:h="http://java.sun.com/jsf/html"
        xmlns:f="http://java.sun.com/jsf/core"
        xmlns:wf="http://xmlns.idega.com/com.idega.webface"	
        xmlns:ws="http://xmlns.idega.com/com.idega.workspace"
        xmlns:b="http://xmlns.idega.com/com.idega.builder"
        xmlns:x="http://myfaces.apache.org/tomahawk"
        
        xmlns:tr="http://xmlns.idega.com/com.idega.content"
version="1.2">	
<jsp:directive.page contentType="text/html;charset=UTF-8" pageEncoding="UTF-8"/>
        	
    <f:view>
        <ws:page id="createpage" javascripturls="/idegaweb/bundles/com.idega.content.bundle/resources/javascript/ajax.js,/idegaweb/bundles/com.idega.content.bundle/resources/javascript/drag-drop-folder-tree.js,/idegaweb/bundles/com.idega.builder.bundle/scripts/BuilderService.js,/idegaweb/bundles/com.idega.builder.bundle/scripts/DWREngine.js">
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

		alert(ajaxObjects[index].response);			

	}

	// Call this function if you want to save it by a form.
	
	function saveMyTree()
	{
//alert('saving tree');
			saveString = treeObj.getNodeOrders();
			
			//call the serverside method via DWR
//ImageProvider.getAllImagePathsForFolder(getCurrentImageGalleryFolderURI(), createAndStartImageGallerySlideShow);

//ThemesPreviewsProvider.compareNodes(saveString, empty);

BuilderService.moveTreeNodes(saveString, empty);

//alert('saveString = ' + saveString);
			var ajaxIndex = ajaxObjects.length;
			ajaxObjects[ajaxIndex] = new sack();
			var url = 'saveNodes.php?saveString=' + saveString;
			ajaxObjects[ajaxIndex].requestFile = url;	// Specifying which file to get
			ajaxObjects[ajaxIndex].onCompletion = function() { saveComplete(ajaxIndex); } ;	// Specify function that will be executed after file has been found
			ajaxObjects[ajaxIndex].runAJAX();		// Execute AJAX function			
	}
	
	function empty(result) {
		alert(result);
	}
	
	function saveMyTree_byForm()

	{
		document.myForm.elements['saveString'].value = treeObj.getNodeOrders();
		document.myForm.submit();		
	}

	</script>
	<!-- 
	<ul id="dhtmlgoodies_tree2" class="dhtmlgoodies_tree">
		<li id="node0" noDrag="true" noSiblings="true"><a href="#">Root node</a>
			<ul>
				<li id="node1"><a href="#">Europe</a>
					<ul>
						<li id="node2"><a href="#">Norway</a>
							<ul>
								<li id="node3"><a href="#">Stavanger</a></li>
								<li id="node6"><a href="#">Bergen</a></li>
								<li id="node7"><a href="#">Oslo</a></li>
							</ul>
						</li>
						<li id="node8"><a href="#">United Kingdom</a>
							<ul>
								<li id="node9"><a href="#">London</a></li>
								<li id="node10"><a href="#">Manchester</a></li>
							</ul>				
						</li>
						<li id="node12"><a href="#">Sweden</a></li>
						<li id="node13"><a href="#">Denmark</a></li>
						<li id="node14"><a href="#">Germany</a>
							<ul>
								<li id="node141"><a href="#">Berlin</a>	</li>
								<li id="node142"><a href="#">Munich</a>	</li>
								<li id="node143"><a href="#">Stuttgart</a>	</li>
							</ul>
						</li>
					</ul>
				</li>
				<li id="node15"><a href="#">Asia</a>
					<ul>
						<li id="node151"><a href="#">Japan</a></li>	
						<li id="node152"><a href="#">China</a></li>	
						<li id="node153"><a href="#">Indonesia</a></li>			
					</ul>
				</li>
				<li id="node16"><a href="#">Africa</a>
					<ul>
						<li id="node17"><a href="#">Tanzania</a></li>
						<li id="node18"><a href="#">Kenya</a></li>
					</ul>
				</li>
				<li id="node19"><a href="#">America</a>
					<ul>
						<li id="node20"><a href="#">Canada</a></li>
						<li id="node21"><a href="#">United States</a></li>
						<li id="node22"><a href="#">Mexico</a></li>
						<li id="node23"><a href="#">Argentina</a></li>
					</ul>		
				</li>	
				<li id="node24" noChildren="true"><a href="#">Cannot have children</a></li>
				<li id="node25" noDrag="true"><a href="#">Cannot be dragged</a></li>
			</ul>
		</li>
	</ul> 
	 -->

		<a href="#" onclick="treeObj.collapseAll()">Collapse all</a> | 
	<a href="#" onclick="treeObj.expandAll()">Expand all</a>
	<p style="margin:10px">Use your mouse to drag and drop the nodes. Use the "Save" button to save your changes. The new structure will be sent to the server by use of Ajax(Asynchron XML and Javascript). </p>
	<p/>
	</f:verbatim>
	                <!-- 		<wf:container id="page_tree_div" styleClass="page_tree">   
	                <wf:container id="page_tree_div">          page_chooser      		-->
	                <wf:container id="page_tree_div">
						<x:tree2 value="#{pageCreationBean.pageSelectorTopNode}" id="page_chooser" var="node" varNodeToggler="t" clientSideToggle="true">
 							<f:facet name="PageTreeNode"> 							
			                  <h:panelGroup>	
			                   <!-- 
			                     <h:commandLink immediate="true" action="#{t.toggleExpanded}">	
			                      <h:graphicImage value="#{pageCreationBean.coreBundle.resourcesPath}/treeviewer/ui/iw/treeviewer_node_leaf.gif"/>-->
			                      <h:commandButton image="#{pageCreationBean.coreBundle.resourcesPath}/treeviewer/ui/iw/treeviewer_node_leaf.gif"/>
			               <!--     </h:commandLink immediate="true" action="#{t.toggleExpanded}" image="#{pageCreationBean.coreBundle.resourcesPath}/treeviewer/ui/iw/treeviewer_node_leaf.gif">
			                    -->
							   <h:commandLink  onclick="document.forms['createpageform'].elements['createpageform:selectedPageLocationIdentifier'].value='#{node.identifier}';document.forms['createpageform'].elements['createpageform:selectedPageLocationName'].value='#{node.description}';document.forms['createpageform'].elements['createpageform:pageLocation'].value='#{node.description}'">
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