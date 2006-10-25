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
        xmlns:a="http://java.sun.com/jmaki-jsf"
        xmlns:ui="http://www.sun.com/web/ui"
version="1.2">	
	
<jsp:directive.page contentType="text/html;charset=UTF-8" pageEncoding="UTF-8"/>
        	
	<f:view>
    	<ws:page id="createpage" javascripturls="/idegaweb/bundles/com.idega.content.bundle/resources/javascript/ajax.js,
    											 /idegaweb/bundles/com.idega.content.bundle/resources/javascript/drag-drop-folder-tree.js,
    											 /idegaweb/bundles/com.idega.content.bundle/resources/javascript/prototype.js,
    											 /idegaweb/bundles/com.idega.content.bundle/resources/javascript/tree.js,
    											 /idegaweb/bundles/com.idega.content.bundle/resources/javascript/unittest.js,
    											 /idegaweb/bundles/com.idega.content.bundle/resources/javascript/scriptaculous.js,
    											 /dwr/interface/BuilderService.js,
    											 /dwr/interface/PagePreview.js,
    											 /dwr/interface/ThemesPreviewsProvider.js,
    											 /idegaweb/bundles/com.idega.content.bundle/resources/javascript/rico.js,
    											 /dwr/engine.js">
        	<h:form id="createpageform">

                <wf:container id="page_tree_div" styleClass="current_structure">
	                <f:verbatim>
	                	<h3>Current structure</h3>
						<a href="#" onclick="treeObj.collapseAll()">Collapse all</a> | 
						<a href="#" onclick="treeObj.expandAll()">Expand all</a>
					</f:verbatim>
		 			<wf:iwtree value="#{pageCreationBean.pageSelectorTopNode}" id="page_chooser" var="node" varNodeToggler="t" clientSideToggle="true">
						<f:facet name="PageTreeNode"> 							
							<h:panelGroup>
					   			<h:outputLink onclick="getPrewUrl(this.parentNode.id);return false;">
			             	 		<h:outputText value="#{node.description}"/>   
			             		</h:outputLink>
			                </h:panelGroup>
			            </f:facet>
			        </wf:iwtree>    
	 	            <iframe id="treePages" width= "100%" height = "400px" style="left: 500px; top: 150px; position: absolute;" scrolling = "no"/>
				</wf:container>

                <wf:container id="dhtmlgoodies_tree2" styleClass="template_tree">
                <f:verbatim>
                	<div id="dhtmlgoodies_tree2_div">
						<ul id="dhtmlgoodies_tree2" class="dhtmlgoodies_tree2">
							<li id="103" noChildren="true" noRemoving="true" typeOfTemplate="page"><a href="#">Page</a></li>
							<li id="106" noChildren="true" noRemoving="true" typeOfTemplate="blog"><a href="#">Blog</a></li>								
							<li id="107" noChildren="true" noRemoving="true" typeOfTemplate="eShop"><a href="#">eShop</a></li>						
							<li id="108" noChildren="true" noRemoving="true" typeOfTemplate="flashIntro"><a href="#">Flash intro</a></li>						

							<li id="103" noChildren="true" noRemoving="true" typeOfTemplate="forum"><a href="#">Forum</a></li>
							<li id="106" noChildren="true" noRemoving="true" typeOfTemplate="guestbook"><a href="#">Guestbook</a></li>								
							<li id="107" noChildren="true" noRemoving="true" typeOfTemplate="imageGallery"><a href="#">Image gallery</a></li>						
							<li id="108" noChildren="true" noRemoving="true" typeOfTemplate="login"><a href="#">Login</a></li>						
						</ul>
					</div>          
                </f:verbatim>
<!-- jsp accordion -->                        

          <x:div id="accordionDiv" forceId="true">
            <x:div id="overviewPanel" forceId="true">
              <x:div id="overviewHeader" forceId="true">
                <h:outputText value="testing DIV tags"/>                
              </x:div>
              <x:div id="overviewContent" forceId="true"> 
<!--  		 			<wf:iwtree value="#{siteTemplateBean.siteTree}" id="page_chooser" var="node" varNodeToggler="t" clientSideToggle="true">-->
	  		 			<wf:iwtree value="#{pageCreationBean.pageSelectorTopNode}" id="page_chooser" var="node" varNodeToggler="t" clientSideToggle="true">
						<f:facet name="PageTreeNode"> 							
							<h:panelGroup>
					   			<h:outputLink onclick="getPrewUrl(this.parentNode.id);return false;">
			             	 		<h:outputText value="#{node.description}"/>   
			             		</h:outputLink>
			                </h:panelGroup>
			            </f:facet>
			        </wf:iwtree>                             


              </x:div>
            </x:div>
          </x:div>


<!-- html accordion -->                

				</wf:container>
				
				<f:verbatim>				
					<script type="text/javascript">	
					
					new Rico.Accordion( $('accordionDiv'), {panelHeight:300} );
					
						treeObj = new JSDragDropTree();
						treeObj.setTreeId('page_tree_div');
						treeObj.setMaximumDepth(7);
						treeObj.setMessageMaximumDepthReached('Maximum depth reached'); // If you want to show a message when maximum depth is reached, i.e. on drop.
						treeObj.initTree(); 
						treeObj.getNodeOrders();
						treeObj.expandAll();
						
						treeObj2 = new JSDragDropTree();
						treeObj2.setTreeId('dhtmlgoodies_tree2');
						treeObj2.initTree(); 

//						treeObj3 = new JSDragDropTree();
//						treeObj3.setTreeId('dhtmlgoodies_tree3');
//						treeObj3.initTree(); 

						
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