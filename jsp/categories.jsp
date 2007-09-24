<?xml version="1.0" encoding="UTF-8"?>
<jsp:root version="1.2" 
	xmlns:f="http://java.sun.com/jsf/core" 	
	xmlns:h="http://java.sun.com/jsf/html" 	
	xmlns:jsp="http://java.sun.com/JSP/Page" 	
	xmlns:wf="http://xmlns.idega.com/com.idega.webface"
	xmlns:ws="http://xmlns.idega.com/com.idega.workspace"
	xmlns:co="http://xmlns.idega.com/com.idega.content">
	
    <jsp:directive.page contentType="text/html;charset=UTF-8" pageEncoding="UTF-8"/>
    
    <f:view>
    	<ws:page id="categories1"	javascripturls="/idegaweb/bundles/com.idega.block.web2.0.bundle/resources/javascript/mootools/1.11/mootools-all-compressed.js,
    												/idegaweb/bundles/com.idega.block.web2.0.bundle/resources/javascript/moodalbox/1.2.1/js/moodalbox.js,
    												
    												/dwr/interface/CategoriesEngine.js,
    												/dwr/engine.js,
    												
    												/idegaweb/bundles/com.idega.content.bundle/resources/javascript/CategoriesHelper.js"
    								
    								stylesheeturls="/idegaweb/bundles/com.idega.content.bundle/resources/style/categories.css,
    												/idegaweb/bundles/com.idega.block.web2.0.bundle/resources/javascript/moodalbox/1.2.1/css/moodalbox.css">
    												
			<h:form id="categoriesForm" enctype="multipart/form-data" onsubmit="return false">
				<f:verbatim>
        			<script type="text/javascript">
        				window.addEvent('domready', function() {
        					getInfoForCategories();
        					initializeContentCategoriesActions();
        				});
        			</script>
				</f:verbatim>           
				
				<co:CategoriesEditor id="categoriesEditor" />
			</h:form>
		</ws:page>
	</f:view>
</jsp:root>

