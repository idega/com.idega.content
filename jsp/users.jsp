<?xml version="1.0" encoding="UTF-8"?>
<jsp:root version="1.2" 
	xmlns:f="http://java.sun.com/jsf/core" 	
	xmlns:h="http://java.sun.com/jsf/html" 	
	xmlns:jsp="http://java.sun.com/JSP/Page" 	
	xmlns:wf="http://xmlns.idega.com/com.idega.webface"
	xmlns:ws="http://xmlns.idega.com/com.idega.workspace"
	xmlns:bu="http://xmlns.idega.com/com.idega.builder">
	
    <jsp:directive.page contentType="text/html;charset=UTF-8" pageEncoding="UTF-8"/>

    <f:view>
    	<ws:page id="simpleUserApplication" stylesheeturls="/idegaweb/bundles/com.idega.content.bundle/resources/style/content.css">							
			<h:form id="simpleUserApplicationForm">
				<wf:wfblock id="simpleUserApplicationBlock" title="#{localizedStrings['com.idega.content']['users']}">
					<bu:module id="simpleUserApplicationModule" componentClass="com.idega.user.app.SimpleUserApp">
						<bu:property name=":method:1:implied:void:setAllFieldsEditable:boolean:" value="Y" type="java.lang.String" />
						<bu:property name=":method:1:implied:void:setAddGroupCreateButton:boolean:" value="Y" type="java.lang.String" />
						<bu:property name=":method:1:implied:void:setAddGroupEditButton:boolean:" value="Y" type="java.lang.String" />
					</bu:module>
				</wf:wfblock>
			</h:form>
		</ws:page>
	</f:view>
</jsp:root>