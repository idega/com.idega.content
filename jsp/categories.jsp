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
      <ws:page id="categories1">
        <h:form id="categoriesForm" enctype="multipart/form-data">
          <h:dataTable value="#{CategoryBean.categories}" var="cat">
            <h:column>
              <f:facet name="header">
                <f:verbatim>Id</f:verbatim>
              </f:facet>
              <h:outputText value="#{cat.id}"/>
            </h:column>
            <h:column>
              <f:facet name="header">
                <f:verbatim>English</f:verbatim>
              </f:facet>
              <h:inputText value="#{cat.names['en']}"/>
            </h:column>
            <h:column>
              <f:facet name="header">
                <f:verbatim>Lithuanian</f:verbatim>
              </f:facet>
              <h:inputText value="#{cat.names['lt']}"/>
            </h:column>
            <h:column>
              <f:facet name="header">
                <f:verbatim>Disabled</f:verbatim>
              </f:facet>
              <h:selectBooleanCheckbox value="#{cat.disabled}"/>
            </h:column>
          </h:dataTable>
          <!-- 
          <co:CategoriesEditor id="categoriesEditor"/>
          -->
        </h:form>
      </ws:page>
    </f:view>
</jsp:root>

