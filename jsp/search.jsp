<?xml version="1.0"?>
<jsp:root xmlns:jsp="http://java.sun.com/JSP/Page"
        xmlns:h="http://java.sun.com/jsf/html"
        xmlns:jsf="http://java.sun.com/jsf/core"
        xmlns:ws="http://xmlns.idega.com/com.idega.workspace"
        xmlns:article="http://xmlns.idega.com/com.idega.block.article" 
        xmlns:builder="http://xmlns.idega.com/com.idega.builder"
        xmlns:wf="http://xmlns.idega.com/com.idega.webface"
version="1.2">
<jsp:directive.page contentType="text/html;charset=UTF-8" pageEncoding="UTF-8"/>
<jsf:view>
        <ws:page id="contentsearch1" stylesheeturls="/idegaweb/bundles/com.idega.content.bundle/resources/style/content.css">
                <h:form id="contentsearchform1">
                <wf:wfblock id="contentsearchblock1" title="#{localizedStrings['com.idega.content']['search']}">
                		<builder:module id="contentsearcher1" componentClass="com.idega.core.search.presentation.Searcher" />
                		<builder:module id="contentsearchresults1" componentClass="com.idega.core.search.presentation.SearchResults" />
                </wf:wfblock>
                </h:form>
        </ws:page>
</jsf:view>
</jsp:root>