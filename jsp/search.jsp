<?xml version="1.0"?>
<jsp:root xmlns:jsp="http://java.sun.com/JSP/Page"
        xmlns:h="http://java.sun.com/jsf/html"
        xmlns:jsf="http://java.sun.com/jsf/core"
        xmlns:ws="http://xmlns.idega.com/com.idega.workspace"
        xmlns:article="http://xmlns.idega.com/com.idega.block.article" 
        xmlns:builder="http://xmlns.idega.com/com.idega.builder"
version="1.2">

<jsf:view>
        <ws:page>
                <h:form>
                		<builder:module componentClass="com.idega.core.search.presentation.Searcher" />
                		<builder:module componentClass="com.idega.core.search.presentation.SearchResults" />
                </h:form>
        </ws:page>
</jsf:view>
</jsp:root>