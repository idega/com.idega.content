<?xml version="1.0" encoding="UTF-8"?>
<jsp:root version="1.2" xmlns:f="http://java.sun.com/jsf/core" xmlns:h="http://java.sun.com/jsf/html" xmlns:jsp="http://java.sun.com/JSP/Page" xmlns:owt="http://www.otrix.com/faces/webtree">
    <jsp:directive.page contentType="text/html;charset=UTF-8" pageEncoding="UTF-8"/>
    <f:view>
        <html lang="is-IS" xml:lang="is-IS">
            <head>
                <meta content="no-cache" http-equiv="Cache-Control"/>
                <meta content="no-cache" http-equiv="Pragma"/>
                <title>listDocuments2 Title</title>
                <link href="../resources/stylesheet.css" rel="stylesheet" type="text/css"/>
            </head>
            <body style="-rave-layout: grid">
                <h:form binding="#{listDocuments2.form1}" id="form1">
                    <div style="float: left; padding:10; ">
                        <!--
                        <owt:webTree height="400" id="tree1">
                            <owt:treeNode enabled="true" expanded="false" href="" id="tn1" label="Web site graphics"/>
                            <owt:treeNode enabled="true" expanded="false" href="" id="tn2" label="Financial">
                            <owt:treeNode enabled="true" expanded="false" href="" id="tn3" label="2004">
                            <owt:treeNode enabled="true" expanded="false" href="" id="tn4" label="2004 Budget"/>
                            </owt:treeNode>
                            <owt:treeNode enabled="true" expanded="false" href="" id="tn5" label="2003"/>
                            </owt:treeNode>
                            <owt:treeNode enabled="true" expanded="false" href="" id="tn6" label="Information">
                            <owt:treeNode enabled="true" expanded="false" href="" id="tn7" label="How to ..."/>
                            </owt:treeNode>
                        </owt:webTree>
                    -->
                        <!--<owt:webTree binding="#{WebDavTree.tree}" height="200" id="tree2" width="200"/>-->
                        <div>
                            <owt:webTree binding="#{WebDavTree.tree}" height="200" id="tree2" width="200"/>
                        
<!--                            <jsp:directive.include file="folderTree.jsp"/> -->
                        </div>
                    </div>
                    <div style="float: left; clear:right; padding:10; ">
                        <h:dataTable binding="#{listDocuments2.dataTable1}" headerClass="list-paging-header" id="dataTable1"
                            rowClasses="list-row-even,list-row-odd" rows="10" value="#{listDocuments2.dataTable1Model}" var="currentRow">
                            <h:column binding="#{listDocuments2.column1}" id="column1">
                                <h:outputText binding="#{listDocuments2.outputText1}" id="outputText1" value="#{currentRow.name}"/>
                                <f:facet name="header">
                                    <h:outputText binding="#{listDocuments2.outputText2}" id="outputText2" value="Name"/>
                                </f:facet>
                            </h:column>
                            <h:column binding="#{listDocuments2.column2}" id="column2">
                                <h:outputText binding="#{listDocuments2.outputText3}" id="outputText3" value="#{currentRow.date}"/>
                                <f:facet name="header">
                                    <h:outputText binding="#{listDocuments2.outputText4}" id="outputText4" value="Date"/>
                                </f:facet>
                            </h:column>
                            <h:column binding="#{listDocuments2.column3}" id="column3">
                                <h:outputText binding="#{listDocuments2.outputText5}" id="outputText5" value="#{currentRow.length}"/>
                                <f:facet name="header">
                                    <h:outputText binding="#{listDocuments2.outputText6}" id="outputText6" value="Size"/>
                                </f:facet>
                            </h:column>
                            <h:column binding="#{listDocuments2.column4}" id="column4">
                                <h:outputText binding="#{listDocuments2.outputText7}" id="outputText7" value="#{currentRow.mime}"/>
                                <f:facet name="header">
                                    <h:outputText binding="#{listDocuments2.outputText8}" id="outputText8" value="Mime type"/>
                                </f:facet>
                            </h:column>
                            <h:column binding="#{listDocuments2.column5}" id="column5">
                                <h:outputText binding="#{listDocuments2.outputText9}" id="outputText9" value="#{currentRow.isCollection}"/>
                                <f:facet name="header">
                                    <h:outputText binding="#{listDocuments2.outputText10}" id="outputText10" value="IsDir"/>
                                </f:facet>
                            </h:column>
                            <f:facet name="header"/>
                            <f:facet name="header">
                                <h:panelGrid binding="#{listDocuments2.gridPanel1}" columns="4" id="gridPanel1">
                                    <h:commandButton action="#{listDocuments2.dataTable1_firstPageAction}"
                                        binding="#{listDocuments2.dataTable1HeaderFirstButton}" id="dataTable1HeaderFirstButton" immediate="true" value="|&lt;"/>
                                    <h:commandButton action="#{listDocuments2.dataTable1_previousPageAction}"
                                        binding="#{listDocuments2.dataTable1HeaderPreviousButton}" id="dataTable1HeaderPreviousButton" immediate="true" value="&lt;-"/>
                                    <h:commandButton action="#{listDocuments2.dataTable1_nextPageAction}" binding="#{listDocuments2.dataTable1HeaderNextButton}"
                                        id="dataTable1HeaderNextButton" immediate="true" value="->"/>
                                    <h:commandButton action="#{listDocuments2.dataTable1_lastPageAction}" binding="#{listDocuments2.dataTable1HeaderLastButton}"
                                        id="dataTable1HeaderLastButton" immediate="true" value=">|"/>
                                </h:panelGrid>
                            </f:facet>
                        </h:dataTable>
                        <!--                    <w:filelist columns="name,modifieddate,createdate,size" folder="http://localhost/files/myfolder" />
    -->
                    </div>
                    <iframe src="documentContent.jsp" style="left: 252px; top: 324px; position: absolute"/>
                </h:form>
            </body>
        </html>
    </f:view>
</jsp:root>
