<?xml version="1.0" encoding="UTF-8"?>
<jsp:root version="1.2" 
xmlns:f="http://java.sun.com/jsf/core" 
xmlns:h="http://java.sun.com/jsf/html" 
xmlns:jsp="http://java.sun.com/JSP/Page" 
xmlns:owt="http://www.otrix.com/faces/webtree" 
xmlns:wf="http://xmlns.idega.com/com.idega.webface"
xmlns:cmf="http://myfaces.sourceforge.net/tld/myfaces_ext_0_9.tld">

    <jsp:directive.page contentType="text/html;charset=UTF-8" pageEncoding="UTF-8"/>
    <f:view>
        <wf:workspace_page>
               <h:form id="form1" name="form1" enctype="multipart/form-data">
                 <cmf:inputFileUpload id="fileupload"
                                       accept="image/*"
                                       value="#{listDocuments2.upFile}"
                                       storage="file"
                                       styleClass="fileUploadInput"
                                       required="true"/>
                    <h:outputText value="and give it a name: "/>
                    <h:inputText value="#{listDocuments2.name}"/>
                    <h:commandButton value="load it up" action="#{listDocuments2.upload}"/>
                </h:form>
            </wf:workspace_page>
    </f:view>
</jsp:root>
