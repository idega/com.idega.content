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
               <h:form id="uploadForm" name="uploadForm" enctype="multipart/form-data">
               <f:verbatim><br/></f:verbatim>
                    <h:outputText value="Select a file to upload : "/>
                 <cmf:inputFileUpload id="fileupload"
                                       accept="*"
                                       value="#{WebDAVUploadBean.uploadFile}"
                                       storage="file"
                                       styleClass="fileUploadInput"
                                       required="true"/>
                <f:verbatim><br/></f:verbatim>
                    <h:outputText value="and give it a name (optional) : "/>
                    <h:inputText value="#{WebDAVUploadBean.fileName}"/>
                    <f:verbatim><br/></f:verbatim>
                    <h:outputText value="and select the folder to upload to (optional) : "/>
                     <h:inputText value="#{WebDAVUploadBean.uploadFilePath}"/>
                    <h:commandButton value="Upload" action="#{WebDAVUploadBean.upload}"/>
                
                <f:verbatim><br/></f:verbatim>
                <h:outputLink id="filelink" value="#{WebDAVUploadBean.downloadPath}" target="_new">
                <f:verbatim>Click here to get the file</f:verbatim>
                </h:outputLink>
                <f:verbatim><br/></f:verbatim>
                <h:graphicImage id="imagePreview" value="#{WebDAVUploadBean.imagePath}"/>
                
                </h:form>
                
            </wf:workspace_page>
    </f:view>
</jsp:root>