<?xml version="1.0" encoding="UTF-8"?>
<jsp:root version="1.2" 
	xmlns:f="http://java.sun.com/jsf/core" 	
	xmlns:h="http://java.sun.com/jsf/html" 	
	xmlns:jsp="http://java.sun.com/JSP/Page" 	
	xmlns:w="http://xmlns.idega.com/com.idega.webface"
	xmlns:cmf="http://myfaces.sourceforge.net/tld/myfaces_ext_0_9.tld"
	xmlns:c="http://xmlns.idega.com/com.idega.content">
    <jsp:directive.page contentType="text/html;charset=UTF-8" pageEncoding="UTF-8"/>
    <f:view>
    		<w:workspace_page id="1234">
			<h:form id="form1">
				<c:ContentViewer id="gt"/>
 	       		<!--
 	       		<c:WebDAVList id="giT"/>
 	       		<c:WebDAVFileDetails id="id" webDAVPath="#{WebDAVListBean.getClickedFilePath}"/>
 	       		<c:WebDAVFileDetails id="id" webDAVPath="#{WebDAVListBean.webDAVPath}"/>
 	       		<cmf:inputFileUpload id="fileupload"
                                       accept="*"
                                       value="#{WebDAVUploadBean.uploadFile}"
                                       storage="file"
                                       styleClass="fileUploadInput"
                                       required="true"/>
                                       -->
			</h:form>
		</w:workspace_page>
    </f:view>
</jsp:root>

