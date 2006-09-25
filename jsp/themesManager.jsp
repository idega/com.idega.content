<?xml version="1.0"?>
<jsp:root xmlns:jsp="http://java.sun.com/JSP/Page"
        xmlns:h="http://java.sun.com/jsf/html"
        xmlns:jsf="http://java.sun.com/jsf/core"
        xmlns:wf="http://xmlns.idega.com/com.idega.webface"
        xmlns:ws="http://xmlns.idega.com/com.idega.workspace"
        xmlns:a="http://java.sun.com/jmaki-jsf"
        xmlns:c="http://xmlns.idega.com/com.idega.content"
version="1.2">
<jsp:directive.page contentType="text/html;charset=UTF-8" pageEncoding="UTF-8"/>
<jsf:view>
        <ws:page id="themeManager" javascripturls="/idegaweb/bundles/com.idega.content.bundle/scripts/ThemesPreviewsProvider.js,/idegaweb/bundles/com.idega.content.bundle/scripts/DWREngine.js,/idegaweb/bundles/com.idega.content.bundle/scripts/ThemesManagerHelper.js">
                <h:form id="uploadForm" enctype="multipart/form-data">
					<wf:wfblock id="themeManagerBlock" title="#{localizedStrings['com.idega.content']['themes_manager']}" >
						<wf:container id="themesGallery">
							<a:ajax name="ibrowser" template="themesPreview" style="themesPreview" script="themesPreview" args="{type: 'ibrowser'}"/>
						</wf:container>
                	</wf:wfblock>
                	<c:ThemesManager id="uploadBlock"></c:ThemesManager>
                </h:form>
        </ws:page>
</jsf:view>
</jsp:root>