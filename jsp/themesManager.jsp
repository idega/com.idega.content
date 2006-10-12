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
        <ws:page id="themeManager" javascripturls="/idegaweb/bundles/com.idega.content.bundle/scripts/ThemesPreviewsProvider.js,/dwr/engine.js,/idegaweb/bundles/com.idega.content.bundle/scripts/ThemesManagerHelper.js">
                <h:form id="uploadForm" enctype="multipart/form-data" onsubmit="showLoadingMessage('Uploading theme...');">
					<wf:wfblock id="themeManagerBlock" title="#{localizedStrings['com.idega.content']['themes_manager']}" >
						<wf:container id="themesGallery">
							<a:ajax name="ibrowser" template="themesPreview" style="themesPreview" script="themesPreview" args="{type: 'ibrowser'}"/>
						</wf:container>
						<div id="themeStyleVariations" style="display: block; position: relative; float: right; top: 110px; right: 30px; z-index: 100;">
						</div>
						<div id="themeSaveArea" style="display: block; position: relative; float: right; top: 500px; right: 30px; z-index: 100;">
							<input type="button" id="themeSaveButton" onclick="saveTheme();" disabled="true" value="Save"/>
						</div>
                	</wf:wfblock>
                	<c:ThemesManager id="uploadBlock"></c:ThemesManager>
                </h:form>
        </ws:page>
</jsf:view>
</jsp:root>