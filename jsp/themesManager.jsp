<?xml version="1.0"?>
<jsp:root xmlns:jsp="http://java.sun.com/JSP/Page"
        xmlns:h="http://java.sun.com/jsf/html"
        xmlns:jsf="http://java.sun.com/jsf/core"
        xmlns:wf="http://xmlns.idega.com/com.idega.webface"
        xmlns:ws="http://xmlns.idega.com/com.idega.workspace"
        xmlns:a="http://java.sun.com/jmaki-jsf"
        xmlns:c="http://xmlns.idega.com/com.idega.content"
        xmlns:x="http://myfaces.apache.org/tomahawk"
version="1.2">
<jsp:directive.page contentType="text/html;charset=UTF-8" pageEncoding="UTF-8"/>
<jsf:view>
        <ws:page id="themeManager" javascripturls="/dwr/engine.js,/dwr/interface/ThemesPreviewsProvider.js,/idegaweb/bundles/com.idega.content.bundle/resources/javascript/ThemesManagerHelper.js">
                <h:form id="uploadForm" enctype="multipart/form-data" onsubmit="showLoadingMessage('Uploading theme...');">
                	<x:div id="themesContainer" forceId="true" styleClass="themesContainer">
						<wf:wfblock id="themeManagerBlock" title="#{localizedStrings['com.idega.content']['themes_manager']}" >
							<wf:container id="themesGallery"> <!-- This container will be replaced with new image slider -->
								<a:ajax name="ibrowser" template="themesPreview" style="themesPreview" script="themesPreview" args="{type: 'ibrowser'}"/>
							</wf:container>
							
							<x:div styleClass="theme_container">
								<x:div styleClass="variationsAndSaveBox">
									<wf:wfblock title="#{localizedStrings['com.idega.content']['theme_variations']}">
										<x:div id="themeStyleVariations" forceId="true" styleClass="variationsBox"></x:div>
										
										<x:div styleClass="wf_webdav_upload"> <!-- Theme name input and save button -->
											<h:outputText value="#{localizedStrings['com.idega.content']['theme_name']}"></h:outputText>
											<x:inputText id="theme_name" onblur="enableButton(this.id)" forceId="true"></x:inputText>
											<x:commandButton id="themeSaveButton" type="button" forceId="true" onclick="saveTheme()" value="#{localizedStrings['com.idega.content']['save']}"></x:commandButton>
										</x:div>
									</wf:wfblock>
								</x:div>
								
								<x:div id="themeUploadContainer" forceId="true" styleClass="themeUploadBox">
									<c:ThemesManager id="uploadBlock"></c:ThemesManager> <!-- Style classes are set in tag -->
								</x:div>
							</x:div>
							
                		</wf:wfblock>
                	</x:div>
                </h:form>
        </ws:page>
</jsf:view>
</jsp:root>