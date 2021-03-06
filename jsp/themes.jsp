<?xml version="1.0"?>
<jsp:root xmlns:jsp="http://java.sun.com/JSP/Page"
        xmlns:h="http://java.sun.com/jsf/html"
        xmlns:jsf="http://java.sun.com/jsf/core"
        xmlns:wf="http://xmlns.idega.com/com.idega.webface"
        xmlns:ws="http://xmlns.idega.com/com.idega.workspace"
        xmlns:c="http://xmlns.idega.com/com.idega.content"
        xmlns:t="http://myfaces.apache.org/tomahawk" version="1.2">
<jsp:directive.page contentType="text/html;charset=UTF-8" pageEncoding="UTF-8"/>
	<jsf:view>
        <ws:page id="themeManager" javascripturls="#{lucidEngine.javaScriptResourcesForThemes}" stylesheeturls="#{lucidEngine.styleSheetResourcesForThemes}">
			<h:form id="uploadForm" enctype="multipart/form-data">
				<jsf:verbatim>
					<script type="text/javascript">
						window.addEvent('domready', function() {
                			errorHandlerInLucid();
						});
						window.addEvent('domready', ThemesManagerHelper.prepareThemesForUsageInLucid);
					</script>
				</jsf:verbatim>

				<t:div id="mainThemesContainer" forceId="true">
					<t:div style="display: none;">
						<jsf:verbatim>
							<img id="myRainbow" src="/idegaweb/bundles/com.idega.content.bundle/resources/images/button_atom.gif" alt="[r]" width="16" height="16" />
							<input id="myInput" name="myInput" type="text" size="13" />
						</jsf:verbatim>
					</t:div>
					<t:div id="themePreviewContainer" forceId="true" styleClass="themePreviewContainerStyle">
						<jsf:verbatim>
							<iframe name="themePreviewFrame" id="themePreviewFrame">iframe</iframe>
						</jsf:verbatim>
					</t:div>
						
					<c:ThemesSliderViewer />
					
					<t:div id="themeVariationsContainer" forceId="true" styleClass="theme_container">
						<h:outputText styleClass="variationHeading" value="#{localizedStrings['com.idega.content']['theme_variations']}"></h:outputText>
						<t:div id="themeStyleVariationsBlock" forceId="true">
							<t:div id="themeUsability" forceId="true" styleClass="themeUsabilityStyle"></t:div>
							<t:inputHidden id="defaultThemeLabel" forceId="true" value="#{localizedStrings['com.idega.content']['theme_is_default']}"></t:inputHidden>
							<t:inputHidden id="notDefaultThemeLabel" forceId="true" value="#{localizedStrings['com.idega.content']['theme_is_not_default']}"></t:inputHidden>
							<t:div id="themeStyleVariations" forceId="true"></t:div>
						</t:div>
						<t:div id="themeSaveArea" forceId="true">
							<h:outputText styleClass="variationHeading" value="#{localizedStrings['com.idega.content']['save_theme']}"></h:outputText>
							<t:div styleClass="wf_webdav_upload">
								<t:div styleClass="themeSaveAreaInput">
									<h:outputText styleClass="inputLabel" value="#{localizedStrings['com.idega.content']['theme_name']}"></h:outputText>
									<t:inputText id="theme_name" forceId="true"></t:inputText>
								</t:div>
								<t:div styleClass="themeSaveAreaButtons">
									<t:commandButton forceId="true" id="themeSaveButton" onclick="saveTheme(); return false;" title="#{localizedStrings['com.idega.content']['save']}" value="#{localizedStrings['com.idega.content']['save']}"></t:commandButton>
									<t:commandButton forceId="true" id="themeRestoreButton" onclick="restoreTheme(); return false;" title="#{localizedStrings['com.idega.content']['restore_theme_to_original_state']}" value="#{localizedStrings['com.idega.content']['restore_theme']}"></t:commandButton>
								</t:div>
							</t:div>
						</t:div>
						<t:div id="themeUploader" forceId="true" styleClass="uploadForm">
							<h:outputText styleClass="variationHeading" value="#{localizedStrings['com.idega.content']['upload_theme']}"></h:outputText>
							<t:div styleClass="uploadThemeForm">
								<c:FileUploadViewer themePack="true" formId="uploadForm" allowMultipleFiles="true" actionAfterUpload="getThemes(THEME_ID, true, true);" actionAfterCounterReset="switchLoadingMessagesForTheme();" autoAddFileInput="false" />
							</t:div>
						</t:div>
					</t:div>
               	</t:div>
               	
               	<t:div styleClass="contextMenu" id="deleteThemeMenu" forceId="true" style="display: none;">
               		<t:htmlTag value="ul">
               			<t:htmlTag value="li" id="deleteThemeButton" forceId="true">
               				<t:outputText styleClass="deleteThemeTextStyle" onclick="deleteTheme();" title="#{localizedStrings['com.idega.content']['delete_theme']}" value="#{localizedStrings['com.idega.content']['delete_theme']}" />
               			</t:htmlTag>
               			<t:htmlTag value="li" id="deleteThemesButton" forceId="true">
               				<t:outputText styleClass="deleteThemeTextStyle" onclick="deleteAllThemes();" title="#{localizedStrings['com.idega.content']['delete_all_themes']}" value="#{localizedStrings['com.idega.content']['delete_all_themes']}" />
               			</t:htmlTag>
               		</t:htmlTag>
				</t:div>
			</h:form>
        </ws:page>
	</jsf:view>
</jsp:root>