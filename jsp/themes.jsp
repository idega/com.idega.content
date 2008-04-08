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
        <ws:page id="themeManager" javascripturls="
        				/idegaweb/bundles/com.idega.block.web2.0.bundle/resources/javascript/mootools/1.11/mootools-all-compressed.js,
        				/idegaweb/bundles/com.idega.block.web2.0.bundle/resources/javascript/reflection/for_mootools/1.2/reflection.js,
						/idegaweb/bundles/com.idega.block.web2.0.bundle/resources/javascript/moorainbow/1.1/mooRainbow.js,
        				
        				/idegaweb/bundles/com.idega.block.web2.0.bundle/resources/javascript/jquery/1.2.3/jquery-compressed.js,
        				/idegaweb/bundles/com.idega.block.web2.0.bundle/resources/javascript/contextmenu/r2/contextmenu-compressed.js,
						
						/dwr/engine.js,
        				/dwr/interface/ThemesEngine.js,
						
        				/idegaweb/bundles/com.idega.content.bundle/resources/javascript/ThemesManagerHelper.js,
        				/idegaweb/bundles/com.idega.content.bundle/resources/javascript/PageInfoHelper.js,
        				/idegaweb/bundles/com.idega.content.bundle/resources/javascript/ThemesHelper.js"
        				
        				stylesheeturls="/idegaweb/bundles/com.idega.block.web2.0.bundle/resources/javascript/moorainbow/1.1/mooRainbow.css">
			<h:form id="uploadForm" enctype="multipart/form-data">
				<jsf:verbatim>
					<script type="text/javascript">
						var $j = jQuery.noConflict();
						
						window.addEvent('domready', function() {
							var errorHanlder = function() {
								reloadPage();
							}
							DWREngine.setErrorHandler(errorHanlder);
						});
						window.addEvent('domready', getLocalizedTextForThemes);
						window.addEvent('domready', initializeThemes);
						window.addEvent('domready', roundThemesSliderCorners);
						window.addEvent('domready', function() {
							themeColourPicker = new MooRainbow('myRainbow', {
								imgPath: '/idegaweb/bundles/com.idega.block.web2.0.bundle/resources/javascript/moorainbow/1.1/images/',
								onComplete: function(color) {
									addThemeColorChange(color);
								},
								centerPosition: true
							});
						});
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
							<img id="themePreview" />
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
								<h:outputText styleClass="inputLabel" value="#{localizedStrings['com.idega.content']['theme_name']}"></h:outputText>
								<t:inputText id="theme_name" forceId="true"></t:inputText>
								<t:div>
									<t:commandButton id="changeVariationsButton" type="button" forceId="true" onclick="changeVariations()" title="#{localizedStrings['com.idega.content']['change_variations']}" value="#{localizedStrings['com.idega.content']['change']}"></t:commandButton>
									<t:commandButton id="themeSaveButton" type="button" forceId="true" onclick="saveTheme()" title="#{localizedStrings['com.idega.content']['save']}" value="#{localizedStrings['com.idega.content']['save']}"></t:commandButton>
									<t:commandButton id="themeRestoreButton" type="button" forceId="true" onclick="restoreTheme()" title="#{localizedStrings['com.idega.content']['restore_theme_to_original_state']}" value="#{localizedStrings['com.idega.content']['restore_theme']}"></t:commandButton>
								</t:div>
							</t:div>
						</t:div>
						<t:div styleClass="uploadForm">
							<h:outputText styleClass="variationHeading" value="#{localizedStrings['com.idega.content']['upload_theme']}"></h:outputText>
							<c:FileUploadViewer themePack="true" formId="uploadForm" allowMultipleFiles="true" actionAfterUpload="getThemes(THEME_ID, true, true);" actionAfterCounterReset="switchLoadingMessagesForTheme();" />
						</t:div>
					</t:div>
               	</t:div>
               	
               	<t:div styleClass="contextMenu" id="deleteThemeMenu" forceId="true" style="display: none;">
               		<t:htmlTag value="ul">
               			<t:htmlTag value="li" id="deleteThemeButton" forceId="true">
               				<t:outputText styleClass="deleteThemeTextStyle" onclick="deleteTheme();" title="#{localizedStrings['com.idega.content']['delete_theme']}" value="#{localizedStrings['com.idega.content']['delete_theme']}" />
               			</t:htmlTag>
               		</t:htmlTag>
				</t:div>
			</h:form>
        </ws:page>
	</jsf:view>
</jsp:root>