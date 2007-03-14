<?xml version="1.0"?>
<jsp:root xmlns:jsp="http://java.sun.com/JSP/Page"
        xmlns:h="http://java.sun.com/jsf/html"
        xmlns:jsf="http://java.sun.com/jsf/core"
        xmlns:wf="http://xmlns.idega.com/com.idega.webface"
        xmlns:ws="http://xmlns.idega.com/com.idega.workspace"
        xmlns:c="http://xmlns.idega.com/com.idega.content"
        xmlns:x="http://myfaces.apache.org/tomahawk"
version="1.2">
<jsp:directive.page contentType="text/html;charset=UTF-8" pageEncoding="UTF-8"/>
	<jsf:view>
        <ws:page id="themeManager" javascripturls="/dwr/engine.js,
        				/dwr/interface/ThemesEngine.js,
        				/idegaweb/bundles/com.idega.block.web2.0.bundle/resources/javascript/scriptaculous/1.7.0/lib/prototype.js,
        				/idegaweb/bundles/com.idega.block.web2.0.bundle/resources/javascript/scriptaculous/1.7.0/src/scriptaculous.js,
        				/idegaweb/bundles/com.idega.content.bundle/resources/javascript/ThemesManagerHelper.js,
        				/idegaweb/bundles/com.idega.content.bundle/resources/javascript/PageInfoHelper.js,
        				/idegaweb/bundles/com.idega.content.bundle/resources/javascript/ThemesHelper.js,
						/idegaweb/bundles/com.idega.block.web2.0.bundle/resources/javascript/reflection/reflection.js">
			<h:form id="uploadForm" enctype="multipart/form-data" onsubmit="showLoadingMessage(getUploadingThemeText());">
				<jsf:verbatim>
					<script type="text/javascript">
						addEvent(window, "load", getLocalizedTextForThemes);
						addEvent(window, "load", initializeThemes);
					</script>
				</jsf:verbatim>
				<wf:wfblock id="themeManagerBlock" title="#{localizedStrings['com.idega.content']['themes']}" >
					<x:div id="themePreviewContainer" forceId="true" style="width: auto">
						<jsf:verbatim>
							<iframe id="themePreview">iframe</iframe>
						</jsf:verbatim>
					</x:div>
					
					<x:div id="themesSliderContainer" forceId="true" styleClass="theme_slider">
						<x:div id="leftScrollerContainer" forceId="true" styleClass="leftThemeScroller">	
							<x:graphicImage url="/idegaweb/bundles/com.idega.content.bundle/resources/images/left.gif" onclick="scroll(this.id)" id="leftScroller" forceId="true" title="#{localizedStrings['com.idega.content']['scroll_left']}"></x:graphicImage>
						</x:div>	
						
						<x:div id="themesTickerContainer" forceId="true" styleClass="themesTicker">
							<x:div id="themes" forceId="true" styleClass="multiImageGallery"></x:div>
						</x:div>
						
						<x:div id="rightScrollerContainer" forceId="true" styleClass="rightThemeScroller">
							<x:graphicImage url="/idegaweb/bundles/com.idega.content.bundle/resources/images/right.gif" onclick="scroll(this.id)" id="rightScroller" forceId="true" title="#{localizedStrings['com.idega.content']['scroll_right']}"></x:graphicImage>
						</x:div>
					</x:div>
					
					<x:div styleClass="theme_container">
						<wf:wfblock id="themeStyleVariationsBlock" title="#{localizedStrings['com.idega.content']['theme_variations']}">
							<x:inputHidden id="defaultThemeLabel" forceId="true" value="#{localizedStrings['com.idega.content']['theme_is_default']}"></x:inputHidden>
							<x:inputHidden id="notDefaultThemeLabel" forceId="true" value="#{localizedStrings['com.idega.content']['theme_is_not_default']}"></x:inputHidden>
							<x:div id="themeStyleVariations" forceId="true"></x:div>
							<x:div id="themeUsability" forceId="true" styleClass="themeUsabilityStyle"></x:div>
							
							<x:div>
								<x:div id="themeSaveArea" forceId="true" styleClass="wf_webdav_upload">
									<h:outputText value="#{localizedStrings['com.idega.content']['theme_name']}"></h:outputText>
									<x:inputText id="theme_name" forceId="true"></x:inputText>
									<x:commandButton id="changeVariationsButton" type="button" forceId="true" onclick="changeVariations()" title="#{localizedStrings['com.idega.content']['change_variations']}" value="#{localizedStrings['com.idega.content']['change']}"></x:commandButton>
									<x:commandButton id="themeSaveButton" type="button" forceId="true" onclick="saveTheme()" title="#{localizedStrings['com.idega.content']['save']}" value="#{localizedStrings['com.idega.content']['save']}"></x:commandButton>
									<x:commandButton id="themeRestoreButton" type="button" forceId="true" onclick="restoreTheme()" title="#{localizedStrings['com.idega.content']['restore_theme']}" value="#{localizedStrings['com.idega.content']['restore_theme']}"></x:commandButton>
								</x:div>
							</x:div>
						</wf:wfblock>
						<c:ThemesManager id="uploadBlock"></c:ThemesManager>
					</x:div>
               	</wf:wfblock>
			</h:form>
        </ws:page>
	</jsf:view>
</jsp:root>