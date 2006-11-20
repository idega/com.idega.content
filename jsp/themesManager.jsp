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
        				/idegaweb/bundles/com.idega.block.web2.0.bundle/resources/javascript/scriptaculous-js-1.6.2/lib/prototype.js,
        				/idegaweb/bundles/com.idega.block.web2.0.bundle/resources/javascript/scriptaculous-js-1.6.2/src/scriptaculous.js,
        				/idegaweb/bundles/com.idega.content.bundle/resources/javascript/ThemesManagerHelper.js,
						/idegaweb/bundles/com.idega.block.web2.0.bundle/resources/javascript/reflection/reflection.js">
                <h:form id="uploadForm" enctype="multipart/form-data" onsubmit="showLoadingMessage('Uploading theme...');">
                	<jsf:verbatim>
                		<script type="text/javascript">insertStyleFile();initScript(false, true, true);getThemes(null, false);</script>
                	</jsf:verbatim>
					<wf:wfblock id="themeManagerBlock" title="#{localizedStrings['com.idega.content']['themes_manager']}" >
						<x:div id="themePreviewContainer" forceId="true">
							<x:graphicImage id="themePreview" forceId="true" styleClass="bigThemePreview" url="noImage.png"></x:graphicImage>
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
							<wf:wfblock title="#{localizedStrings['com.idega.content']['theme_variations']}">
								<x:div id="themeStyleVariations" forceId="true"></x:div>
								
								<x:div>
									<x:div id="themeSaveArea" forceId="true" styleClass="wf_webdav_upload">
										<h:outputText value="#{localizedStrings['com.idega.content']['theme_name']}"></h:outputText>
										<x:inputText id="theme_name" forceId="true"></x:inputText>
										<x:commandButton id="themeSaveButton" type="button" forceId="true" onclick="saveTheme()" value="#{localizedStrings['com.idega.content']['save']}"></x:commandButton>
										<x:commandButton id="themeRestoreButton" type="button" forceId="true" onclick="restoreTheme()" value="#{localizedStrings['com.idega.content']['restore_theme']}"></x:commandButton>
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