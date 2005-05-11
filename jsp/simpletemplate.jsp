<?xml version="1.0"?>
<jsp:root xmlns:jsp="http://java.sun.com/JSP/Page"
        xmlns:h="http://java.sun.com/jsf/html"
        xmlns:f="http://java.sun.com/jsf/core"
        xmlns:wf="http://xmlns.idega.com/com.idega.webface"
        xmlns:ws="http://xmlns.idega.com/com.idega.workspace"
        xmlns:b="http://xmlns.idega.com/com.idega.builder"
        xmlns:x="http://myfaces.apache.org/extensions"
version="1.2">
<jsp:directive.page contentType="text/html;charset=UTF-8" pageEncoding="UTF-8"/>
	<f:view>
        <ws:page id="contentpages1">
                <h:form id="contentpagesform1">
					<wf:wfblock title="Pages">
						<f:facet name="wf_block_toolbar">
							<wf:toolbar id="toolbar">
								<wf:toolbarbutton id="button1" displayText="#{localizedStrings['com.idega.builder']['create_simple_template.Create']}/#{localizedStrings['com.idega.builder']['create_simple_template.Edit']}" styleClass="page_create_edit_link"/>
								<!--wf:toolbarbutton id="button2" displayText="Details" styleClass="page_details_link"/-->
							   	<h:commandLink id="saveCommand" action="save" actionListener="#{simpleTemplateCreationBean.processAction}" styleClass="page_preview_link">
			                    		<h:outputText value="#{localizedStrings['com.idega.builder']['create_simple_template.Save']}"/>
							   	</h:commandLink>
							   	<!--h:commandLink id="resetCommand" immediate="false" action="reset" actionListener="#{simpleTemplateCreationBean.processAction}" styleClass="page_preview_link">
			                    		<h:outputText value="#{localizedStrings['com.idega.builder']['create_simple_template.Reset']}"/>
							   	</h:commandLink-->
							</wf:toolbar>
						</f:facet>

						<wf:container styleClass="create_simple_template_fieldset">

						<h:outputText id="tempText" value="This page should be located in the builder and depends on that some template is selected as currentSelected page there.  If you have not done so, go to the builder and select the super template you want to use and then come back.  The super template must have region called 'main' as it is now" styleClass="label"/>
						<f:verbatim>
							<p/>
						</f:verbatim>

						<h:outputLabel for="simpleTemplate">
						  <h:outputText id="templateLabel" value="#{localizedStrings['com.idega.builder']['create_simple_template.PageType']}: " styleClass="label"/>
						</h:outputLabel>
						<h:selectOneMenu id="simpleTemplate" value="#{simpleTemplateCreationBean.simpleTemplateIdentifier}" styleClass="formInput">
							<f:selectItems value="#{simpleTemplateCreationBean.templateSelectItemList}"/>
						</h:selectOneMenu>

						<f:verbatim>
							<br/>
						</f:verbatim>

						<h:outputLabel for="templateName2">
						  <h:outputText id="templateNameLabel" value="#{localizedStrings['com.idega.builder']['create_simple_template.PageTypeName']}: " styleClass="label"/>
						</h:outputLabel>
						<h:inputText id="templateName" value="#{simpleTemplateCreationBean.simpleTemplateName}" styleClass="formInput"/>

						<f:verbatim>
							<br/>
						</f:verbatim>

						<!--h:outputLabel for="image">
						  <h:outputText id="imageLabel" value="#{localizedStrings['com.idega.builder']['create_simple_template.Image']}: " styleClass="label"/>
						</h:outputLabel>
						<h:inputText id="image" value="[Select image]" styleClass="formInput"/>

						<f:verbatim>
							<br/>
						</f:verbatim>

						<h:outputLabel for="description">
						  <h:outputText id="descriptionLabel" value="#{localizedStrings['com.idega.builder']['create_simple_template.Description']}: " styleClass="label"/>
						</h:outputLabel>
						<h:inputTextarea id="description" value="" rows="4" cols="40" styleClass="formInput"/>

						<f:verbatim>
							<br/>
						</f:verbatim>

						
						<h:outputLabel for="type">
						  <h:outputText id="typeLabel" value="#{localizedStrings['com.idega.builder']['create_simple_template.Type']}: " styleClass="label"/>
						</h:outputLabel>
						<h:selectOneMenu id="type" value="jsp" styleClass="formInput">
							<f:selectItem itemValue="jsp" itemLabel="JSP"/>
							<f:selectItem itemValue="ibxml" itemLabel="IBXML"/>
						</h:selectOneMenu>

						<f:verbatim>
							<br/>
						</f:verbatim-->

						<h:outputLabel for="region">
						  <h:outputText id="regionLabel" value="#{localizedStrings['com.idega.builder']['create_simple_template.Region']}: " styleClass="label"/>
						</h:outputLabel>
						<h:selectOneMenu id="region" value="main" styleClass="formInput">
							<f:selectItem itemValue="main" itemLabel="Main"/>
						</h:selectOneMenu>

						<f:verbatim>
							<br/>
						</f:verbatim>

						<h:outputLabel for="component">
						  <h:outputText id="componentLabel" value="#{localizedStrings['com.idega.builder']['create_simple_template.Component']}: " styleClass="label"/>
						</h:outputLabel>
						<h:selectOneMenu id="component" value="#{simpleTemplateCreationBean.selectedComponent}" styleClass="formInput">
							<f:selectItems value="#{simpleTemplateCreationBean.componentSelectItemList}"/>
						</h:selectOneMenu>
						
						
						</wf:container>
						
					</wf:wfblock>
                </h:form>
        </ws:page>
	</f:view>
</jsp:root>