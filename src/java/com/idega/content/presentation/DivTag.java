package com.idega.content.presentation;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;
import javax.faces.webapp.UIComponentTag;

public class DivTag extends UIComponentTag {
    private String styleClass;
    private String id;

    public DivTag() {
    }

    public void setId(String id) {
        this.id = id;
    }

    public String getId() {
        return id;
    }
    
    
    public String getComponentType() {
        return UIDiv.COMPONENT_TYPE;
    }

    public String getRendererType() {
        return UIDiv.RENDERER_TYPE;
    }

    public void setProperties(UIComponent component) {
        super.setProperties(component);
        setStringProperty(component, "styleclass", styleClass);
    }

    public void setStyleclass(String styleClass) {
        this.styleClass = styleClass;
    }

    public String getStyleclass() {
        return styleClass;
    }

    private void setStringProperty(UIComponent component, String name, 
                                   String value) {
        if (value == null) {
            return;
        }
        if (isValueReference(value)) {
            component.setValueBinding(name, 
                                      FacesContext.getCurrentInstance().getApplication()
                                      .createValueBinding(value));
        } else {
            component.getAttributes().put(name, value);
        }
    }
}
