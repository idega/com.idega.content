package com.idega.content.presentation;

import javax.faces.component.UIComponent;
import javax.faces.event.AbortProcessingException;
import javax.faces.event.ActionEvent;

import com.idega.webface.event.WFToolbarButtonPressedListener;

/**
 * @author gimmi
 */
public class ContentViewerBean implements WFToolbarButtonPressedListener {

	public void processAction(ActionEvent actionEvent) throws AbortProcessingException {
		UIComponent parent = ((UIComponent)actionEvent.getSource()).getParent();
		ContentViewer viewer = (ContentViewer) parent.getParent();
		viewer.processAction(actionEvent);
	}

}
