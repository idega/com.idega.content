package com.idega.content.presentation;

import javax.faces.component.UIComponent;
import javax.faces.event.AbortProcessingException;
import javax.faces.event.ActionEvent;
import javax.faces.event.ActionListener;

import com.idega.webface.event.WFToolbarButtonPressedListener;

/**
 * @author gimmi
 */
public class ContentViewerBean implements WFToolbarButtonPressedListener {

	public void processAction(ActionEvent actionEvent) throws AbortProcessingException {
		UIComponent parent = ((UIComponent)actionEvent.getSource()).getParent();
		while (parent!= null && !(parent instanceof ActionListener)) {
			parent = parent.getParent();
		}
		ActionListener listener = (ActionListener) parent;
		listener.processAction(actionEvent);
	}

}
