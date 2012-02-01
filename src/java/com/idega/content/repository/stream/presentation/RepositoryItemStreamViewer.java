package com.idega.content.repository.stream.presentation;

import java.util.UUID;

import com.idega.content.business.ContentConstants;
import com.idega.content.presentation.WebDAVListManagedBean;
import com.idega.idegaweb.IWResourceBundle;
import com.idega.presentation.Block;
import com.idega.presentation.IWContext;
import com.idega.presentation.Layer;
import com.idega.presentation.text.Heading2;
import com.idega.presentation.text.Text;
import com.idega.presentation.ui.Form;
import com.idega.presentation.ui.GenericButton;
import com.idega.presentation.ui.InterfaceObject;
import com.idega.presentation.ui.Label;
import com.idega.presentation.ui.RadioGroup;
import com.idega.presentation.ui.TextInput;
import com.idega.util.ArrayUtil;

public class RepositoryItemStreamViewer extends Block {

	private IWResourceBundle iwrb;

	@Override
	public void main(IWContext iwc) throws Exception {
		String[] urls = iwc.getParameterValues(WebDAVListManagedBean.PARAMETER_WEB_DAV_URL);
		if (ArrayUtil.isEmpty(urls))
			return;

		String url = urls[urls.length - 1];

		iwrb = getResourceBundle(iwc);

		Layer container = new Layer();
		container.add(new Heading2(iwrb.getLocalizedString("upload_to_remote_server", "Upload selected item to remote server")));

		Form form = new Form();
		container.add(form);
		String serverId = addFormItem(form, iwrb.getLocalizedString("server", "Server"), "server", "http://").getId();
		String directoryId = addFormItem(form, iwrb.getLocalizedString("destination_in_repository", "Destination in repository"), "directory", url).getId();
		String reCreateFolderStructure = addFormItem(form, iwrb.getLocalizedString("create_identical_file_structure", "Create identical file structure"), "structure",
				Boolean.TRUE).getId();
		String uuid = UUID.randomUUID().toString();

		Layer resultsContainer = new Layer();
		container.add(resultsContainer);

		Layer buttons = new Layer();
		buttons.setStyleClass("webfaceButtonLayer");
		buttons.setStyleAttribute("float", "right");
		container.add(buttons);
		GenericButton stream = new GenericButton("stream", iwrb.getLocalizedString("stream", "Stream"));
		stream.setOnClick("ContentAdmin.streamToRemoteServer('" + serverId + "', '" + url + "', '" + directoryId + "', '" + reCreateFolderStructure + "', '" + uuid + "', '" +
				resultsContainer.getId() + "');");
		buttons.add(stream);

		add(container);
	}

	private InterfaceObject addFormItem(Form form, String label, String name, Boolean value) {
		Layer formItem = new Layer();
		formItem.setStyleClass("webfaceFormItem repositoryItemStreamerReCreateStructure");
		form.add(formItem);

		RadioGroup group = new RadioGroup(name);
		group.setStyleAttribute("width", "200px");
		group.addRadioButton(Boolean.TRUE.toString(), new Text(iwrb.getLocalizedString("yes", "Yes")), value);
		group.addRadioButton(Boolean.FALSE.toString(), new Text(iwrb.getLocalizedString("no", "No")), !value);
		Label labelUI = new Label(label, group);
		labelUI.setStyleAttribute("width", "180px");
		formItem.add(labelUI);
		formItem.add(group);

		return group;
	}

	private InterfaceObject addFormItem(Form form, String label, String name, String value) {
		Layer formItem = new Layer();
		formItem.setStyleClass("webfaceFormItem");
		form.add(formItem);

		TextInput input = new TextInput(name, value);
		input.setStyleAttribute("width", "200px");
		Label labelUI = new Label(label, input);
		labelUI.setStyleAttribute("width", "180px");
		formItem.add(labelUI);
		formItem.add(input);

		return input;
	}

	@Override
	public String getBundleIdentifier() {
		return ContentConstants.IW_BUNDLE_IDENTIFIER;
	}

}