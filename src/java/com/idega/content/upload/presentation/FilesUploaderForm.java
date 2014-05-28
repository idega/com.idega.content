package com.idega.content.upload.presentation;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;

import com.idega.content.business.ContentConstants;
import com.idega.content.upload.business.FileUploader;
import com.idega.presentation.Block;
import com.idega.presentation.CSSSpacer;
import com.idega.presentation.IWContext;
import com.idega.presentation.Layer;
import com.idega.presentation.text.Heading1;
import com.idega.presentation.ui.DropdownMenu;
import com.idega.presentation.ui.Label;
import com.idega.presentation.ui.SelectOption;
import com.idega.repository.bean.RepositoryItem;
import com.idega.util.CoreConstants;
import com.idega.util.ListUtil;
import com.idega.util.StringUtil;
import com.idega.util.expression.ELUtil;

public class FilesUploaderForm extends Block {

	@Autowired
	private FileUploader fileUploader;

	private boolean reloadPageAfterUpload = false;

	private String parentPath = null;

	protected static final String 	PARENT_PATH_FOLDER_CHOOSER_PARAMETER = "parentPathForFolderChooserPrm",
									COMPONENT_TO_RERENDER_ID_PARAMETER = "componentToRerenderIdPrm",
									RELOAD_AFTER_UPLOAD = "reloadAfterUploadPrm";

	@Override
	public void main(IWContext iwc) {
		ELUtil.getInstance().autowire(this);

		Layer container = new Layer();
		add(container);
		container.setStyleClass("filesUploaderFormStyle");

		if (iwc.isParameterSet(PARENT_PATH_FOLDER_CHOOSER_PARAMETER)) {
			parentPath = iwc.getParameter(PARENT_PATH_FOLDER_CHOOSER_PARAMETER);
		}
		if (iwc.isParameterSet(RELOAD_AFTER_UPLOAD)) {
			reloadPageAfterUpload = Boolean.valueOf(iwc.getParameter(RELOAD_AFTER_UPLOAD));
		}
		if (StringUtil.isEmpty(parentPath)) {
			container.add(new Heading1(getResourceBundle(iwc).getLocalizedString("unkown_parent_path_in_repository", "Provide parent path in repository!")));
			return;
		}
		if (!parentPath.startsWith(CoreConstants.SLASH)) {
			parentPath = new StringBuilder(CoreConstants.SLASH).append(parentPath).toString();
		}
		if (!parentPath.endsWith(CoreConstants.SLASH)) {
			parentPath = new StringBuilder(parentPath).append(CoreConstants.SLASH).toString();
		}

		Layer folderChooserContainer = new Layer();
		container.add(folderChooserContainer);
		folderChooserContainer.setStyleClass("filesUploaderFolderChooserStyle");
		DropdownMenu folders = getFolderChooser(iwc);
		Label selectFolder = new Label(getResourceBundle(iwc).getLocalizedString("select_folder_in_repository", "Select folder in repository"), folders);
		folderChooserContainer.add(selectFolder);
		folderChooserContainer.add(folders);

		container.add(new CSSSpacer());

		String uploadPath = folders.getSelectedElementValue();
		Layer uploaderContainer = new Layer();
		uploaderContainer.setStyleClass("filesUploaderUploaderContainerStyle");
		container.add(uploaderContainer);
		FileUploadViewer uploader = new FileUploadViewer();
		uploaderContainer.add(uploader);
		uploader.setUploadPath(StringUtil.isEmpty(uploadPath) ? parentPath : uploadPath);
		uploader.setAutoAddFileInput(true);
		uploader.setShowLoadingMessage(true);
		uploader.setActionAfterCounterReset("MOOdalBox.close();" +
				(isReloadPageAfterUpload() ?
						"showLoadingMessage('" + getResourceBundle(iwc).getLocalizedString("reloading", "Reloading...") + "');reloadPage();" :
						CoreConstants.EMPTY)
		);

		if (iwc.isParameterSet(COMPONENT_TO_RERENDER_ID_PARAMETER)) {
			uploader.setComponentToRerenderId(iwc.getParameter(COMPONENT_TO_RERENDER_ID_PARAMETER));
		}
	}

	public boolean isReloadPageAfterUpload() {
		return reloadPageAfterUpload;
	}

	public void setReloadPageAfterUpload(boolean reloadPageAfterUpload) {
		this.reloadPageAfterUpload = reloadPageAfterUpload;
	}

	private DropdownMenu getFolderChooser(IWContext iwc) {
		DropdownMenu folders = new DropdownMenu();

		if (parentPath.startsWith(CoreConstants.WEBDAV_SERVLET_URI)) {
			parentPath = parentPath.replaceFirst(CoreConstants.WEBDAV_SERVLET_URI, CoreConstants.EMPTY);
		}

		List<String> paths = null;
		try {
			paths = getRepositoryService().getChildFolderPaths(parentPath);
		} catch (Exception e) {
			e.printStackTrace();
		}
		if (ListUtil.isEmpty(paths)) {
			folders.add(new SelectOption(parentPath, parentPath));
			folders.setSelectedElement(parentPath);
			return folders;
		}

		String pathString = null;
		for (int i = 0; i < paths.size(); i++) {
			pathString = paths.get(i);
			if (pathString.startsWith(CoreConstants.WEBDAV_SERVLET_URI)) {
				pathString = pathString.replaceFirst(CoreConstants.WEBDAV_SERVLET_URI, CoreConstants.EMPTY);
			}

			RepositoryItem item = null;
			try {
				item = getRepositoryService().getRepositoryItem(pathString);
			} catch (Exception e) {
				e.printStackTrace();
			}
			folders.add(new SelectOption(item == null ? pathString.replaceFirst(parentPath, CoreConstants.EMPTY) : item.getName(), pathString));

			if (i == 0) {
				folders.setSelectedElement(pathString);
			}
		}

		StringBuilder action = new StringBuilder("FileUploadHelper.changeUploadPath(dwr.util.getValue('").append(folders.getId()).append("'), '")
								.append(ContentConstants.UPLOADER_PATH).append("');");
		folders.setOnChange(getFileUploader().getActionToLoadFilesAndExecuteCustomAction(action.toString(), true, true));

		return folders;
	}

	public String getParentPath() {
		return parentPath;
	}

	public void setParentPath(String parentPath) {
		this.parentPath = parentPath;
	}

	@Override
	public String getBundleIdentifier() {
		return ContentConstants.IW_BUNDLE_IDENTIFIER;
	}

	public FileUploader getFileUploader() {
		return fileUploader;
	}

	public void setFileUploader(FileUploader fileUploader) {
		this.fileUploader = fileUploader;
	}

}