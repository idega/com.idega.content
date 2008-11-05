package com.idega.content.upload.presentation;

import java.rmi.RemoteException;
import java.util.List;

import com.idega.business.IBOLookup;
import com.idega.business.IBOLookupException;
import com.idega.content.business.ContentConstants;
import com.idega.presentation.Block;
import com.idega.presentation.CSSSpacer;
import com.idega.presentation.IWContext;
import com.idega.presentation.Layer;
import com.idega.presentation.text.Heading1;
import com.idega.presentation.ui.DropdownMenu;
import com.idega.presentation.ui.Label;
import com.idega.presentation.ui.SelectOption;
import com.idega.slide.business.IWSlideService;
import com.idega.util.CoreConstants;
import com.idega.util.ListUtil;
import com.idega.util.StringUtil;

public class FilesUploaderForm extends Block {
	
	private String parentPath = null;
	
	protected static final String PARENT_PATH_FOLDER_CHOOSER_PARAMETER = "parentPathForFolderChooserPrm";
	
	@Override
	public void main(IWContext iwc) {
		Layer container = new Layer();
		add(container);
		container.setStyleClass("filesUploaderFormStyle");
		
		if (iwc.isParameterSet(PARENT_PATH_FOLDER_CHOOSER_PARAMETER)) {
			parentPath = iwc.getParameter(PARENT_PATH_FOLDER_CHOOSER_PARAMETER);
		}
		if (StringUtil.isEmpty(parentPath)) {
			container.add(new Heading1(getResourceBundle(iwc).getLocalizedString("unkown_parent_path_in_repository", "Provide parent path in repository!")));
			return;
		}
		
		Layer folderChooserContainer = new Layer();
		container.add(folderChooserContainer);
		folderChooserContainer.setStyleClass("filesUploaderFolderChooserStyle");
		DropdownMenu folders = getFolderChooser(iwc);
		Label selectFolder = new Label(getResourceBundle(iwc).getLocalizedString("select_folder_in_repostory", "Select folder in repository"), folders);
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
		uploader.setActionAfterCounterReset("MOOdalBox.close();");
	}

	@SuppressWarnings("unchecked")
	private DropdownMenu getFolderChooser(IWContext iwc) {
		DropdownMenu folders = new DropdownMenu();
		
		IWSlideService slide = null;
		try {
			slide = (IWSlideService) IBOLookup.getServiceInstance(iwc, IWSlideService.class);
		} catch (IBOLookupException e) {
			e.printStackTrace();
		}
		if (slide == null) {
			return folders;
		}
		
		if (parentPath.startsWith(CoreConstants.WEBDAV_SERVLET_URI)) {
			parentPath = parentPath.replaceFirst(CoreConstants.WEBDAV_SERVLET_URI, CoreConstants.EMPTY);
		}
		
		List<Object> paths = null;
		try {
			paths = slide.getChildFolderPaths(parentPath);
		} catch (RemoteException e) {
			e.printStackTrace();
		}
		if (ListUtil.isEmpty(paths)) {
			folders.add(new SelectOption(parentPath, parentPath));
			folders.setSelectedElement(parentPath);
			return folders;
		}
		
		String pathString = null;
		for (int i = 0; i < paths.size(); i++) {
			pathString = paths.get(i).toString();
			if (pathString.startsWith(CoreConstants.WEBDAV_SERVLET_URI)) {
				pathString = pathString.replaceFirst(CoreConstants.WEBDAV_SERVLET_URI, CoreConstants.EMPTY);
			}
			
			folders.add(new SelectOption(pathString, pathString));
			
			if (i == 0) {
				folders.setSelectedElement(pathString);
			}
		}
		
		StringBuilder action = new StringBuilder("FileUploadHelper.changeUploadPath(DWRUtil.getValue('").append(folders.getId()).append("'), '")
								.append(ContentConstants.UPLOADER_PATH).append("');");
		folders.setOnChange(FileUploadViewer.getActionToLoadFilesAndExecuteCustomAction(action.toString()));
		
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
}
