package com.idega.content.upload.presentation.handler;

import java.rmi.RemoteException;
import java.util.List;

import com.idega.builder.bean.AdvancedProperty;
import com.idega.core.builder.presentation.ICPropertyHandler;
import com.idega.presentation.IWContext;
import com.idega.presentation.PresentationObject;
import com.idega.presentation.ui.TextInput;
import com.idega.slide.business.IWSlideService;
import com.idega.util.CoreConstants;
import com.idega.util.ListUtil;

public class RepositoryPathChooser implements ICPropertyHandler {

	public List<?> getDefaultHandlerTypes() {
		return null;
	}

	public PresentationObject getHandlerObject(String name, String stringValue, IWContext iwc, boolean oldGenerationHandler, String instanceId, String method) {
		return new TextInput();
		
		/* TODO: we should define "untouchable" folders
		IWSlideService slideService = null;
		try {
			slideService = (IWSlideService) IBOLookup.getServiceInstance(iwc, IWSlideService.class);
		} catch (IBOLookupException e) {
			e.printStackTrace();
		}
		if (slideService == null) {
			return new TextInput();
		}
		
		List<String> rootPaths = null;
		try {
			rootPaths = slideService.getChildFolderPaths(CoreConstants.PATH_FILES_ROOT);
		} catch (RemoteException e) {
			e.printStackTrace();
		}
		if (ListUtil.isEmpty(rootPaths)) {
			return new TextInput();
		}
		
		List<AdvancedProperty> allPaths = new ArrayList<AdvancedProperty>();
		allPaths = getAllPaths(slideService, rootPaths, allPaths, 0);
		if (ListUtil.isEmpty(allPaths)) {
			return new TextInput();
		}
		
		DropdownMenu repositoryPaths = new DropdownMenu();
		repositoryPaths.addFirstOption(new SelectOption(ContentUtil.getBundle().getResourceBundle(iwc).getLocalizedString("select_folder_in_repostory",
																															"Select folder in repository"), -1));
		for (AdvancedProperty path: allPaths) {
			repositoryPaths.add(new SelectOption(path.getId(), path.getValue()));
		}
		
		return repositoryPaths;*/
	}
	
	@SuppressWarnings("unchecked")
	private List<AdvancedProperty> getAllPaths(IWSlideService slideService, List<String> slidePaths, List<AdvancedProperty> allPaths, int level) {
		if (slideService == null || ListUtil.isEmpty(slidePaths)) {
			return null;
		}
		
		String value = null;
		StringBuilder name = null;
		List<AdvancedProperty> childPaths = null;
		for (String path: slidePaths) {
			value = path;
			
			if (value.startsWith(CoreConstants.WEBDAV_SERVLET_URI)) {
				value = value.replaceFirst(CoreConstants.WEBDAV_SERVLET_URI, CoreConstants.EMPTY);
			}
			
			name = new StringBuilder();
			for (int i = 0; i < level; i++) {
				name.append(CoreConstants.MINUS);
			}
			name.append(value);
			
			allPaths.add(new AdvancedProperty(name.toString(), value));
			
			childPaths = null;
			try {
				childPaths = getAllPaths(slideService, slideService.getChildFolderPaths(path), allPaths, level++);
			} catch (RemoteException e) {
				e.printStackTrace();
			}
			if (!ListUtil.isEmpty(childPaths)) {
				allPaths.addAll(childPaths);
			}
		}
		
		return allPaths;
	}

	public void onUpdate(String[] values, IWContext iwc) {
	}

}
