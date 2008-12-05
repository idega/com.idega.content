package com.idega.content.lucid.business;

import java.rmi.RemoteException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Scope;
import org.springframework.stereotype.Service;

import com.idega.block.web2.business.Web2Business;
import com.idega.builder.bean.AdvancedProperty;
import com.idega.builder.business.BuilderLogicWrapper;
import com.idega.content.business.ContentUtil;
import com.idega.core.accesscontrol.business.StandardRoles;
import com.idega.core.builder.business.ICBuilderConstants;
import com.idega.idegaweb.IWBundle;
import com.idega.idegaweb.IWMainApplication;
import com.idega.presentation.IWContext;
import com.idega.util.CoreConstants;
import com.idega.util.CoreUtil;

@Service("lucidEngine")
@Scope("singleton")
public class LucidEngineImpl implements LucidEngine {

	private static final long serialVersionUID = 201381337142886542L;

	private static final Logger LOGGER = Logger.getLogger(LucidEngineImpl.class.getName());
	
	@Autowired
	private Web2Business web2;
	@Autowired
	private BuilderLogicWrapper builderLogic;
	
	public String getJavaScriptResources() {
		//	DWR
		StringBuilder js = new StringBuilder("/dwr/engine.js,/dwr/interface/ThemesEngine.js,/dwr/interface/BuilderService.js,/dwr/interface/LucidEngine.js,");
		
		//	MooTools
		try {
			js.append(web2.getBundleURIToMootoolsLib()).append(CoreConstants.COMMA);
		} catch (RemoteException e) {
			LOGGER.log(Level.WARNING, "Error getting URI to MooTools script", e);
		}
		try {
			js.append(web2.getBundleURIToReflectionLib()).append(CoreConstants.COMMA);
		} catch (RemoteException e) {
			LOGGER.log(Level.WARNING, "Error getting URI to Reflection script", e);
		}
		
		//	jQuery
		js.append(web2.getBundleURIToJQueryLib()).append(CoreConstants.COMMA);
		js.append(web2.getBundleUriToContextMenuScript()).append(CoreConstants.COMMA);
		
		//	Helpers
		IWBundle bundle = ContentUtil.getBundle();
		js.append(bundle.getVirtualPathWithFileNameString("javascript/drag-drop-folder-tree.js")).append(CoreConstants.COMMA);
		js.append(bundle.getVirtualPathWithFileNameString("javascript/ThemesHelper.js")).append(CoreConstants.COMMA);
		js.append(bundle.getVirtualPathWithFileNameString("javascript/ThemesManagerHelper.js")).append(CoreConstants.COMMA);
		js.append(bundle.getVirtualPathWithFileNameString("javascript/PageInfoHelper.js")).append(CoreConstants.COMMA);
		js.append(bundle.getVirtualPathWithFileNameString("javascript/tree.js")).append(CoreConstants.COMMA);
		js.append(bundle.getVirtualPathWithFileNameString("javascript/SiteManagerHelper.js"));
		
		return js.toString();
	}

	public String getStyleSheetResources() {
		return ContentUtil.getBundle().getVirtualPathWithFileNameString("style/content.css");
	}

	public Web2Business getWeb2() {
		return web2;
	}

	public void setWeb2(Web2Business web2) {
		this.web2 = web2;
	}

	public BuilderLogicWrapper getBuilderLogic() {
		return builderLogic;
	}

	public void setBuilderLogic(BuilderLogicWrapper builderLogic) {
		this.builderLogic = builderLogic;
	}

	private List<String> getThickBoxResources() {
		List<String> resources = new ArrayList<String>();
		try {
			resources.add(web2.getThickboxStyleFilePath());
		} catch (RemoteException e) {
			LOGGER.log(Level.WARNING, "Error getting URI to Thickbox style", e);
		}
		resources.add(web2.getBundleURIToJQueryLib());
		try {
			resources.add(web2.getThickboxScriptFilePath());
		} catch (RemoteException e) {
			LOGGER.log(Level.WARNING, "Error getting URI to Thickbox script", e);
		}
		return resources;
	}
	
	public List<String> getPermissionWindowResources() {
		List<String> resources = getThickBoxResources();
		
		List<AdvancedProperty> parameters = Arrays.asList(new AdvancedProperty(ICBuilderConstants.UI_COMPONENT_IS_IN_LIGHTBOX, Boolean.TRUE.toString()));
		resources.add(0, getBuilderLogic().getBuilderService(IWMainApplication.getDefaultIWApplicationContext()).getUriToPagePermissionsWindow(parameters));
		
		return resources;
	}

	public List<String> getPropertiesWindowResources() {
		List<String> resources = getThickBoxResources();
		
		List<AdvancedProperty> parameters = Arrays.asList(
				new AdvancedProperty(ICBuilderConstants.IB_CONTROL_PARAMETER, ICBuilderConstants.ACTION_EDIT),
				new AdvancedProperty(ICBuilderConstants.IC_OBJECT_INSTANCE_ID_PARAMETER, String.valueOf(-1)),
				new AdvancedProperty(ICBuilderConstants.UI_COMPONENT_IS_IN_LIGHTBOX, Boolean.TRUE.toString())
		);
		resources.add(0, getBuilderLogic().getBuilderService(IWMainApplication.getDefaultIWApplicationContext()).getUriToPagePropertiesWindow(parameters));
		
		return resources;
	}

	public boolean isContentEditor() {
		IWContext iwc = CoreUtil.getIWContext();
		if (iwc == null) {
			return false;
		}
		
		try {
			if (!iwc.isLoggedOn()) {
				return false;
			}
		} catch(Exception e) {
			LOGGER.log(Level.WARNING, "Error while determining if user is logged", e);
			return false;
		}
		
		return iwc.hasRole(StandardRoles.ROLE_KEY_EDITOR);
	}
}
