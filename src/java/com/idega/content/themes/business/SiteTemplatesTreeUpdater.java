package com.idega.content.themes.business;

import org.directwebremoting.ScriptBuffer;
import org.jdom.Document;

import com.idega.content.themes.presentation.TemplatesTree;
import com.idega.core.builder.business.BuilderService;
import com.idega.presentation.IWContext;
import com.idega.util.CoreConstants;

public class SiteTemplatesTreeUpdater extends TreeUpdater implements Runnable {

	public SiteTemplatesTreeUpdater(IWContext iwc, boolean sendToAllSessions) {
		super(iwc, sendToAllSessions);
	}
	
	public void run() {
		if (getIWContext() == null) {
			return;
		}
		
		BuilderService service = getBuilderService();
		if (service == null) {
			return;
		}
		
		Document renderedTree = service.getRenderedComponent(getIWContext(), new TemplatesTree(), false);
		if (renderedTree == null) {
			return;
		}
		
		StringBuffer uri = new StringBuffer(CoreConstants.SLASH).append(CoreConstants.WORKSPACE_VIEW_MANAGER_ID).append(CoreConstants.SLASH);
		uri.append(CoreConstants.CONTENT_VIEW_MANAGER_ID).append(CoreConstants.SLASH).append(CoreConstants.PAGES_VIEW_MANAGER_ID).append(CoreConstants.SLASH);
		sendScript(renderedTree, new ScriptBuffer("updateSiteTemplatesTree(").appendData(renderedTree).appendScript(");"), uri.toString());
	}

}
