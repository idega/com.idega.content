package com.idega.content.themes.business;

import org.apache.myfaces.custom.tree2.TreeNode;
import org.directwebremoting.ScriptBuffer;
import org.jdom.Document;

import com.idega.content.themes.presentation.SiteTreeViewer;
import com.idega.core.builder.business.BuilderService;
import com.idega.presentation.IWContext;
import com.idega.webface.WFUtil;

public class SitePagesTreeUpdater extends TreeUpdater implements Runnable {
	
	public SitePagesTreeUpdater(IWContext iwc, boolean updateAllSessions) {
		super(iwc, updateAllSessions);
	}

	public void run() {
		if (getIWContext() == null) {
			return;
		}
		
		SiteTreeViewer tree = new SiteTreeViewer();
		Object o = WFUtil.getValue("pageCreationBean", "pageSelectorTopNode");
		if (o instanceof TreeNode) {
			tree.setRootNode((TreeNode) o);
		}
		else {
			return;
		}
		
		BuilderService service = getBuilderService();
		if (service == null) {
			return;
		}
		
		Document rendered = service.getRenderedComponent(getIWContext(), tree, true);
		if (rendered == null) {
			return;
		}
		
		sendScript(rendered, new ScriptBuffer("updateSiteTree(").appendData(rendered).appendScript(");"), null);
	}

}
