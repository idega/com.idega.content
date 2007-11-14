package com.idega.content.themes.business;

import java.rmi.RemoteException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;

import org.apache.myfaces.custom.tree2.TreeNode;
import org.directwebremoting.ScriptBuffer;
import org.directwebremoting.ScriptSession;
import org.directwebremoting.WebContext;
import org.directwebremoting.WebContextFactory;
import org.jdom.Document;

import com.idega.content.themes.presentation.SiteTreeViewer;
import com.idega.core.builder.business.BuilderService;
import com.idega.core.builder.business.BuilderServiceFactory;
import com.idega.presentation.IWContext;
import com.idega.webface.WFUtil;

public class SiteTreeUpdater implements Runnable {
	
	private IWContext iwc = null;
	private boolean updateAllSessions = false;
	
	public SiteTreeUpdater(IWContext iwc, boolean updateAllSessions) {
		this.iwc = iwc;
		this.updateAllSessions = updateAllSessions;
	}

	public void run() {
		if (iwc == null) {
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
		
		BuilderService service = null;
		try {
			service = BuilderServiceFactory.getBuilderService(iwc);
		} catch (RemoteException e) {
			e.printStackTrace();
		}
		if (service == null) {
			return;
		}
		
		Document rendered = service.getRenderedComponent(iwc, tree, true);
		if (rendered == null) {
			return;
		}
		
		ScriptSession ss = WebContextFactory.get().getScriptSession();
		
		List<ScriptSession> allPages = getAllCurrentPageSessions();
		if (allPages == null) {
			return;
		}
		
		ScriptBuffer script = new ScriptBuffer("updateSiteTree(").appendData(rendered).appendScript(");");
		ScriptSession session = null;
		for (int i = 0; i < allPages.size(); i++) {
			session = allPages.get(i);
			if (updateAllSessions) {
				session.addScript(script);
			}
			else {
				if (ss != null && !session.equals(ss)) {
					session.addScript(script);
				}
			}
		}
	}
	
	@SuppressWarnings("unchecked")
	private List<ScriptSession> getAllCurrentPageSessions() {
		WebContext wctx = WebContextFactory.get();
		if (wctx == null) {
			return null;
		}
		Collection pages = wctx.getScriptSessionsByPage(wctx.getCurrentPage());
		if (pages == null) {
			return null;
		}
		
		List<ScriptSession> allPages = new ArrayList<ScriptSession>();
		Object o = null;
		for (Iterator it = pages.iterator(); it.hasNext(); ) {
			o = it.next();
			if (o instanceof ScriptSession) {
	            allPages.add((ScriptSession) o);
	        }
		}
		
		return allPages;
	}

}
