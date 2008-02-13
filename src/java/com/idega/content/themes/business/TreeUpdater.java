package com.idega.content.themes.business;

import java.rmi.RemoteException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;

import org.directwebremoting.ScriptBuffer;
import org.directwebremoting.ScriptSession;
import org.directwebremoting.WebContext;
import org.directwebremoting.WebContextFactory;
import org.jdom.Document;

import com.idega.core.builder.business.BuilderService;
import com.idega.core.builder.business.BuilderServiceFactory;
import com.idega.presentation.IWContext;

public class TreeUpdater {
	
	private IWContext iwc = null;
	private boolean sendToAllSessions = false;
	
	public TreeUpdater(IWContext iwc, boolean sendToAllSessions) {
		this.iwc = iwc;
		this.sendToAllSessions = sendToAllSessions;
	}

	protected void sendScript(Document renderedObject, ScriptBuffer script, String uri) {
		if (renderedObject == null) {
			return;
		}
		
		ScriptSession ss = WebContextFactory.get().getScriptSession();
		
		List<ScriptSession> allPages = getAllCurrentPageSessions(uri);
		if (allPages == null) {
			return;
		}
		
		ScriptSession session = null;
		for (int i = 0; i < allPages.size(); i++) {
			session = allPages.get(i);
			if (sendToAllSessions) {
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
	private List<ScriptSession> getAllCurrentPageSessions(String uri) {
		WebContext wctx = WebContextFactory.get();
		if (wctx == null) {
			return null;
		}
		
		Collection currentPageSessions = null;
		String currentPage = wctx.getCurrentPage();
		if (currentPage != null) {
			currentPageSessions = wctx.getScriptSessionsByPage(currentPage);
		}
		if (currentPageSessions == null) {
			currentPageSessions = new ArrayList();
		}
		
		if (uri != null) {
			//	Looking for sessions in custom uri
			Collection pages = wctx.getScriptSessionsByPage(uri);
			if (pages != null) {
				currentPageSessions.addAll(pages);
			}
		}
		
		List<ScriptSession> allPages = new ArrayList<ScriptSession>();
		Object o = null;
		for (Iterator it = currentPageSessions.iterator(); it.hasNext(); ) {
			o = it.next();
			if (o instanceof ScriptSession) {
	            allPages.add((ScriptSession) o);
	        }
		}
		
		return allPages;
	}
	
	protected BuilderService getBuilderService() {
		try {
			return BuilderServiceFactory.getBuilderService(iwc);
		} catch (RemoteException e) {
			e.printStackTrace();
		}
		
		return null;
	}

	protected IWContext getIWContext() {
		return iwc;
	}

	protected boolean isSendToAllSessions() {
		return sendToAllSessions;
	}

}
