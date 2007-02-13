package com.idega.content.business;

import java.rmi.RemoteException;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.List;

import com.idega.block.rss.business.RSSBusiness;
import com.idega.business.IBOLookup;
import com.idega.business.IBOLookupException;
import com.idega.business.IBOServiceBean;
import com.idega.content.bean.ContentItemFeedBean;
import com.idega.content.themes.helpers.ThemesHelper;
import com.idega.presentation.IWContext;
import com.idega.slide.business.IWSlideService;
import com.sun.syndication.feed.synd.SyndEntry;
import com.sun.syndication.feed.synd.SyndFeed;

public class ContentItemCommentsEngineBean extends IBOServiceBean implements ContentItemCommentsEngine {

	private static final long serialVersionUID = 7299800648381936213L;
	
	private RSSBusiness rss = getRSSBusiness();

	public String addComment(String user, String subject, String body, String uri, boolean existsCommentsFile) {
		if (uri == null) {
			return null;
		}
		
		IWContext iwc = ThemesHelper.getInstance().getIWContext();
		
		String language = "en";
		if (iwc != null) {
			if (iwc.getLocale() != null) {
				language = iwc.getLocale().getLanguage();
			}
		}
		
		Timestamp date = new Timestamp(System.currentTimeMillis());
		
		SyndFeed comments = null;
		if (existsCommentsFile) {
			if (rss != null) {
				comments = rss.getFeed(ThemesHelper.getInstance().getFullWebRoot() + uri);
			}
			else {
				return null;
			}
		}
		else {
			comments = createFeed(uri, user, subject, body, date, language, iwc);
		}
		if (!addNewEntry(comments, subject, uri, date, body, user, language)) {
			return null;
		}
		
		if (comments == null) {
			return null;
		}
		
		String commentsXml = null;
		try {
			commentsXml = rss.convertFeedToRSS2XMLString(comments);
		} catch (RemoteException e) {
			e.printStackTrace();
			return null;
		}
		
		String base = uri.substring(0, uri.lastIndexOf(ContentConstants.SLASH));
		String file = uri.substring(uri.lastIndexOf(ContentConstants.SLASH));
		IWSlideService service = ThemesHelper.getInstance().getSlideService(null);
		try {
			if (service.uploadFileAndCreateFoldersFromStringAsRoot(base, file, commentsXml, ContentConstants.XML_MIME_TYPE, true)) {
				return date.toString();
			}
		} catch (RemoteException e) {
			e.printStackTrace();
			return null;
		}
		return null;
	}
	
	private boolean addNewEntry(SyndFeed feed, String subject, String uri, Timestamp date, String body, String user, String language) {
		if (feed == null) {
			return false;
		}
		List<SyndEntry> entries = initEntries(feed.getEntries());
		
		SyndEntry entry = rss.createNewEntry(subject, uri, date, null, "text", getShortBody(body), "text", body, user, language, null,
				null, null, null);
		entries.add(entry);
		feed.setEntries(entries);
		return true;
	}
	
	private List<SyndEntry> initEntries(List oldEntries) {
		if (oldEntries == null) {
			return new ArrayList<SyndEntry>();
		}
		if (oldEntries.size() == 0) {
			return new ArrayList<SyndEntry>();
		}
		List<SyndEntry> entries = new ArrayList<SyndEntry>();
		Object o = null;
		for (int i = 0; i < oldEntries.size(); i++) {
			o = oldEntries.get(i);
			if (o instanceof SyndEntry) {
				entries.add((SyndEntry) o);
			}
		}
		return entries;
	}
	
	private SyndFeed createFeed(String uri, String user, String subject, String body, Timestamp date, String language,
			IWContext iwc) {
		if (rss == null) {
			return null;
		}

		String serverName = ThemesHelper.getInstance().getFullServerName(iwc);
		
		SyndFeed comments = rss.createNewFeed("Comments", serverName, "All comments", ContentItemFeedBean.FEED_TYPE_RSS_2, language,
				date);
		
		return comments;
	}
	
	private String getShortBody(String body) {
		if (body == null) {
			return ContentConstants.EMPTY;
		}
		if (body.length() >= 200) {
			StringBuffer shortBody = new StringBuffer(body.substring(0, 200)).append(ContentConstants.DOT);
			shortBody.append(ContentConstants.DOT).append(ContentConstants.DOT);
			return shortBody.toString();
		}
		return body;
	}
	
	private RSSBusiness getRSSBusiness() {
		if (rss == null) {
			synchronized (ContentItemCommentsEngineBean.class) {
				if (rss == null) {
					try {
						rss = (RSSBusiness) IBOLookup.getServiceInstance(IWContext.getInstance(), RSSBusiness.class);
					} catch (IBOLookupException e) {
						e.printStackTrace();
					}
				}
			}
		}
		return rss;
	}
}