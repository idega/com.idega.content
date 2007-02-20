package com.idega.content.business;

import java.rmi.RemoteException;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.directwebremoting.WebContext;
import org.directwebremoting.WebContextFactory;
import org.directwebremoting.proxy.dwr.Util;

import com.idega.block.rss.business.RSSBusiness;
import com.idega.business.IBOLookup;
import com.idega.business.IBOLookupException;
import com.idega.business.IBOServiceBean;
import com.idega.content.bean.ContentItemFeedBean;
import com.idega.content.themes.helpers.ThemesHelper;
import com.idega.presentation.IWContext;
import com.idega.slide.business.IWSlideService;
import com.sun.syndication.feed.synd.SyndContent;
import com.sun.syndication.feed.synd.SyndEntry;
import com.sun.syndication.feed.synd.SyndFeed;

public class CommentsEngineBean extends IBOServiceBean implements CommentsEngine {

	private static final long serialVersionUID = 7299800648381936213L;
	
	private RSSBusiness rss = getRSSBusiness();

	public boolean addComment(String user, String subject, String body, String uri) {
		if (uri == null) {
			return false;
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
		synchronized (CommentsEngineBean.class) {
			boolean existsCommentsFile = ThemesHelper.getInstance().existFileInSlide(uri);
			
			if (existsCommentsFile) {
				if (rss != null) {
					comments = rss.getFeed(ThemesHelper.getInstance().getFullWebRoot() + uri);
				}
				else {
					return false;
				}
			}
			else {
				comments = createFeed(uri, user, subject, body, date, language, iwc);
			}
			if (!addNewEntry(comments, subject, uri, date, body, user, language)) {
				return false;
			}
			
			if (comments == null) {
				return false;
			}
			
			String commentsXml = null;
			try {
				commentsXml = rss.convertFeedToRSS2XMLString(comments);
			} catch (RemoteException e) {
				e.printStackTrace();
				return false;
			}
			
			String base = uri.substring(0, uri.lastIndexOf(ContentConstants.SLASH));
			String file = uri.substring(uri.lastIndexOf(ContentConstants.SLASH));
			IWSlideService service = ThemesHelper.getInstance().getSlideService(null);
			try {
				if (service.uploadFileAndCreateFoldersFromStringAsRoot(base, file, commentsXml, ContentConstants.XML_MIME_TYPE, true)) {				
					return true;
				}
			} catch (RemoteException e) {
				e.printStackTrace();
				return false;
			}
			return false;
		}
	}
	
	private boolean addNewEntry(SyndFeed feed, String subject, String uri, Timestamp date, String body, String user, String language) {
		if (feed == null) {
			return false;
		}
		List<SyndEntry> entries = initEntries(feed.getEntries());
		
		SyndEntry entry = rss.createNewEntry(subject, uri, date, date, "text", getShortBody(body), "text", body, user, language, null,
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
			synchronized (CommentsEngineBean.class) {
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
	
	public boolean getCommentsForAllPages(String uri) {
		WebContext wctx = WebContextFactory.get();
		Collection pages = wctx.getScriptSessionsByPage(wctx.getCurrentPage());
		
		Util utilAll = new Util(pages);
		utilAll.addFunctionCall("getComments", uri);
		
		return true;
	}
	
	public List<ContentItemComment> getComments(String uri) {
		if (uri == null) {
			return null;
		}
		if (!ThemesHelper.getInstance().existFileInSlide(uri)) {
			return null;
		}
		if (rss == null) {
			return null;
		}
		SyndFeed comments = rss.getFeed(ThemesHelper.getInstance().getFullWebRoot() + uri);
		if (comments == null) {
			return null;
		}
		List entries = comments.getEntries();
		if (entries == null) {
			return null;
		}
		List<ContentItemComment> items = new ArrayList<ContentItemComment>();
		ContentItemComment comment = null;
		Object o = null;
		SyndEntry entry = null;
		SyndContent content = null;
		for (int i = 0; i < entries.size(); i++) {
			o = entries.get(i);
			if (o instanceof SyndEntry) {
				comment = new ContentItemComment();
				entry = (SyndEntry) o;
				comment.setUser(entry.getAuthor());
				comment.setSubject(entry.getTitle());
				try {
					if (entry.getContents() != null) {
						content = (SyndContent) entry.getContents().get(0);
						comment.setComment(content.getValue());
					}
					else {
						comment.setComment(ContentConstants.EMPTY);
					}
				} catch (ClassCastException e) {
					comment.setComment(ContentConstants.EMPTY);
				} catch (IndexOutOfBoundsException e) {
					comment.setComment(ContentConstants.EMPTY);
				}
				comment.setPosted(entry.getPublishedDate().toString());
				items.add(comment);
			}
		}
		
		return items;
	}
}