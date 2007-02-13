package com.idega.content.bean;

import java.io.Serializable;
import java.rmi.RemoteException;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.List;

import org.jdom.Document;

import com.idega.block.rss.business.RSSBusiness;
import com.idega.business.IBOLookup;
import com.idega.business.IBOLookupException;
import com.idega.presentation.IWContext;
import com.sun.syndication.feed.synd.SyndEntry;
import com.sun.syndication.feed.synd.SyndFeed;

public class ContentItemFeedBean implements Serializable {

	private static final long serialVersionUID = 5244185733994532891L;
	
	public static final String FEED_TYPE_ATOM_1 = "atom_1.0";
	public static final String FEED_TYPE_RSS_2 = "rss_2.0";
	private static final String FEED_ENTRY_DESCRIPTION_TYPE = "html";
	private static final String FEED_ENTRY_BODY_TYPE = "html";
	
	private volatile RSSBusiness rss = null;
	
	private String feedType = null;
	
	public ContentItemFeedBean(IWContext iwc, String feedType) {
		this.feedType = feedType;
		init(iwc);
	}
	
	private void init(IWContext iwc) {
		if (rss == null) {
			synchronized (ContentItemFeedBean.class) {
				if (rss == null) {
					if (iwc == null) {
						iwc = IWContext.getInstance();
					}
					try {
						rss = (RSSBusiness) IBOLookup.getServiceInstance(iwc, RSSBusiness.class);
					} catch (IBOLookupException e) {
						e.printStackTrace();
					}
				}
			}
		}
	}
	
	/**
	 * 
	 * @param title
	 * @param uri
	 * @param description
	 * @return
	 */
	private SyndFeed createFeed(String title, String uri, String description, String language, Timestamp date) {
		if (rss == null) {
			return null;
		}
		return rss.createNewFeed(title, uri, description, getFeedType(), language, date);
	}
	
	/**
	 * 
	 * @param feedTitle
	 * @param url
	 * @param feedDescription
	 * @param title
	 * @param date
	 * @param description
	 * @param author
	 * @param language
	 * @param categories
	 * @param serverName
	 * @param updated
	 * @param published
	 * @param body
	 * @param source
	 * @param comment
	 * @param linkToComments
	 * @return
	 */
	private SyndFeed createFeedWithEntry(String feedTitle, String serverName, String feedDescription, String title, Timestamp updated,
			Timestamp published, String description, String body, String author, String language, List<String> categories,
			String url, String source, String comment, String linkToComments) {
		if (rss == null) {
			return null;
		}
		
		SyndFeed feed = createFeed(feedTitle, serverName, feedDescription, language, updated);
		if (feed == null) {
			return null;
		}
		
		List<SyndEntry> entries = new ArrayList<SyndEntry>();
		entries.add(rss.createNewEntry(title, url, updated, published, FEED_ENTRY_DESCRIPTION_TYPE, description, FEED_ENTRY_BODY_TYPE,
				body, author, language, categories, source, comment, linkToComments));
		feed.setEntries(entries);
		
		return feed;
	}
	
	/**
	 * 
	 * @param feedTitle
	 * @param feedDescription
	 * @param title
	 * @param description
	 * @param author
	 * @param language
	 * @param categories
	 * @param url
	 * @param updated
	 * @param published
	 * @param body
	 * @param serverName
	 * @param source
	 * @param comment
	 * @param linkToComments
	 * @return
	 */
	public String getFeedEntryAsXML(String feedTitle, String serverName, String feedDescription, String title, Timestamp updated,
			Timestamp published, String description, String body, String author, String language, List<String> categories,
			String url, String source, String comment, String linkToComments) {
		if (rss == null) {
			return null;
		}
		if (updated == null) {
			updated = new Timestamp(System.currentTimeMillis());
		}

		SyndFeed feed = createFeedWithEntry(feedTitle, serverName, feedDescription, title, updated, published, description, body,
				author, language, categories, url, source, comment, linkToComments);
		if (feed == null) {
			return null;
		}
		if (FEED_TYPE_ATOM_1.equals(feed.getFeedType())) {
			try {
				return rss.convertFeedToAtomXMLString(feed);
			} catch (RemoteException e) {
				e.printStackTrace();
				return null;
			}
		}
		if (FEED_TYPE_RSS_2.equals(feed.getFeedType())) {
			try {
				return rss.convertFeedToRSS2XMLString(feed);
			} catch (RemoteException e) {
				e.printStackTrace();
				return null;
			}
		}
		
		return null;
	}
	
	protected Document getFeedAsJDomDocument(SyndFeed feed) {
		if (rss == null) {
			return null;
		}
		return rss.convertFeedToJDomDocument(feed);
	}

	protected String getFeedType() {
		return feedType;
	}

	protected void setFeedType(String feedType) {
		this.feedType = feedType;
	}

}
