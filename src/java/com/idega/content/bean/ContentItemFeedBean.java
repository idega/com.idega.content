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
	private static final String FEED_ENTRY_DESCRIPTION_TYPE = "text/html";
	
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
	protected SyndFeed createFeed(String title, String uri, String description) {
		if (rss == null) {
			return null;
		}
		return rss.createNewFeed(title, uri, description, getFeedType());
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
	 * @return
	 */
	public SyndFeed createFeedWithEntry(String feedTitle, String url, String feedDescription, String title, Timestamp date,
			String description, String author, String language, List<String> categories) {
		if (rss == null) {
			return null;
		}
		
		SyndFeed feed = createFeed(feedTitle, url, feedDescription);
		if (feed == null) {
			return null;
		}
		
		List<SyndEntry> entries = new ArrayList<SyndEntry>();
		entries.add(rss.createNewEntry(title, url, date, FEED_ENTRY_DESCRIPTION_TYPE, description, author, language, categories));
		feed.setEntries(entries);
		
		return feed;
	}
	
	/**
	 * 
	 * @param feedTitle
	 * @param uri
	 * @param feedDescription
	 * @param title
	 * @param date
	 * @param description
	 * @param author
	 * @param language
	 * @param categories
	 * @return
	 */
	public String getFeedEntryAsXML(String feedTitle, String url, String feedDescription, String title, Timestamp date,
			String description, String author, String language, List<String> categories) {
		if (rss == null) {
			return null;
		}
		SyndFeed feed = createFeedWithEntry(feedTitle, url, feedDescription, title, date, description, author, language, categories);
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
			return rss.convertFeedToRSS2XMLString(feed);
		}
		
		return null;
	}
	
	public Document getFeedAsJDomDocument(SyndFeed feed) {
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
