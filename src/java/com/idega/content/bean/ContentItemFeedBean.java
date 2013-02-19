package com.idega.content.bean;

import java.io.Serializable;
import java.rmi.RemoteException;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.List;

import org.jdom2.Document;

import com.idega.block.rss.business.EntryData;
import com.idega.block.rss.business.RSSBusiness;
import com.idega.business.IBOLookup;
import com.idega.business.IBOLookupException;
import com.idega.presentation.IWContext;
import com.idega.util.CoreUtil;
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

	public ContentItemFeedBean(RSSBusiness rss, String feedType) {
		this.feedType = feedType;
		this.rss=rss;
	}

	private void init(IWContext iwc) {
		if (rss == null) {
			if (iwc == null) {
				iwc = CoreUtil.getIWContext();
			}
			try {
				rss = IBOLookup.getServiceInstance(iwc, RSSBusiness.class);
			} catch (IBOLookupException e) {
				e.printStackTrace();
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

	private SyndFeed createFeedWithEntry(String feedTitle, String serverName, String feedDescription, EntryData entryData) {
		if (rss == null) {
			return null;
		}

		SyndFeed feed = createFeed(feedTitle, serverName, feedDescription,
				entryData.getLanguage(), entryData.getUpdated());
		if (feed == null) {
			return null;
		}
		List<SyndEntry> entries = new ArrayList<SyndEntry>();
		entryData.setDescriptionType(FEED_ENTRY_DESCRIPTION_TYPE);
		entryData.setBodyType(FEED_ENTRY_BODY_TYPE);
		entries.add(rss.createNewEntry(entryData));
		feed.setEntries(entries);

		return feed;
	}

	public String getFeedEntryAsXML(String feedTitle, String serverName, String feedDescription, EntryData entryData) {
		if (rss == null) {
			return null;
		}
		if (entryData.getUpdated() == null) {
			entryData.setUpdated(new Timestamp(System.currentTimeMillis()));
		}

		SyndFeed feed = createFeedWithEntry(feedTitle, serverName, feedDescription, entryData);
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
