package com.idega.content.business;

import java.io.IOException;
import java.rmi.RemoteException;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.directwebremoting.WebContext;
import org.directwebremoting.WebContextFactory;
import org.directwebremoting.proxy.dwr.Util;

import sun.misc.BASE64Decoder;
import sun.misc.BASE64Encoder;

import com.idega.block.rss.business.RSSBusiness;
import com.idega.business.IBOLookup;
import com.idega.business.IBOLookupException;
import com.idega.business.IBOServiceBean;
import com.idega.content.bean.ContentItemFeedBean;
import com.idega.content.themes.helpers.ThemesHelper;
import com.idega.core.cache.IWCacheManager2;
import com.idega.presentation.IWContext;
import com.idega.slide.business.IWSlideService;
import com.sun.syndication.feed.atom.Content;
import com.sun.syndication.feed.atom.Entry;
import com.sun.syndication.feed.atom.Feed;
import com.sun.syndication.feed.atom.Link;
import com.sun.syndication.feed.atom.Person;
import com.sun.syndication.feed.synd.SyndFeed;
import com.sun.syndication.io.FeedException;
import com.sun.syndication.io.WireFeedOutput;

public class CommentsEngineBean extends IBOServiceBean implements CommentsEngine {

	private static final long serialVersionUID = 7299800648381936213L;
	
	private static final String COMMENTS_CACHE_NAME = "article_comments_feeds_cache";
	
	private RSSBusiness rss = getRSSBusiness();
	private WireFeedOutput wfo = new WireFeedOutput();
	private BASE64Encoder encoder = new BASE64Encoder();
	private BASE64Decoder decoder = new BASE64Decoder();
	
	private String newComment = ContentUtil.getBundle().getLocalizedString("new_comment");
	private String newCommentMessage = ContentUtil.getBundle().getLocalizedString("new_comment_message");
	
	private List<String> parsedEmails = new ArrayList<String>();

	public boolean addComment(String cacheKey, String user, String subject, String email, String body, String uri, boolean notify) {
		if (uri == null) {
			return false;
		}
		if (ContentConstants.EMPTY.equals(uri)) {
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
		
		Feed comments = null;
		synchronized (CommentsEngineBean.class) {
			comments = getCommentsFeed(uri, iwc);
			if (comments == null) {
				comments = createFeed(uri, user, subject, body, date, language, iwc);
			}
			if (comments == null) {
				return false;
			}
			
			if (!addNewEntry(comments, subject, uri, date, body, user, language, email, notify)) {
				return false;
			}
			
			putFeedToCache(comments, uri, iwc);
			removeArticleFromCache(iwc, cacheKey);
			
			String commentsXml = null;
			try {
				commentsXml = wfo.outputString(comments);
			} catch (IllegalArgumentException e1) {
				e1.printStackTrace();
				return false;
			} catch (FeedException e1) {
				e1.printStackTrace();
				return false;
			}
			
			sendNotification(comments, email, iwc);
			
			String base = uri;
			String file = uri;
			int index = uri.lastIndexOf(ContentConstants.SLASH);
			if (index != -1) {
				base = uri.substring(0, index);
				file = uri.substring(index);
			}
			IWSlideService service = ThemesHelper.getInstance().getSlideService(iwc);
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
	
	private boolean removeArticleFromCache(IWContext iwc, String cacheKey) {
		if (cacheKey == null) {
			return false;
		}
		if (iwc == null) {
			iwc = ThemesHelper.getInstance().getIWContext();
		}
		if (iwc == null) {
			return false;
		}
		IWCacheManager2 cache = IWCacheManager2.getInstance(iwc.getIWMainApplication());
		if (cache == null) {
			return false;
		}
		Map articles = cache.getCache("article");
		if (articles == null) {
			return false;
		}
		if (articles.containsKey(cacheKey)) {
			articles.remove(cacheKey);
		}
		else {
			return false;
		}
		
		return true;
	}
	
	private boolean sendNotification(Feed comments, String email, IWContext iwc) {
		List<String> emails = getEmails(comments, email);
		StringBuffer body = new StringBuffer(newCommentMessage);
		WebContext wctx = WebContextFactory.get();
		body.append(ThemesHelper.getInstance().getFullServerName(iwc)).append(wctx.getCurrentPage());
		// TODO: remove testing info
		String host = iwc.getApplicationSettings().getProperty("messagebox_smtp_mailserver");
		if (host == null) {
			host = "mail.simnet.is";
		}
		String from = iwc.getApplicationSettings().getProperty("messagebox_from_mailaddress");
		if (from == null) {
			from = "testing@formbuilder.idega.is";
		}
		Thread sender = new Thread(new CommentsNotificationSender(emails, from, newComment,	body.toString(), host));
		sender.start();
		return true;
	}
	
	@SuppressWarnings("unchecked")
	private List<String> getEmails(Feed comments, String email) {
		List<String> emails = new ArrayList<String>();
		if (comments == null) {
			return emails;
		}
		List<Entry> entries = comments.getEntries();
		if (entries == null) {
			return emails;
		}
		Entry entry = null;
		Object o = null;
		Object oo = null;
		List<Person> authors = null;
		Person author = null;
		String mail = null;
		parsedEmails = new ArrayList<String>();
		for (int i = 0; i < entries.size(); i++) {
			o = entries.get(i);
			if (o instanceof Entry) {
				entry = (Entry) o;
				authors = entry.getAuthors();
				if (authors != null) {
					for (int j = 0; j < authors.size(); j++) {
						oo = authors.get(j);
						if (oo instanceof Person) {
							author = (Person) oo;
							mail = author.getEmail();
							if (mail != null) {
								mail = decodeMail(mail);
								if (!mail.equals(email)) {
									if (!parsedEmails.contains(mail)) {
										parsedEmails.add(mail);
										emails.add(mail);
									}
								}
							}
						}
					}
				}
			}
		}
		return emails;
	}
	
	private boolean addNewEntry(Feed feed, String subject, String uri, Timestamp date, String body, String user, String language,
			String email, boolean notify) {
		if (feed == null) {
			return false;
		}
		
		List<Entry> entries = initEntries(feed.getEntries());
		Entry entry = new Entry();
		
		// Title
		entry.setTitle(subject);
		
		// Summary
		Content summary = new Content();
		summary.setType("html");
		summary.setValue(getShortBody(body));
		entry.setSummary(summary);
		
		// Body of comment
		Content comment = new Content();
		comment.setType("html");
		comment.setValue(body);
		List<Content> comments = new ArrayList<Content>();
		comments.add(comment);
		entry.setContents(comments);
		
		// Dates
		entry.setUpdated(date);
		entry.setCreated(date);
		entry.setPublished(date);
		entry.setModified(date);
		
		// Author & Email
		Person author = new Person();
		author.setName(user);
		if (notify) {
			author.setEmail(encodeMail(email));
		}
		List<Person> authors = new ArrayList<Person>();
		authors.add(author);
		entry.setAuthors(authors);
		
		// ID
		entry.setId(ThemesHelper.getInstance().getUniqueIdByNumberAndDate(ContentConstants.COMMENT_SCOPE));
		
		// URI
		Link link = new Link();
		link.setHref(uri);
		List<Link> links = new ArrayList<Link>();
		links.add(link);
		entry.setAlternateLinks(links);
		
		entries.add(entry);
		feed.setEntries(entries);
		return true;
	}
	
	private List<Entry> initEntries(List oldEntries) {
		if (oldEntries == null) {
			return new ArrayList<Entry>();
		}
		if (oldEntries.size() == 0) {
			return new ArrayList<Entry>();
		}
		List<Entry> entries = new ArrayList<Entry>();
		Object o = null;
		for (int i = 0; i < oldEntries.size(); i++) {
			o = oldEntries.get(i);
			if (o instanceof Entry) {
				entries.add((Entry) o);
			}
		}
		return entries;
	}
	
	private Feed createFeed(String uri, String user, String subject, String body, Timestamp date, String language,
			IWContext iwc) {

		String serverName = ThemesHelper.getInstance().getFullServerName(iwc);
		
		Feed comments = new Feed();
		comments.setFeedType(ContentItemFeedBean.FEED_TYPE_ATOM_1);
		
		// Title
		comments.setTitle("Comments of Article");
		
		// Subtitle
		Content subtitle = new Content();
		subtitle.setValue("All comments");
		comments.setSubtitle(subtitle);
		
		// Language
		comments.setLanguage(language);
		
		// Dates
		comments.setModified(date);
		comments.setUpdated(date);
		
		// ID
		comments.setId(serverName + ContentConstants.CONTENT + uri);
		
		// Author
		Person author = new Person();
		author.setName(user);
		List<Person> authors = new ArrayList<Person>();
		authors.add(author);
		comments.setAuthors(authors);
		
		// Link
		Link link = new Link();
		link.setHref(serverName);
		List<Link> links = new ArrayList<Link>();
		links.add(link);
		comments.setAlternateLinks(links);
		putFeedToCache(comments, uri, iwc);
		
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
		Feed comments = getCommentsFeed(uri, null);
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
		Entry entry = null;
		Content content = null;
		Person author = null;
		for (int i = 0; i < entries.size(); i++) {
			o = entries.get(i);
			if (o instanceof Entry) {
				comment = new ContentItemComment();
				entry = (Entry) o;
				
				// ID
				comment.setId(entry.getId());

				// Author
				try {
					if (entry.getAuthors() != null) {
						author = (Person) entry.getAuthors().get(0);
						comment.setUser(author.getName());
					}
					else {
						comment.setUser(ContentConstants.EMPTY);
					}
				} catch(ClassCastException e) {
					comment.setUser(ContentConstants.EMPTY);
				} catch (IndexOutOfBoundsException e) {
					comment.setUser(ContentConstants.EMPTY);
				}
				
				// Subject
				comment.setSubject(entry.getTitle());
				
				// Content
				try {
					if (entry.getContents() != null) {
						content = (Content) entry.getContents().get(0);
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
				
				// Date of creation
				comment.setPosted(entry.getPublished().toString());
				items.add(comment);
			}
		}
		
		return items;
	}
	
	public int getCommentsCount(String uri) {
		Feed comments = getCommentsFeed(uri, null);
		if (comments == null) {
			return 0;
		}
		if (comments.getEntries() == null) {
			return 0;
		}
		return comments.getEntries().size();
	}
	
	private Feed getCommentsFeed(String uri, IWContext iwc) {
		Feed cachedFeed = getFeedFromCache(uri, iwc);
		if (cachedFeed != null) {
			return cachedFeed;
		}
		
		if (uri == null) {
			return null;
		}
		ThemesHelper helper = ThemesHelper.getInstance();
		if (!helper.existFileInSlide(uri)) {
			return null;
		}
		
		if (rss == null) {
			return null;
		}
		SyndFeed comments = rss.getFeed(helper.getFullWebRoot() + uri);
		Feed realFeed = (Feed) comments.createWireFeed();
		putFeedToCache(realFeed, uri, iwc);
		
		return realFeed;
	}
	
	@SuppressWarnings("unchecked")
	private void putFeedToCache(Feed comments, String uri, IWContext iwc) {
		if (comments == null || uri == null) {
			return;
		}
		if (iwc == null) {
			iwc = ThemesHelper.getInstance().getIWContext();
			if (iwc == null) {
				return;
			}
		}
		IWCacheManager2 cache = IWCacheManager2.getInstance(iwc.getIWMainApplication());
		if (cache == null) {
			return;
		}
		Map<String, Feed> commentsMap = cache.getCache(COMMENTS_CACHE_NAME);
		if (commentsMap == null) {
			commentsMap = new HashMap<String, Feed>();
		}
		commentsMap.put(uri, comments);
	}
	
	private Feed getFeedFromCache(String uri, IWContext iwc) {
		if (uri == null) {
			return null;
		}
		if (iwc == null) {
			iwc = ThemesHelper.getInstance().getIWContext();
			if (iwc == null) {
				return null;
			}
		}
		IWCacheManager2 cache = IWCacheManager2.getInstance(iwc.getIWMainApplication());
		if (cache == null) {
			return null;
		}
		Map comments = cache.getCache(COMMENTS_CACHE_NAME);
		if (comments == null) {
			return null;
		}
		Object o = comments.get(uri);
		if (o == null) {
			return null;
		}
		try {
			return (Feed) o;
		} catch (ClassCastException e) {
			e.printStackTrace();
			return null;
		}
	}
	
	private String encodeMail(String email) {
		if (email == null) {
			return email;
		}
		return encoder.encode(email.getBytes());
	}
	
	private String decodeMail(String email) {
		if (email == null) {
			return email;
		}
		try {
			return new String(decoder.decodeBuffer(email));
		} catch (IOException e) {
			e.printStackTrace();
			return null;
		}
	}
}