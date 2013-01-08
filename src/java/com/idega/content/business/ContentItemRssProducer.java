package com.idega.content.business;

import java.io.IOException;
import java.rmi.RemoteException;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.Iterator;
import java.util.List;
import java.util.logging.Logger;

import javax.jcr.RepositoryException;
import javax.servlet.ServletException;

import org.apache.commons.httpclient.HttpException;

import com.idega.block.rss.business.RSSAbstractProducer;
import com.idega.block.rss.business.RSSBusiness;
import com.idega.block.rss.business.RSSProducer;
import com.idega.block.rss.data.RSSRequest;
import com.idega.business.IBOLookup;
import com.idega.business.IBOLookupException;
import com.idega.content.themes.helpers.business.ThemesHelper;
import com.idega.core.search.business.SearchResult;
import com.idega.idegaweb.IWMainApplication;
import com.idega.presentation.IWContext;
import com.idega.repository.bean.RepositoryItem;
import com.idega.util.CoreConstants;
import com.idega.util.CoreUtil;
import com.idega.util.FileUtil;
import com.idega.util.expression.ELUtil;
import com.sun.syndication.feed.synd.SyndContent;
import com.sun.syndication.feed.synd.SyndContentImpl;
import com.sun.syndication.feed.synd.SyndEntry;
import com.sun.syndication.feed.synd.SyndEntryImpl;
import com.sun.syndication.feed.synd.SyndFeed;
import com.sun.syndication.feed.synd.SyndFeedImpl;

public class ContentItemRssProducer extends RSSAbstractProducer implements RSSProducer {

	protected static final String ARTICLE_SEARCH_KEY = "*.xml*";
	public static final String RSS_FOLDER_NAME = "rss";
	public static final String RSS_FILE_NAME = "feed.xml";

	public static final String PATH = CoreConstants.WEBDAV_SERVLET_URI + ContentUtil.getContentBaseFolderPath() + "/article/";
	private static Logger LOGGER = Logger.getLogger(ContentItemRssProducer.class.getName());

	public ContentItemRssProducer() {
		super();
	}

	@Override
	public void handleRSSRequest(RSSRequest rssRequest) throws IOException {
		String uri = fixURI(rssRequest);

		if(this.isAFolderInSlide(uri,rssRequest)){
			try {
				dispatch(rssRequest.getURI(), rssRequest);
			} catch (ServletException e) {
				e.printStackTrace();
			}
		}
		else{
			//create a file and dispatch the request to it
			searchForArticles();
			try {
				dispatch(rssRequest.getURI(), rssRequest);
			} catch (ServletException e) {
				e.printStackTrace();
			}

		}
	}
	public void searchForArticles() {
		IWContext iwc = CoreUtil.getIWContext();
		if (iwc == null) {
			LOGGER.warning("Unable to search for articles, IWContext is null");
			return;
		}

		ContentSearch search = new ContentSearch(IWMainApplication.getDefaultIWMainApplication());

		Collection<SearchResult> results = search.doSimpleDASLSearch(ARTICLE_SEARCH_KEY, PATH);

		if (results == null) {
			LOGGER.warning("ContentSearch.doSimpleDASLSearch returned results Collection, which is null: " + results);
			return;
		}
		List<String> urisToArticles = new ArrayList<String>();
		String uri = null;
		for (SearchResult result: results) {
			uri = result.getSearchResultURI();
			urisToArticles.add(uri);
		}

		RSSBusiness rss = null;
		SyndFeed articleFeed = null;
		Date now = new Date();
		long time = now.getTime();
		try {
			rss = IBOLookup.getServiceInstance(IWMainApplication.getDefaultIWApplicationContext(), RSSBusiness.class);
		} catch (IBOLookupException e1) {
			e1.printStackTrace();
		}
		SyndFeed allArticles = rss.createNewFeed("title", getThemesHelper().getWebRootWithoutContent(), "description", "atom_1.0", "language",
				new Timestamp(time));

		List<SyndEntry> allEntries = new ArrayList<SyndEntry>();
		for (int i = 0; i < urisToArticles.size(); i++) {
			articleFeed = rss.getFeed(getThemesHelper().getWebRootWithoutContent() + urisToArticles.get(i));

			for (@SuppressWarnings("unchecked")
			Iterator<SyndEntry> iter = articleFeed.getEntries().iterator(); iter.hasNext();) {
				SyndEntry element = iter.next();
				allEntries.add(element);
			}

		}

		allArticles.setEntries(allEntries);
		String allArticlesContent = null;
		try {
			allArticlesContent = rss.convertFeedToAtomXMLString(allArticles);
		} catch (RemoteException e) {
			e.printStackTrace();
		}

		try {
			getRepository().uploadFileAndCreateFoldersFromStringAsRoot("/files/cms/rss/all_articles/", "feed.xml", allArticlesContent, "text/xml");
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	/**
	 * @param rssRequest
	 * @return
	 */
	protected String fixURI(RSSRequest rssRequest) {
		String uri = "/"+rssRequest.getExtraUri();
		if(!uri.endsWith("/")){
			uri+="/";
		}

		if(!uri.startsWith(CoreConstants.PATH_FILES_ROOT)){
			uri = CoreConstants.PATH_FILES_ROOT+uri;
		}
		return uri;
	}

	/**
	 * @param rssRequest
	 * @param uri
	 * @throws HttpException
	 * @throws IOException
	 * @throws RemoteException
	 */
	protected synchronized void createRSSFile(RSSRequest rssRequest, String uri) throws HttpException, IOException, RepositoryException {
		RepositoryItem resource = getRepository().getRepositoryItemAsRootUser(uri);

		SyndFeed feed = new SyndFeedImpl();
		feed.setTitle(uri+" : Generated by IdegaWeb ePlatform");
		feed.setLink(this.getServerURLWithURI(rssRequest.getRequestWrapped().getServletPath()+"/"+rssRequest.getIdentifier()+uri,rssRequest));
		feed.setDescription("File feed generated by IdegaWeb ePlatform, <a href'http://www.idega.com'/>. This feed lists the latest documents from the folder: "+uri);
		feed.setPublishedDate(new Date());
		feed.setEncoding(CoreConstants.ENCODING_UTF8);
		feed.setCopyright("Idega Software");
		List<SyndEntry> entries = new ArrayList<SyndEntry>();
		SyndEntry entry;
		SyndContent description;

		for (RepositoryItem child: resource.getChildResources()) {
			String fileName = child.getName();
			if (!child.isHidden()) {
				boolean isFolder = child.isCollection();

				entry = new SyndEntryImpl();
				entry.setTitle(fileName);

				if(isFolder){
					entry.setLink(this.getServerURLWithURI("/rss"+child.getPath(),rssRequest));
				}
				else{
					entry.setLink(this.getServerURLWithURI(child.getPath(),rssRequest));
				}

				long creationDate = child.getCreationDate();
				long modifiedDate = child.getLastModified();

				if(creationDate==0){
					creationDate = modifiedDate;
				}

				entry.setPublishedDate(new Date(creationDate));
				entry.setUpdatedDate(new Date(modifiedDate));

				description = new SyndContentImpl();
				description.setType("text/html");

				if (!isFolder) {
					long contentLength = child.getLength();
					if (child.getMimeType().indexOf("image") >-1) {
						description.setValue("<img src='"+this.getServerURLWithURI(child.getPath(), rssRequest)+"'/><br/>Size : "+
								FileUtil.getHumanReadableSize(contentLength)+"<br/>Content type: "+child.getMimeType());
					} else{
						description.setValue("Size : "+FileUtil.getHumanReadableSize(contentLength)+"</br>Content type: "+child.getMimeType());
					}
				} else {
					description.setValue("Folder");
				}
				entry.setDescription(description);

				entries.add(entry);
			}
		}

		feed.setEntries(entries);
		String feedXML = this.getRSSBusiness().convertFeedToRSS2XMLString(feed);
		//deletes the previous version
		getRepository().uploadFileAndCreateFoldersFromStringAsRoot(uri+RSS_FOLDER_NAME+"/", RSS_FILE_NAME, feedXML,this.getRSSContentType());
	}

	public ThemesHelper getThemesHelper() {
		return ELUtil.getInstance().getBean(ThemesHelper.class);
	}

}