package com.idega.content.business;

import java.io.IOException;
import java.rmi.RemoteException;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.Iterator;
import java.util.List;

import javax.faces.context.FacesContext;
import javax.servlet.ServletException;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.idega.block.rss.business.RSSAbstractProducer;
import com.idega.block.rss.business.RSSBusiness;
import com.idega.block.rss.business.RSSProducer;
import com.idega.block.rss.business.RSSProducerImpl;
import com.idega.block.rss.data.RSSRequest;
import com.idega.business.IBOLookup;
import com.idega.business.IBOLookupException;
import com.idega.content.themes.helpers.ThemesHelper;
import com.idega.core.search.business.SearchResult;
import com.idega.idegaweb.IWMainApplication;
import com.idega.slide.business.IWSlideService;
//import com.sun.jmx.snmp.Timestamp;
import com.sun.syndication.feed.synd.SyndEntry;
import com.sun.syndication.feed.synd.SyndFeed;

public class ContentItemRssProducer  extends RSSAbstractProducer implements RSSProducer{
	
	private static final String RSS_PRODUCER_KEY = "rss_producer";
	protected static final String ARTICLE_SEARCH_KEY = "*.xml*";
	public static final String PATH = ContentConstants.CONTENT + ContentUtil.getContentBaseFolderPath() + "/article";//"/files/cms/article";
	
	private static Log log = LogFactory.getLog(RSSProducerImpl.class);
	
	public ContentItemRssProducer() {
		super();
		// TODO Auto-generated constructor stub
	}
	public void handleRSSRequest(RSSRequest rssRequest) throws IOException {
		// TODO Auto-generated method stub
		
		if (existsInSlide(rssRequest.getURI(), rssRequest) == true){
			try {
				dispatch(rssRequest.getURI(), rssRequest);
			} catch (ServletException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
		else{
			//create a file and dispatch the request to it
			searchForArticles();
			try {
				dispatch(rssRequest.getURI(), rssRequest);
			} catch (ServletException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			
		}
	}
	public void searchForArticles() {

		ContentSearch search = new ContentSearch(IWMainApplication.getDefaultIWMainApplication());
		
//System.out.println("Search path: " + PATH);
		Collection results = search.doSimpleDASLSearch(ARTICLE_SEARCH_KEY, PATH);
		if (results == null) {
			log.error("ContentSearch.doSimpleDASLSearch returned results Collection, which is null: " + results);
			return;
		}
		Iterator it = results.iterator();
		List <String> urisToArticles = new ArrayList<String>();
		String uri = null;
		Object o = null;
		while (it.hasNext()) {
			o = it.next();
			if (o instanceof SearchResult) {
				uri = ((SearchResult) o).getSearchResultURI();
//				if (isCorrectFile(uri)) {
				urisToArticles.add(uri);
//				}
			}
		}
		
		RSSBusiness rss = null;
		SyndFeed articleFeed = null;
		Date now = new Date();
		long time = now.getTime();
		try {
			rss = (RSSBusiness) IBOLookup.getServiceInstance(IWMainApplication.getDefaultIWApplicationContext(),
					RSSBusiness.class);
		} catch (IBOLookupException e1) {
			// TODO Auto-generated catch block
			e1.printStackTrace();
		}
		SyndFeed allArticles = rss.createNewFeed("title", ThemesHelper.getInstance().getWebRootWithoutContent(), "description", "atom_1.0", "language", new Timestamp(time));
		List<SyndEntry> allEntries = new ArrayList<SyndEntry>();
//		SyndEntry articleEntry = null;
		for (int i = 0; i < urisToArticles.size(); i++) {
			articleFeed = rss.getFeed(ThemesHelper.getInstance().getWebRootWithoutContent() + urisToArticles.get(i));
//			allArticles.getEntries().add(articleFeed);
//			articleFeed.g
//			allEntries.add(articleFeed.getEntries());
			
//			articleFeed.getEntries();
			articleFeed.getEntries();

			for (Iterator iter = articleFeed.getEntries().iterator(); iter.hasNext();) {
				SyndEntry element = (SyndEntry) iter.next();
				allEntries.add(element);
			}
			
		}
//		allEntries = allArticles.getEntries();
		
		allArticles.setEntries(allEntries);
		String allArticlesContent = null;
		try {
			allArticlesContent = rss.convertFeedToAtomXMLString(allArticles);
		} catch (RemoteException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		IWSlideService service = ThemesHelper.getInstance().getSlideService(null);
		try {
			service.uploadFileAndCreateFoldersFromStringAsRoot("/files/cms/rss/all_articles/", "feed.xml", allArticlesContent, "text/xml", true);
		} catch (RemoteException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

//		checkedFromSlide = getThemesLoader().loadThemes(urisToThemes, false, true);
	}
	
	public static synchronized RSSProducer getInstance(IWMainApplication iwma){
		ContentItemRssProducer producer = (ContentItemRssProducer) iwma.getAttribute(RSS_PRODUCER_KEY);
	    if(producer==null){
	    	producer = new ContentItemRssProducer();
//	    	RSSProducerRegistry.getInstance().addRSSProducer(identifier, rssProducer)
		    iwma.setAttribute(RSS_PRODUCER_KEY,producer);
		    
		    
//		    producer.searchForArticles();
		}
		return producer;		
	}
	public static RSSProducer getInstance(FacesContext context){
		IWMainApplication iwma = IWMainApplication.getIWMainApplication(context);
		return getInstance(iwma);
	}	
	
}
