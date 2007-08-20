package com.idega.content.business;

import java.io.IOException;
import java.rmi.RemoteException;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import javax.servlet.ServletException;

import org.apache.commons.httpclient.HttpException;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.webdav.lib.WebdavResources;

import com.idega.block.rss.business.RSSAbstractProducer;
import com.idega.block.rss.business.RSSBusiness;
import com.idega.block.rss.business.RSSProducer;
import com.idega.block.rss.data.RSSRequest;
import com.idega.business.IBOLookup;
import com.idega.business.IBOLookupException;
import com.idega.content.themes.helpers.ThemesHelper;
import com.idega.core.search.business.SearchResult;
import com.idega.idegaweb.IWMainApplication;
import com.idega.presentation.IWContext;
import com.idega.slide.business.IWContentEvent;
import com.idega.slide.business.IWSlideChangeListener;
import com.idega.slide.business.IWSlideService;
import com.idega.slide.util.WebdavExtendedResource;
import com.idega.util.CoreConstants;
import com.idega.util.CoreUtil;
import com.idega.util.FileUtil;
import com.sun.syndication.feed.synd.SyndContent;
import com.sun.syndication.feed.synd.SyndContentImpl;
import com.sun.syndication.feed.synd.SyndEntry;
import com.sun.syndication.feed.synd.SyndEntryImpl;
import com.sun.syndication.feed.synd.SyndFeed;
import com.sun.syndication.feed.synd.SyndFeedImpl;

public class ContentItemRssProducer  extends RSSAbstractProducer implements RSSProducer, IWSlideChangeListener{
	
	private static final String RSS_PRODUCER_KEY = "rss_producer";
	protected static final String ARTICLE_SEARCH_KEY = "*.xml*";
	public static final String RSS_FOLDER_NAME = "rss";
	public static final String RSS_FILE_NAME = "feed.xml";

	public static final String PATH = CoreConstants.WEBDAV_SERVLET_URI + ContentUtil.getContentBaseFolderPath() + "/article/";//"/files/cms/article";
	private Map rssFileURIsCacheMap = new HashMap();
	private static Log log = LogFactory.getLog(ContentItemRssProducer.class);
	
	public ContentItemRssProducer() {
		super();
		// TODO Auto-generated constructor stub
	}
	
//	public void handleRSSRequest(RSSRequest rssRequest) throws IOException {
//		
//		String uri = fixURI(rssRequest);		
//		
//		String feedFile = uri+RSS_FOLDER_NAME+"/"+RSS_FILE_NAME;
//		String realURI = "/article"+feedFile;
//		try {
//			if(this.isAFolderInSlide(uri,rssRequest)){
//				if(this.existsInSlide(feedFile,rssRequest) && rssFileURIsCacheMap.containsKey(uri)){
//					this.dispatch(realURI, rssRequest);
//				}	
//				else{
//					try {
//						createRSSFile(rssRequest, uri);
//						rssFileURIsCacheMap.put(uri,realURI);
//						this.dispatch("/article"+feedFile, rssRequest);
//					} catch (Exception e) {
//						throw new IOException(e.getMessage());
//					}
//				}
//			}
//			else{}
//		} catch (ServletException e) {
//			e.printStackTrace();
//		}
//	}	
	public void handleRSSRequest(RSSRequest rssRequest) throws IOException {
		// TODO Auto-generated method stub
		
		String uri = fixURI(rssRequest);
		
//		if (existsInSlide(rssRequest.getURI(), rssRequest) == true){
		if(this.isAFolderInSlide(uri,rssRequest)){
			try {
				dispatch(rssRequest.getURI(), rssRequest);
			} catch (ServletException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
		else{
			//create a file and dispatch the request to it
//			createRSSFile(rssRequest, uri);
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
//	public void createRSSFile(RSSRequest rssRequest, String fixedUri) {
		IWContext iwc = CoreUtil.getIWContext();
		System.out.println("Context: " + iwc);
		ContentSearch search = new ContentSearch(IWMainApplication.getDefaultIWMainApplication());
		
//System.out.println("Search path: " + PATH);
		Collection results = search.doSimpleDASLSearch(ARTICLE_SEARCH_KEY, PATH);
//		search.
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
		
//		feed.setTitle(uri+" : Generated by IdegaWeb ePlatform");
//		feed.setLink(this.getServerURLWithURI(rssRequest.getRequestWrapped().getServletPath()+"/"+rssRequest.getIdentifier()+uri,rssRequest));
//		feed.setDescription("File feed generated by IdegaWeb ePlatform, <a href'http://www.idega.com'/>. This feed lists the latest documents from the folder: "+uri);
//		feed.setPublishedDate(new Date());
//		feed.setEncoding("UTF-8");
//		feed.setCopyright("Idega Software");		
		
		
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
	
//	public static synchronized RSSProducer getInstance(IWMainApplication iwma){
//		ContentItemRssProducer producer = (ContentItemRssProducer) iwma.getAttribute(RSS_PRODUCER_KEY);
//	    if(producer==null){
//	    	producer = new ContentItemRssProducer();
////	    	RSSProducerRegistry.getInstance().addRSSProducer(identifier, rssProducer)
//		    iwma.setAttribute(RSS_PRODUCER_KEY,producer);
//		    
//		    
////		    producer.searchForArticles();
//		}
//		return producer;		
//	}
//	public static RSSProducer getInstance(FacesContext context){
//		IWMainApplication iwma = IWMainApplication.getIWMainApplication(context);
//		return getInstance(iwma);
//	}	
	public void onSlideChange(IWContentEvent contentEvent) {
		// TODO Auto-generated method stub		
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
	protected synchronized void createRSSFile(RSSRequest rssRequest, String uri) throws HttpException, IOException, RemoteException {
		WebdavExtendedResource folder = this.getIWSlideSession(rssRequest).getWebdavResource(uri);
//		WebdavResources childResources = folder.

		WebdavResources resources = folder.listWithDeltaV();
		Enumeration children = resources.getResources();

		SyndFeed feed = new SyndFeedImpl();
		feed.setTitle(uri+" : Generated by IdegaWeb ePlatform");
		feed.setLink(this.getServerURLWithURI(rssRequest.getRequestWrapped().getServletPath()+"/"+rssRequest.getIdentifier()+uri,rssRequest));
		feed.setDescription("File feed generated by IdegaWeb ePlatform, <a href'http://www.idega.com'/>. This feed lists the latest documents from the folder: "+uri);
		feed.setPublishedDate(new Date());
		feed.setEncoding("UTF-8");
		feed.setCopyright("Idega Software");
		List entries = new ArrayList();
		SyndEntry entry;
		SyndContent description;

		while (children.hasMoreElements()) {
			WebdavExtendedResource resource = (WebdavExtendedResource) children.nextElement();
			String fileName = resource.getDisplayName();
			if(! this.getIWSlideService(rssRequest).isHiddenFile(fileName)){
				boolean isFolder = resource.isCollection();
				
				entry = new SyndEntryImpl();
				entry.setTitle(fileName);
				
				if(isFolder){
					entry.setLink(this.getServerURLWithURI("/rss"+resource.getPath(),rssRequest));
				}
				else{
					entry.setLink(this.getServerURLWithURI(resource.getPath(),rssRequest));
				}
				
				long creationDate = resource.getCreationDate();
				long modifiedDate = resource.getGetLastModified();
				
				if(creationDate==0){
					creationDate = modifiedDate;
				}
				
				entry.setPublishedDate(new Date(creationDate));
				entry.setUpdatedDate(new Date(modifiedDate));

				description = new SyndContentImpl();
				description.setType("text/html");
				
				if(!isFolder){
					if(resource.getGetContentType().indexOf("image")>-1){
						description.setValue("<img src='"+this.getServerURLWithURI(resource.getPath(),rssRequest)+"'/><br/>Size : "+FileUtil.getHumanReadableSize(resource.getGetContentLength())+"<br/>Content type: "+resource.getGetContentType());									
					}
					else{
						description.setValue("Size : "+FileUtil.getHumanReadableSize(resource.getGetContentLength())+"</br>Content type: "+resource.getGetContentType());	
					}
				}
				else{
					description.setValue("Folder");
				}
				entry.setDescription(description);
				
				entries.add(entry);
			}
		}

		feed.setEntries(entries);
		String feedXML = this.getRSSBusiness().convertFeedToRSS2XMLString(feed);	
		//deletes the previous version
		this.getIWSlideService(rssRequest).uploadFileAndCreateFoldersFromStringAsRoot(uri+RSS_FOLDER_NAME+"/", RSS_FILE_NAME, feedXML,this.getRSSContentType(),true);
		System.out.println("parentPath: "+uri+RSS_FOLDER_NAME+"/"+"rss file name: "+RSS_FILE_NAME);
	}
	
	private void getEntriesFromFolder(){
		
	}
}
