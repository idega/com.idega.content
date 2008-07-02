package com.idega.content.themes.helpers.business;

import java.awt.Dimension;
import java.awt.Image;
import java.awt.image.BufferedImage;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.Reader;
import java.io.UnsupportedEncodingException;
import java.net.URL;
import java.net.URLDecoder;
import java.net.URLEncoder;
import java.rmi.RemoteException;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Random;
import java.util.TreeMap;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.ejb.FinderException;

import org.apache.commons.httpclient.HttpException;
import org.apache.commons.httpclient.HttpURL;
import org.apache.commons.httpclient.URIException;
import org.jdom.Attribute;
import org.jdom.Document;
import org.jdom.Element;
import org.jdom.JDOMException;
import org.jdom.Namespace;
import org.jdom.input.SAXBuilder;
import org.xml.sax.EntityResolver;

import com.idega.builder.bean.AdvancedProperty;
import com.idega.business.IBOLookup;
import com.idega.business.IBOLookupException;
import com.idega.content.bean.ContentItemFeedBean;
import com.idega.content.business.ContentConstants;
import com.idega.content.business.ContentSearch;
import com.idega.content.business.ContentUtil;
import com.idega.content.themes.bean.ThemesManagerBean;
import com.idega.content.themes.business.ThemesEngine;
import com.idega.content.themes.business.ThemesService;
import com.idega.content.themes.helpers.bean.Setting;
import com.idega.content.themes.helpers.bean.Theme;
import com.idega.content.themes.helpers.bean.ThemeStyleGroupMember;
import com.idega.core.builder.data.ICDomain;
import com.idega.core.builder.data.ICPage;
import com.idega.core.component.business.ICObjectBusiness;
import com.idega.core.component.data.ICObject;
import com.idega.core.component.data.ICObjectHome;
import com.idega.core.component.data.ICObjectInstance;
import com.idega.core.component.data.ICObjectInstanceHome;
import com.idega.core.search.business.SearchResult;
import com.idega.data.IDOLookup;
import com.idega.graphics.image.business.ImageGenerator;
import com.idega.graphics.image.business.ImageGeneratorImpl;
import com.idega.graphics.util.GraphicsConstants;
import com.idega.idegaweb.IWApplicationContext;
import com.idega.idegaweb.IWMainApplication;
import com.idega.idegaweb.IWMainApplicationSettings;
import com.idega.presentation.IWContext;
import com.idega.repository.data.Singleton;
import com.idega.servlet.filter.IWBundleResourceFilter;
import com.idega.slide.business.IWSlideService;
import com.idega.util.CoreConstants;
import com.idega.util.CoreUtil;
import com.idega.util.StringHandler;
import com.idega.util.expression.ELUtil;
import com.idega.webface.WFUtil;

//	TODO: make this class Spring bean
public class ThemesHelper implements Singleton {
	
	private static Logger log = Logger.getLogger(ThemesHelper.class.getName());
	
	private volatile static ThemesHelper helper = null;
	private volatile ImageGenerator generator = null;
	private volatile IWSlideService service = null;
	private volatile ThemesService themesService = null;
	private volatile ContentItemFeedBean feedBean = null;
	
	private ThemesEngine themesEngine = null;
	
	private Map<String, Theme> themes = null;
	private Map<String, Setting> themeSettings = null;
	private Map<String, Setting> pageSettings = null;
	private List<String> moduleIds = null;
	private List<String> themeQueue = null;
	private List<String> urisToThemes = null;
	private List<String> loadedThemes = null;
	private List<String> predefinedThemeStyles = null;
	
	private boolean checkedFromSlide = false;
	private boolean loadedThemeSettings = false;
	private boolean loadedPageSettings = false;
	
	private String fullWebRoot; // For cache
	private String webRoot;
	
	private static final String RESOURCE_PATH_END = CoreConstants.DOT + "article";
	private static final String ATTRIBUTE_PROPERTY = "value";
	private static final String ROOT_PAGE_ARTICLE = "root_page_article";
	private static final String MODULE_ID_SCOPE = "module_id";
	private static final String IDEGA_PAGES_SCOPE = "idega_pages";
	private static final String MODULE_ELEMENT_NAME = "module";
	private static final String ELEMENT_CLASS_ATTRIBUTE = "class";
	private static final String ATTRIBUTE_NAME = "name";
	private static final String ATTRIBUTE_RESOURCE_PATH_VALUE = "resourcePath";
	
	private Random numberGenerator = null;
	
	private ThemesHelper(boolean canUseSlide) {
		themes = new HashMap <String, Theme> ();
		pageSettings = new HashMap <String, Setting> ();
		
		themeSettings = Collections.synchronizedMap(new TreeMap<String, Setting>());
		
		themeQueue = new ArrayList <String> ();
		urisToThemes = new ArrayList <String> ();
		loadedThemes = new ArrayList<String>();
		moduleIds = new ArrayList<String>();
		predefinedThemeStyles = new ArrayList<String>();
		
		numberGenerator = new Random();
		if (canUseSlide) {
			searchForThemes();
		}
	}
	
	public static synchronized ThemesHelper getInstance() {
		if (helper == null) {
			helper = new ThemesHelper(false);
		}
		return helper;
	}
	
	public static synchronized ThemesHelper getInstance(boolean searchForThemes) {
		if (helper == null) {
			helper = new ThemesHelper(searchForThemes);
		}
		return helper;
	}
	
	protected ImageGenerator getImageGenerator(IWContext iwc) {
		if (generator == null) {
			if (iwc == null) {
				iwc = CoreUtil.getIWContext();
			}
			if (iwc == null) {
				generator = new ImageGeneratorImpl();
			}
			else {
				generator = new ImageGeneratorImpl(iwc);
			}
		}
		return generator;
	}
	
	public ThemeChanger getThemeChanger() throws NullPointerException {
		try {
			return getThemesEngine().getThemeChanger();
		} catch(Exception e) {
			log.log(Level.SEVERE, "Error getting ThemeChanger", e);
			return null;
		}
	}
	
	public ThemesPropertiesExtractor getThemesPropertiesExtractor() throws NullPointerException {
		try {
			return getThemesEngine().getThemesPropertiesExtractor();
		} catch(Exception e) {
			log.log(Level.SEVERE, "Error getting ThemesPropertiesExtractor", e);
			return null;
		}
	}
	
	protected IWSlideService getSlideService() {
		return getSlideService(null);
	}
	
	public IWSlideService getSlideService(IWApplicationContext iwac) {
		if (service == null) {
			try {
				if (iwac == null) {
					iwac = CoreUtil.getIWContext();
				}
				service = (IWSlideService) IBOLookup.getServiceInstance(iwac, IWSlideService.class);
			} catch (Exception e) {
				e.printStackTrace();
				return null;
			}
		}
		return service;
	}
	
	public ThemesLoader getThemesLoader() {
		return new ThemesLoader(helper);
	}
	
	private ContentItemFeedBean getFeedBean() {
		if (feedBean == null) {
			feedBean = new ContentItemFeedBean(null, ContentItemFeedBean.FEED_TYPE_ATOM_1);
		}
		return feedBean;
	}
	
	public synchronized void searchForThemes() {
		if (checkedFromSlide) {
			return;
		}
		checkedFromSlide = true;
		
		String searchScope = new StringBuffer(CoreConstants.WEBDAV_SERVLET_URI).append(ThemesConstants.THEMES_PATH).toString();
		
		List<SearchResult> themes = search(ThemesConstants.THEME_SEARCH_KEY, searchScope);
		if (themes == null) {
			log.log(Level.WARNING, "ContentSearch.doSimpleDASLSearch returned results Collection, which is null!");
			checkedFromSlide = false;
			return;
		}
		List<String> themesSkeletons = loadSearchResults(themes, ThemesConstants.THEME_SKELETONS_FILTER);
		
//		String propSearchKey = new StringBuffer("*").append(ThemesConstants.THEME_PROPERTIES_FILE_END).toString();
//		Collection propertiesLists = search(propSearchKey, searchScope);
//		List<String> pLists = loadSearchResults(propertiesLists, null);
//		
//		String configSearchKey = new StringBuffer("*").append(ThemesConstants.IDEGA_THEME_INFO).toString();
//		Collection configurationXmls = search(configSearchKey, searchScope);
//		List<String> configurations = loadSearchResults(configurationXmls, null);
	
		try {
			getThemesLoader().loadThemes(themesSkeletons, false, true);
		} catch (Exception e) {
			e.printStackTrace();
		}
//		getThemesPropertiesExtractor().prepareThemes(pLists, configurations, true);
	}
	
	public List<String> loadSearchResults(List<SearchResult> searchResults, List<String> filter) {
		List <String> loadedResults = new ArrayList<String>();
		if (searchResults == null) {
			return loadedResults;
		}
		
		String uri = null;
		for (int i = 0; i < searchResults.size(); i++) {
			uri = searchResults.get(i).getSearchResultURI();
			if (isCorrectThemeTemplateFile(uri, filter)) {
				loadedResults.add(uri);
			}
		}
		return loadedResults;
	}
	
	@SuppressWarnings("unchecked")
	public List<SearchResult> search(String searchKey, String searchScope) {
		if (searchKey == null || searchScope == null) {
			return null;
		}
		
		ContentSearch search = new ContentSearch(IWMainApplication.getDefaultIWMainApplication());
		Collection results = search.doSimpleDASLSearch(searchKey, searchScope);
		if (results == null) {
			return null;
		}
		
		List<SearchResult> wrapped = new ArrayList<SearchResult>();
		Object o = null;
		for (Iterator it = results.iterator(); it.hasNext();) {
			o = it.next();
			if (o instanceof SearchResult) {
				wrapped.add((SearchResult) o); 
			}
		}
		
		return wrapped;
	}
	
	protected String getFileName(String uri) {
		if (uri == null) {
			return null;
		}
		String name = null;
		int begin = uri.lastIndexOf(ContentConstants.SLASH);
		int end = uri.lastIndexOf(CoreConstants.DOT);
		if (begin == -1) {
			name = extractValueFromString(uri, 0, end);
		}
		else {
			name = extractValueFromString(uri, begin + 1, end);
		}
		return name;
	}
	
	protected String getFileNameWithExtension(String uri) {
		if (uri == null) {
			return null;
		}
		String name = null;
		int begin = uri.lastIndexOf(ContentConstants.SLASH);
		if (begin == -1) {
			return uri;
		}
		else {
			name = extractValueFromString(uri, begin + 1, uri.length());
		}
		return name;
	}
	
	public String extractValueFromString(String fullString, int beginIndex, int endIndex) {
		String value = ThemesConstants.EMPTY;
		if (canExtractValueFromString(fullString, beginIndex, endIndex)) {
			value = fullString.substring(beginIndex, endIndex);
		}
		return value;
	}
	
	private boolean canExtractValueFromString(String fullString, int beginIndex, int endIndex) {
		if (fullString == null) {
			return false;
		}
		if (beginIndex != -1 && endIndex != -1) {
			if (beginIndex <= endIndex && endIndex <= fullString.length()) {
				return true;
			}
		}
		return false;
	}
	
	protected String getFileExtension(String uri) {
		if (uri == null) {
			return null;
		}
		String type = null;
		int begin = uri.lastIndexOf(CoreConstants.DOT);
		if (begin != -1) {
			type = uri.substring(begin + 1).toLowerCase();
		}
		return type;
	}
	
	public String getWebRootWithoutContent() {
		return getWebRootWithoutContent(getFullWebRoot());
	}
	
	protected String getWebRootWithoutContent(String fullWebRoot) {
		if (webRoot != null) {
			return webRoot;
		}
		String webDAVServerURI = ThemesConstants.EMPTY;
		try {
			webDAVServerURI = getSlideService().getWebdavServerURI();
		} catch (RemoteException e) {
			e.printStackTrace();
		}
		int contentIndex = fullWebRoot.indexOf(webDAVServerURI);
		webRoot = extractValueFromString(fullWebRoot, 0, contentIndex);
		return webRoot;
	}
	
	public String getFullWebRoot() {
		if (fullWebRoot != null) {
			return fullWebRoot;
		}
		HttpURL root = null;
		try {
			root = getSlideService().getWebdavServerURL();
		} catch (RemoteException e) {
			e.printStackTrace();
			return null;
		}
		
		String serverURL = null;
		try {
			serverURL = root.getURI();
		} catch (URIException e) {
			e.printStackTrace();
			return null;
		}
		
		fullWebRoot = serverURL;
		
		return fullWebRoot;
	}
	
	protected boolean isCorrectFile(String fileName, String nameTemplate) {
		if (fileName == null || nameTemplate == null) {
			return false;
		}
		return fileName.equals(nameTemplate);
	}
	
	public boolean isCorrectThemeTemplateFile(String fileName, List<String> filter) {
		boolean result = false;
		if (fileName == null) {
			return false;
		}
		if (isSystemFile(fileName)) {
			return false;
		}
		if (isDraft(fileName)) {
			return false;
		}
		
		int index = fileName.lastIndexOf(CoreConstants.DOT);
		if (index == -1) {
			return false;
		}
		String fileExtension = fileName.substring(index + 1);
		
		if (filter == null) {
			return true;
		}
		
		if (fileName.indexOf("index") == -1 && fileName.indexOf(ThemesConstants.IDEGA_THEME) == -1) {
			return false;
		}
		
		for (int i = 0; (i < filter.size() && !result); i++) {
			result = isCorrectFile(fileExtension, filter.get(i));
		}
		return result;
	}
	
	public boolean isCreatedManually(String fileName) {
		if (fileName == null) {
			return true;
		}
		if (fileName.endsWith(ThemesConstants.THEME)) {
			return true;
		}
		return false;
	}
	
	protected boolean isDraft(String fileName) {
		if (fileName == null) {
			return true;
		}
		if (fileName.endsWith(ThemesConstants.DRAFT)) {
			return true;
		}
		return false;
	}
	
	protected boolean isSystemFile(String fileName) {
		if (fileName == null) {
			return true; // Not a system file, but invalid also
		}
		if (getFileNameWithExtension(fileName).startsWith(CoreConstants.DOT)) {
			return true;
		}
		return false;
	}
	
	protected boolean isPropertiesFile(String uri) {
		if (ThemesConstants.THEME_PROPERTIES_FILES.contains(uri)) {
			return true;
		}
		return false;
	}
	
	protected void addTheme(Theme theme) {
		themes.put(theme.getId(), theme);
	}
	
	public List<Theme> getAvailableThemes() {
		Iterator<Theme> it = themes.values().iterator();
		if (it == null) {
			return null;
		}
		
		List<Theme> availableThemes = new ArrayList<Theme>();
		Theme theme = null;
		for (Iterator<Theme> i = it; i.hasNext();) {
			theme = i.next();
			
			if (!theme.isLoading() && !theme.isLocked() && theme.isPropertiesExtracted()) {
				availableThemes.add(theme);
			}
		}
		
		return availableThemes;
	}
	
	public Collection<Theme> getAllThemes() {
		return themes.values();
	}
	
	public List<Theme> getSortedThemes() {
		List<Theme> availableThemes = getAvailableThemes();
		
		if (availableThemes == null || availableThemes.isEmpty()) {
			return null;
		}
		
		Locale locale = null;
		IWContext iwc = CoreUtil.getIWContext();
		if (iwc != null) {
			locale = iwc.getCurrentLocale();
		}
		Collections.sort(availableThemes, new ThemesComparator(locale == null ? Locale.ENGLISH : locale));
		
		return availableThemes;
	}
	
	protected synchronized void addUriToTheme(String uri) {
		urisToThemes.add(uri);
	}
	
	public synchronized boolean existTheme(String uri) {
		if (urisToThemes == null) {
			return false;
		}
		for (int i = 0; i < urisToThemes.size(); i++) {
			if (urisToThemes.get(i).equals(uri)) {
				return true;
			}
		}
		return false;
	}
	
	public Document getXMLDocument(String url) {
		return getXMLDocument(url, false, false);
	}
	
	private Document getXMLDocument(String url, boolean cleanWithHtmlCleaner, boolean useLog) {
		if (url == null) {
			return null;
		}
		
		InputStream stream = getInputStream(url, useLog);
		
		if (stream != null && cleanWithHtmlCleaner) {
			String content = getThemesService().getBuilderService().getCleanedHtmlContent(stream, false, false, true);
			if (content == null) {
				return null;
			}
			try {
				stream = StringHandler.getStreamFromString(content);
			} catch (Exception e) {
				e.printStackTrace();
			}
		}
		
		try {
			return getXMLDocument(stream);
		} catch(Exception e) {
			e.printStackTrace();
		} finally {
			closeInputStream(stream);
		}
		
		return null;
	}
	
	public Document getXMLDocument(String url, boolean cleanWithHtmlCleaner) {
		return getXMLDocument(url, cleanWithHtmlCleaner, false);
	}
	
	public Document getXMLDocument(InputStream stream) throws Exception {
		if (stream == null) {
			return null;
		}

		Reader reader = null;
		try {
			reader = new InputStreamReader(stream, CoreConstants.ENCODING_UTF8);
		} catch (Exception e) {
			e.printStackTrace();
			return null;
		}
		
		Document document = null;
		try {
			SAXBuilder builder = new SAXBuilder(false);
			EntityResolver resolver = null;
			//Creating our EntityResolver to avoid IOException trying to load DTD file, defined in every Theme.plist file:
			//<!DOCTYPE plist PUBLIC "-//Apple Computer//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
			resolver = new ThemesEntityResolver();
			builder.setEntityResolver(resolver);
			try {
				document = builder.build(reader);
			} catch (JDOMException e) {
				log.log(Level.SEVERE, "JDOM exception trying to build JDOM Document");
				e.printStackTrace();
				return null;
			} catch (IOException e) {
				log.log(Level.SEVERE, "IOException trying to build a JDOM Document");
				e.printStackTrace();
				return null;
			}
		} catch(Exception e) {
			e.printStackTrace();
		} finally {
			closeInputStream(stream);
			reader.close();
		}
			
		return document;
	}
	
	protected String getLinkToBase(String uri) {
		int index = uri.lastIndexOf(ContentConstants.SLASH);
		String link = extractValueFromString(uri, 0, index);
		if (!link.endsWith(ContentConstants.SLASH)) {
			link += ContentConstants.SLASH;
		}
		return link;
	}
	
	public Theme getTheme(String themeKey) {
		if (themeKey == null) {
			return null;
		}
		return themes.get(themeKey);
	}
	
	public void removeTheme(String uri, String themeKey) {
		if (uri == null || themeKey == null) {
			return;
		}
		urisToThemes.remove(uri);
		themes.remove(themeKey);
	}

	protected Map <String, Theme> getThemes() {
		return themes;
	}
	
	public Map <String, Setting> getThemeSettings() {
		return themeSettings;
	}
	
	public Map <String, Setting> getPageSettings() {
		return pageSettings;
	}
	
	public void loadThemeSettings(InputStream stream) {
		if (loadedThemeSettings) {
			return;
		}
		
		try {
			loadSettings(themeSettings, getXMLDocument(stream));
			loadedThemeSettings = true;
		} catch(Exception e) {
			e.printStackTrace();
			loadedThemeSettings = false;
		}
	}
	
	public void loadPageSettings(String url) {
		if (loadedPageSettings) {
			return;
		}
		loadSettings(pageSettings, getXMLDocument(url));
		loadedPageSettings = true;
	}
	
	@SuppressWarnings("unchecked")
	private void loadSettings(Map <String, Setting> settings, Document doc) {
		if (doc == null) {
			return;
		}
		Element root = doc.getRootElement();
		if (root == null) {
			return;
		}
		List<Element> keys = root.getChildren();
		if (keys == null) {
			return;
		}
		Element key = null;
		Setting setting = null;
		for (int i = 0; i < keys.size(); i++) {
			key = keys.get(i);
			setting = new Setting();
			
			setting.setCode(key.getChildTextNormalize(ThemesConstants.SETTING_CODE));
			setting.setLabel(key.getChildTextNormalize(ThemesConstants.SETTING_LABEL));
			setting.setDefaultValue(key.getChildTextNormalize(ThemesConstants.SETTING_DEFAULT_VALUE));
			setting.setType(key.getChildTextNormalize(ThemesConstants.SETTING_TYPE));
			setting.setMethod(key.getChildTextNormalize(ThemesConstants.SETTING_METHOD));
			
			settings.put(setting.getCode(), setting);
		}
	}

	private InputStream getInputStream(String uri, boolean printError) {
		if (uri == null) {
			return null;
		}
		
		InputStream stream = null;
		if (uri.startsWith(IWBundleResourceFilter.BUNDLES_STANDARD_DIR)) {
			File file = IWBundleResourceFilter.copyResourceFromJarToWebapp(IWMainApplication.getDefaultIWMainApplication(), uri);
			if (file == null) {
				if (printError) {
					log.log(Level.WARNING, "Error getting file (file was not found): " + uri);
				}
				return null;
			}
			
			try {
				stream = new FileInputStream(file);
			} catch (FileNotFoundException e) {
				if (printError) {
	        		e.printStackTrace();
	        		log.log(Level.WARNING, "Error getting file: " + uri);
	        	}
			}
		}
		else {
			try {
				URL url = new URL(uri);
				stream = url.openStream();
			} catch(Exception e) {
				if (printError) {
	        		e.printStackTrace();
	        		log.log(Level.WARNING, "Error getting URL: " + uri);
	        	}
			}
		}
		
		return stream;
	}
	
	protected InputStream getInputStream(String link) {
		return getInputStream(link, false);
	}
	
	public boolean closeInputStream(InputStream is) {
		if (is == null) {
			return true;
		}
		
		try {
			is.close();
		} catch (IOException e) {
			e.printStackTrace();
			return false;
		}
		return true;
	}
	
	protected boolean closeOutputStream(OutputStream os) {
		try {
			os.close();
		} catch (IOException e) {
			e.printStackTrace();
			return false;
		}
		return true;
	}
	
	public String encode(String value, boolean fullyEncode) {
		if (value == null) {
			return null;
		}
		if (fullyEncode) {
			try {
				value = URLEncoder.encode(value, CoreConstants.ENCODING_UTF8);
			} catch (UnsupportedEncodingException e) {
				e.printStackTrace();
				return value;
			}
		}
		while (value.indexOf(ThemesConstants.PLUS) != -1) {
			value = value.replace(ThemesConstants.PLUS, ThemesConstants.SPACE_ENCODED);
		}
		return value;
	}
	
	public String urlEncode(String url) {
		String[] fileParts = url.split(ContentConstants.SLASH);
		StringBuffer encoded = new StringBuffer();
		for (int i = 0; i < fileParts.length; i++) {
			if (fileParts[i].equals(ThemesConstants.EMPTY)) {
				encoded.append(ContentConstants.SLASH);
			}
			else {
				try {
					encoded.append(URLEncoder.encode(fileParts[i], CoreConstants.ENCODING_UTF8));
				} catch (UnsupportedEncodingException e) {
					e.printStackTrace();
					return url;
				}
				if (i + 1 < fileParts.length) {
					encoded.append(ContentConstants.SLASH);
				}
			}
		}
		return encode(encoded.toString(), false);
	}
	
	protected String decode(String value, boolean fullyDecode) {
		if (value == null) {
			return null;
		}
		while (value.indexOf(ThemesConstants.SPACE_ENCODED) != -1) {
			value = value.replace(ThemesConstants.SPACE_ENCODED, ThemesConstants.PLUS);
		}
		if (fullyDecode) {
			try {
				value = URLDecoder.decode(value, CoreConstants.ENCODING_UTF8);
			} catch (UnsupportedEncodingException e) {
				e.printStackTrace();
				return value;
			}
		}
		return value;
	}
	
	public String decodeUrl(String url) {
		url = decode(url, false);
		String[] fileParts = url.split(ContentConstants.SLASH);
		StringBuffer encoded = new StringBuffer();
		encoded.append(ContentConstants.SLASH);
		for (int i = 0; i < fileParts.length; i++) {
			if (!fileParts[i].equals(ThemesConstants.EMPTY)) {
				try {
					encoded.append(URLDecoder.decode(fileParts[i], CoreConstants.ENCODING_UTF8));
				} catch (UnsupportedEncodingException e) {
					e.printStackTrace();
					return url;
				}
				if (i + 1 < fileParts.length) {
					encoded.append(ContentConstants.SLASH);
				}
			}
		}
		return encoded.toString();
	}
	
	protected boolean createSmallImage(Theme theme, String url) {
		if (theme == null || url == null) {
			return false;
		}
		
		String extension = getFileExtension(url);
		if (extension == null) {
			return false;
		}
		extension = extension.toLowerCase();
		String mimeType = new StringBuffer(ThemesConstants.DEFAULT_MIME_TYPE).append(extension).toString();
		String newName = new StringBuffer(theme.getName()).append(ThemesConstants.THEME_SMALL_PREVIEW).append(CoreConstants.DOT).append(extension).toString();
		boolean isJpg = extension.equals(GraphicsConstants.JPG_FILE_NAME_EXTENSION);
		
		ImageGenerator imageGenerator = getImageGenerator(null);
		InputStream stream = getInputStream(url);
		if (stream == null) {
			return false;
		}
		Image image = null;
		try {
			image = imageGenerator.getScaledImage(stream, ThemesConstants.SMALL_PREVIEW_WIDTH, ThemesConstants.SMALL_PREVIEW_HEIGHT, isJpg);
		} catch(Exception e) {
			e.printStackTrace();
			image = null;
		} finally {
			closeInputStream(stream);
		}
		if (image == null) {
			return generatePreviewsForTheme(theme, false, isJpg, ThemesConstants.THEME_PREVIEW_QUALITY, false);
		}
		stream = imageGenerator.getImageInputStream(image, extension, isJpg);
		if (stream == null) {
			return false;
		}
		boolean uploadedSuccessfully = true;
		try {
			uploadedSuccessfully = getSlideService().uploadFileAndCreateFoldersFromStringAsRoot(theme.getLinkToBaseAsItIs(), newName, stream, mimeType, true);
		} catch (RemoteException e) {
			e.printStackTrace();
			return false;
		} finally {
			closeInputStream(stream);
		}
		if (uploadedSuccessfully) {
			theme.setLinkToSmallPreview(newName);
		}
		
		return uploadedSuccessfully;
	}
	
	protected boolean createSmallImage(Theme theme, boolean useDraftPreview) {
		String encodedUriToImage = null;
		String uriToImage = null;
		if (useDraftPreview) {
			uriToImage = theme.getLinkToDraftPreview();
		}
		else {
			uriToImage = theme.getLinkToThemePreview();
		}
		encodedUriToImage = encode(uriToImage, true);
		
		String url = new StringBuffer(getFullWebRoot()).append(theme.getLinkToBase()).append(encodedUriToImage).toString();
		return createSmallImage(theme, url);
	}
	
	public ThemesService getThemesService() {
		if (themesService == null) {
			try {
				themesService = (ThemesService) IBOLookup.getServiceInstance(CoreUtil.getIWContext(), ThemesService.class);
			} catch (IBOLookupException e) {
				e.printStackTrace();
			}
		}
		return themesService;
	}

	protected boolean createThemeConfig(Theme theme) {
		Document doc = new Document();
		Element root = new Element(ThemesConstants.CON_THEME);
		Collection <Element> rootElements = new ArrayList<Element>();
		
		//	Name
		Element name = new Element(ThemesConstants.CON_NAME);
		name.setText(theme.getName());
		rootElements.add(name);
		
		//	Style variations
		Element styles = new Element(ThemesConstants.CON_STYLES);
		Collection <Element> stylesElements = new ArrayList<Element>();
		
		ThemeChanger changer = getThemeChanger();
		if (changer == null) {
			return false;
		}
		
		List <ThemeStyleGroupMember> enabled = null;
		try {
			enabled = changer.getEnabledStyles(theme);
		} catch (Exception e) {
			e.printStackTrace();
			return false;
		}
		
		ThemeStyleGroupMember member = null;
		
		Element style = null;
		Collection <Element> styleElements = null;
		Element groupName = null;
		Element variation = null;
		Element color = null;
		Element variable = null;
		String hexColor = null;
		for (int i = 0; i < enabled.size(); i++) {
			member = enabled.get(i);
			style = new Element(ThemesConstants.CON_STYLE);
			styleElements = new ArrayList<Element>();

			groupName = new Element(ThemesConstants.CON_GROUP);
			groupName.setText(member.getGroupName());
			styleElements.add(groupName);
			
			variation = new Element(ThemesConstants.CON_VARIATION);
			variation.setText(member.getName());
			styleElements.add(variation);
			
			if (!member.isStylesheet()) {	//	For other types then simple CSS variations
				if (member.getColour() != null && member.getVariable() != null) {
					//	Color settings
					variable = new Element(ThemesConstants.CON_VARIABLE);
					variable.setText(member.getVariable());
					styleElements.add(variable);
					
					color = new Element(ThemesConstants.CON_COLOR);
					hexColor = theme.getStyleVariableValue(member.getVariable());
					if (hexColor == null) {
						hexColor = member.getColour();
					}
					member.setColour(hexColor);
					color.setText(hexColor);
					styleElements.add(color);
				}
			}

			style.setContent(styleElements);
			stylesElements.add(style);
		}
		styles.setContent(stylesElements);
		rootElements.add(styles);
		
		//	Color files
		addColourFilesToConfig(rootElements, theme.getOriginalColourFiles(), ThemesConstants.CON_COLOUR_FILES_ORIGINAL);
		addColourFilesToConfig(rootElements, theme.getColourFiles(), ThemesConstants.CON_COLOUR_FILES);
		
		//	Big preview image
		Element preview = new Element(ThemesConstants.CON_PREVIEW);
		preview.setText(theme.getLinkToThemePreview());
		rootElements.add(preview);
		
		//	Small preview image
		Element smallPreview = new Element(ThemesConstants.CON_SMALL_PREVIEW);
		smallPreview.setText(theme.getLinkToSmallPreview());
		rootElements.add(smallPreview);
		
		//	Page id
		Element pageId = new Element(ThemesConstants.CON_PAGE_ID);
		pageId.setText(String.valueOf(theme.getIBPageID()));
		rootElements.add(pageId);
		
		//	Extra regions
		List<AdvancedProperty> extraRegions = theme.getExtraRegions();
		if (extraRegions.size() > 0) {
			Element regions = new Element(ThemesConstants.CON_EXTRA_REGIONS);
			AdvancedProperty extraRegion = null;
			Element region = null;
			Collection<Element> allRegions = new ArrayList<Element>();
			for (int i = 0; i < extraRegions.size(); i++) {
				extraRegion = extraRegions.get(i);
			
				region = new Element(ThemesConstants.CON_EXTRA_REGION);
				region.setAttribute(ThemesConstants.CON_ATT_EXTRA_REGION_PARENT, extraRegion.getId());
				region.setText(extraRegion.getValue());
				allRegions.add(region);
			}
			
			regions.setContent(allRegions);
			rootElements.add(regions);
		}
		
		//	Current built-in style
		String uriOfCurrentlyUsedBuiltInStyle = theme.getCurrentlyUsedBuiltInStyleUri();
		Element builtInStyleUri = new Element(ThemesConstants.CON_URI_OF_CURRENT_BUILT_IN_STYLE);
		builtInStyleUri.setText(uriOfCurrentlyUsedBuiltInStyle == null ? ThemesConstants.MINUS_ONE : uriOfCurrentlyUsedBuiltInStyle);
		rootElements.add(builtInStyleUri);
		
		root.setContent(rootElements);
		doc.setRootElement(root);
		
		String fileName = getPreparedThemeNameToUseInRepository(theme);
		try {
			return changer.uploadDocument(doc, theme.getLinkToBaseAsItIs(), fileName + ThemesConstants.IDEGA_THEME_INFO, theme, false);
		} catch (Exception e) {
			e.printStackTrace();
			return false;
		}
	}
	
	private void addColourFilesToConfig(Collection<Element> rootElements, List<String> files, String elementName) {
		Element filesElement = new Element(elementName);
		
		Element fileElement = null;
		Collection<Element> filesCollection = new ArrayList<Element>();
		for (String file: files) {
			fileElement = new Element(ThemesConstants.CON_COLOUR_FILE);
			fileElement.setText(file);
			
			filesCollection.add(fileElement);
		}
		
		if (filesCollection.isEmpty()) {
			return;
		}
		
		filesElement.setContent(filesCollection);
		rootElements.add(filesElement);
	}
	
	public String getPreparedThemeNameToUseInRepository(Theme theme) {
		if (theme == null) {
			return null;
		}
		
		return getPreparedThemeNameToUseInRepository(theme.getName());
	}
	
	public String getPreparedThemeNameToUseInRepository(String themeName) {
		if (themeName == null) {
			return null;
		}
		
		String fileName = StringHandler.removeCharacters(themeName, ContentConstants.SPACE, ContentConstants.UNDER);
		return StringHandler.replace(fileName, "'", CoreConstants.EMPTY);
	}
	
	public String[] getPageValues(Setting s, String value) {
		if (ThemesConstants.EMPTY.equals(s.getDefaultValue()) && value == null) {
			return new String[] {ThemesConstants.EMPTY};
		}
		String[] settingValues = null;
		if (s.getDefaultValue() != null) {
			if (!ThemesConstants.EMPTY.equals(s.getDefaultValue())) {
				settingValues = s.getDefaultValue().split(ThemesConstants.COMMA);
			}
		}
		if (settingValues == null) {
			 return new String[] {value.trim()};
		}
		String[] parsedValues = new String[settingValues.length + 1];
		for (int i = 0; i < settingValues.length; i++) {
			parsedValues[i] = settingValues[i];
		}
		parsedValues[parsedValues.length - 1] = value.trim();
		return parsedValues;
	}
	
	protected boolean closeInputStreamReader(InputStreamReader stream) {
		if (stream == null) {
			return true;
		}
		try {
			stream.close();
		} catch (IOException e) {
			e.printStackTrace();
			return false;
		}
		return true;
	}
	
	protected boolean closeBufferedReader(BufferedReader buffer) {
		if (buffer == null) {
			return true;
		}
		try {
			buffer.close();
		} catch (IOException e) {
			e.printStackTrace();
			return false;
		}
		return true;
	}
	
	public synchronized void addThemeToQueue(String linkToBase) {
		if (!themeQueue.contains(linkToBase)) {
			themeQueue.add(linkToBase);
		}
	}
	
	public synchronized void removeThemeFromQueue(String linkToBase) {
		Collection<Theme> themes = getAllThemes();
		Theme theme = null;
		for (Iterator<Theme> it = themes.iterator(); it.hasNext();) {
			theme = it.next();
			if (theme.getLinkToBaseAsItIs().startsWith(linkToBase)) {
				theme.setLoading(false);
			}
		}
		themeQueue.remove(linkToBase);
	}
	
	public ThemesEngine getThemesEngine() throws NullPointerException {
		if (themesEngine == null) {
			themesEngine = ELUtil.getInstance().getBean(ThemesEngine.SPRING_BEAN_IDENTIFIER);
		}
		
		return themesEngine;
	}

	public String getLastVisitedPage() {
		Object lastVisitedPage = WFUtil.invoke(ThemesManagerBean.THEMES_MANAGER_BEAN_ID, "getLastVisitedPageId");
		if (lastVisitedPage != null) {
			return lastVisitedPage.toString();
		}
		return null;
	}

	public void setLastVisitedPage(String lastVisitedPage) {
		WFUtil.invoke(ThemesManagerBean.THEMES_MANAGER_BEAN_ID, "setLastVisitedPageId", lastVisitedPage, String.class);
	}
	
	public String getLastUsedTheme() {
		String lastUsedTheme = getDefaultTheme();
		if (lastUsedTheme != null) {
			return lastUsedTheme;
		}
		List<Theme> themes = getAvailableThemes();
		if (themes == null) {
			return null;
		}
		Theme theme = null;
		boolean foundDefaultValue = false;
		for (int i = 0; (i < themes.size() && !foundDefaultValue); i++) {
			theme = themes.get(i);
			if (theme.getIBPageID() != -1) {
				foundDefaultValue = true;
				lastUsedTheme = String.valueOf(theme.getIBPageID());
			}
		}
		return lastUsedTheme;
	}
	
	public Theme getThemeByTemplateKey(String templateKey) {
		if (templateKey == null) {
			return null;
		}
		
		List<Theme> themes = getAvailableThemes();
		if (themes == null) {
			return null;
		}
		
		Theme theme = null;
		for (int i = 0; i < themes.size(); i++) {
			theme = themes.get(i);
			if (templateKey.equals(String.valueOf(theme.getIBPageID()))) {
				return theme;
			}
		}
		
		return null;
	}
	
	public void setLastUsedTheme(String templateId) {
		if (templateId == null || templateId.equals(ThemesConstants.MINUS_ONE)) {
			return;
		}
		IWMainApplicationSettings settings = ContentUtil.getBundle().getApplication().getSettings();
		try {
			settings.setProperty(ThemesConstants.LAST_USED_THEME, templateId);
		} catch (NumberFormatException e) {
			e.printStackTrace();
		}
	}
	
	public String getDefaultTheme() {
		IWMainApplicationSettings settings  = ContentUtil.getBundle().getApplication().getSettings();
		if (settings == null) {
			return null;
		}
		return settings.getProperty(ThemesConstants.LAST_USED_THEME);
	}
	
	private Document preparePageDocument(Document doc, List<String> articlesPaths, int pageID) {
		if (articlesPaths != null) {
			List<Element> articleViewers = getArticleViewerElements(doc);
			if (articleViewers != null) {
				if (articlesPaths.size() == articleViewers.size()) {
					Element articleViewer = null;
					String path = null;
					Attribute resourcePathValue = null;
					for (int i = 0; i < articleViewers.size(); i++) {
						articleViewer = articleViewers.get(i);
						path = articlesPaths.get(i);
						resourcePathValue = getArticleViewerResourcePathValueAttribute(articleViewer);
						if (resourcePathValue != null) {
							resourcePathValue.setValue(path);
						}
					}
				}
			}
		}
		
		if (!addIDsToModules(doc.getRootElement(), pageID)) {
			return null;
		}
		
		return doc;
	}
	
	@SuppressWarnings("unchecked")
	private List<Element> getArticleViewerElements(Document doc) {
		if (doc == null) {
			return null;
		}
		
		List<Element> articleViewers = new ArrayList<Element>();
		Object o = null;
		Element e = null;
		Attribute classAttribute = null;
		for (Iterator it = doc.getDescendants(); it.hasNext();) {
			o = it.next();
			if (o instanceof Element) {
				e = (Element) o;
				if (MODULE_ELEMENT_NAME.equals(e.getName())) {
					classAttribute = e.getAttribute(ELEMENT_CLASS_ATTRIBUTE);
					if (classAttribute != null) {
						if (classAttribute.getValue() != null) {
							if (classAttribute.getValue().equals(CoreConstants.getArticleItemViewerClass().getName())) {
								if (isArticleViewerWithoutResourcePath(e)) {
									articleViewers.add(e);
								}
							}
						}
					}
				}
			}
		}
		
		return articleViewers;
	}
	
	@SuppressWarnings("unchecked")
	private Attribute getArticleViewerResourcePathValueAttribute(Element articleViewer) {
		if (articleViewer == null) {
			return null;
		}
		List<Element> elements = articleViewer.getChildren();
		if (elements == null) {
			return null;
		}
		
		Attribute resourcePath = null;
		Element element = null;
		for (int j = 0; j < elements.size(); j++) {
			element = elements.get(j);
			resourcePath = element.getAttribute(ATTRIBUTE_NAME);
			if (resourcePath != null) {
				if (ATTRIBUTE_RESOURCE_PATH_VALUE.equals(resourcePath.getValue())) {
					return element.getAttribute(ATTRIBUTE_PROPERTY);
				}
			}
		}
		return null;
	}
	
	private boolean isArticleViewerWithoutResourcePath(Element articleViewer) {
		Attribute resourcePathValue = getArticleViewerResourcePathValueAttribute(articleViewer);
		if (resourcePathValue == null) {
			return true;
		}
		String value = resourcePathValue.getValue();
		if (value == null) {
			return true;
		}
		return ContentConstants.EMPTY.equals(value) ? true : false;
	}
	
	private String getTemplateForPage(String type, List<String> articlesPaths, String templateFile, int pageId) {
		if (!templateFile.startsWith(IWBundleResourceFilter.BUNDLES_STANDARD_DIR)) {
			templateFile = new StringBuffer(getWebRootWithoutContent()).append(templateFile).toString();
		}
		Document doc = getXMLDocument(templateFile, false, true);
		if (doc == null) {
			log.log(Level.WARNING, "Template file ("+templateFile+") wasn'tfound!");
			return null;
		}
		doc = preparePageDocument(doc, articlesPaths, pageId);
		try {
			return getThemeChanger().getXMLOutputter().outputString(doc);
		} catch (Exception e) {
			e.printStackTrace();
			return null;
		}
	}
	
	public String loadPageToSlide(String type, String templateFile, List<String> articlesPaths, int pageID) {
		if (type == null || templateFile == null) {
			return null;
		}
		
		ICPage page = getThemesService().getICPage(pageID);
		if (page == null) {
			return null;
		}
		
		String docContent = getTemplateForPage(type, articlesPaths, templateFile, pageID);
		if (docContent == null) {
			return null;
		}
		
		templateFile = getFileNameWithExtension(templateFile);
		if (templateFile == null) {
			return null;
		}
		
		String fullUrl = changeUploadFileName(ThemesConstants.PAGES_PATH_SLIDE + templateFile);
		String base = extractValueFromString(fullUrl, 0, fullUrl.lastIndexOf(ContentConstants.SLASH));
		if (!base.endsWith(ContentConstants.SLASH)) {
			base += ContentConstants.SLASH;
		}
		String changedFileName = extractValueFromString(fullUrl, fullUrl.lastIndexOf(ContentConstants.SLASH) + 1, fullUrl.length());

		try {
			getSlideService().uploadFileAndCreateFoldersFromStringAsRoot(base, changedFileName, docContent, ContentConstants.XML_MIME_TYPE, true);
		} catch (RemoteException e) {
			e.printStackTrace();
			return null;
		}
		
		return CoreConstants.WEBDAV_SERVLET_URI + base + changedFileName;
	}
	
	private boolean existInSlide(String path) {
		try {
			return getSlideService().getExistence(path);
		} catch (HttpException e) {
			e.printStackTrace();
		} catch (RemoteException e) {
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		}
		return false;
	}
	
	public String getFixedSlideFileName(String fileName) {
		if (fileName == null) {
			return null;
		}
		
		fileName = getPreparedThemeNameToUseInRepository(fileName);
		fileName = StringHandler.removeCharacters(fileName, ContentConstants.BRACKET_OPENING, ContentConstants.EMPTY);
		fileName = StringHandler.removeCharacters(fileName, ContentConstants.BRACKET_CLOSING, ContentConstants.EMPTY);
		
		return fileName;
	}
	
	private String changeUploadFileName(String fileName) {
		if (fileName == null) {
			return null;
		}
		
		String fileRoot = getFixedSlideFileName(fileName);
		String fileType = ThemesConstants.EMPTY;
		if (fileName.indexOf(CoreConstants.DOT) != -1) {
			fileRoot = extractValueFromString(fileName, 0, fileName.lastIndexOf(CoreConstants.DOT));
			fileType = getFileExtension(fileName);
		}
		
		StringBuffer path = new StringBuffer(fileRoot).append(ContentConstants.UNDER);
		path.append(getUniqueIdByNumberAndDate(IDEGA_PAGES_SCOPE)).append(CoreConstants.DOT).append(fileType);
		
		return path.toString();
	}
	
	public String changeFileUploadPath(String path) {
		if (path == null) {
			return null;
		}

		path = getFixedSlideFileName(path);
		StringBuffer tempPath = new StringBuffer(path);
		
		int i = 1;
		while (existInSlide(tempPath.toString())) {
			tempPath = new StringBuffer(path).append(i);
			i++;
		}
		path = tempPath.toString();
		return path;
	}
	
	protected int getRandomNumber(int maxValue) {
		int number;
		try {
			number = numberGenerator.nextInt(maxValue);
		} catch (IllegalArgumentException e) {
			e.printStackTrace();
			return 0;
		}
		return number;
	}
	
	public void removeLastUsedTheme(String templateID) {
		if (templateID == null) {
			return;
		}
		IWMainApplicationSettings settings  = ContentUtil.getBundle().getApplication().getSettings();
		String lastUsedTheme = settings.getProperty(ThemesConstants.LAST_USED_THEME);
		if (lastUsedTheme == null) {
			return;
		}
		if (templateID.equals(lastUsedTheme)) {
			settings.removeProperty(ThemesConstants.LAST_USED_THEME);
		}
	}
	
	public List<String> createArticle(String templateFile, int id) {
		if (templateFile == null) {
			return null;
		}
		if (id == -1) {
			return null;
		}
		
		Document doc = getXMLDocument(new StringBuffer(getWebRootWithoutContent()).append(templateFile).toString());
		List<Element> articleViewers = getArticleViewerElements(doc);
		if (articleViewers == null) {
			return null;
		}
		
		ICPage page = getThemesService().getICPage(id);
		if (page == null) {
			return null;
		}
		String uri = page.getDefaultPageURI();
		if (uri == null) {
			return null;
		}
		
		IWContext iwc = CoreUtil.getIWContext();
		if (iwc == null) {
			return null;
		}
		
		String language = getCurrentLanguage(iwc);

		if (uri.endsWith(ContentConstants.SLASH)) {
			uri = extractValueFromString(uri, 0, uri.lastIndexOf(ContentConstants.SLASH));
		}

		List<String> paths = new ArrayList<String>();
		StringBuffer file = null;
		StringBuffer base = null;
		String article = null;
		for (int i = 0; i < articleViewers.size(); i++) {
			file = new StringBuffer(language);
			file.append(CoreConstants.DOT).append(ThemesConstants.XML_EXTENSION);
			base = new StringBuffer(ContentConstants.ARTICLE_PATH_START);
			base.append(uri);
			if (uri.equals(ContentConstants.EMPTY)) {
				if (!base.toString().endsWith(ContentConstants.SLASH)) {
					base.append(ContentConstants.SLASH);
				}
				base.append(ROOT_PAGE_ARTICLE);
			}
			base.append(getSlideService().createUniqueFileName(ContentConstants.ARTICLE_SCOPE));
			base.append(RESOURCE_PATH_END);
			
			article = getArticleDocument(language, base.toString(), iwc);
			
			base.append(CoreConstants.SLASH);
			try {
				getSlideService().uploadFileAndCreateFoldersFromStringAsRoot(base.toString(), file.toString(), article, ContentConstants.XML_MIME_TYPE, true);
				paths.add(base.toString());
			} catch (RemoteException e) {
				e.printStackTrace();
				return null;
			}
		}
		
		return paths;
	}
	
	private String getArticleDocument(String language, String uri, IWContext iwc) {
		String article = getArticle();
		StringBuffer summary = new StringBuffer();
		if (article.length() >= 200) {
			summary.append(article.substring(0, 200)).append(CoreConstants.DOT).append(CoreConstants.DOT);
			summary.append(CoreConstants.DOT);
		}
		else {
			summary = new StringBuffer(article);
		}
		String server = getFullServerName(iwc);
		StringBuffer link = new StringBuffer(server);
		link.append(ContentConstants.PAGES_START_URI);
		link.append(uri);
		String linkToComments = getArticleCommentLink(iwc, uri);
		String user = iwc.getCurrentUser().getName();
		return getFeedBean().getFeedEntryAsXML(ThemesConstants.ARTICLE_TITLE, server, null, ThemesConstants.ARTICLE_TITLE,
				new Timestamp(System.currentTimeMillis()), null, summary.toString(), article, user, language, null, link.toString(),
				null, null, linkToComments, ThemesConstants.MINUS_ONE);
	}
	
	public String getArticleCommentLink(IWContext iwc, String pageURI) {
		StringBuffer commentPath = new StringBuffer(ContentConstants.ARTICLE_PATH_START);
		if (pageURI == null) {
			commentPath.append(ContentConstants.SLASH).append(ContentUtil.getYearMonthPath(iwc));
			commentPath.append(ContentConstants.SLASH);
		} 
		else {
			String articleEnd = new StringBuffer(CoreConstants.DOT).append(CoreConstants.ARTICLE_FILENAME_SCOPE).toString();
			if (pageURI.indexOf(commentPath.toString()) != -1 && pageURI.endsWith(articleEnd)) {
				String articlePath = pageURI.substring(pageURI.indexOf(commentPath.toString()) + commentPath.length(), pageURI.indexOf(articleEnd));
				commentPath.append(articlePath);
				return getFinishedCommentsLink(commentPath);
			}
			else {
				if (pageURI.equals(ContentConstants.SLASH)) {
					pageURI = new StringBuffer(pageURI).append(ROOT_PAGE_ARTICLE).toString();
				}
				if (pageURI.endsWith(ContentConstants.SLASH)) {
					pageURI = pageURI.substring(0, pageURI.lastIndexOf(ContentConstants.SLASH));
				}
				commentPath.append(pageURI);
			}
		}
		commentPath.append(getSlideService().createUniqueFileName(ContentConstants.COMMENT_SCOPE));
		
		return getFinishedCommentsLink(commentPath);
	}
	
	private String getFinishedCommentsLink(StringBuffer uri) {
		if (uri == null) {
			return null;
		}
		
		uri.append(ContentConstants.COMMENT_PREFIX).append(ContentConstants.SLASH).append(ContentConstants.COMMENT_SCOPE).append(CoreConstants.DOT);
		uri.append(ThemesConstants.XML_EXTENSION);
		
		return uri.toString();
	}
	
	@SuppressWarnings("unchecked")
	private boolean addIDsToModules(Element root, int pageID) {
		if (root == null || pageID < 0) {
			return false;
		}
		Iterator allElements = root.getDescendants();
		if (allElements == null) {
			return false;
		}
		Element e = null;
		Object o = null;
		Attribute moduleIdAttribute = null;
		
		String id = "id";
		String pageKey = String.valueOf(pageID);
		String moduleID = null;
		
		int icObjectId = -1;
		
		ICObjectInstanceHome icoiHome = null;
		ICObjectHome icoHome = null;
		ICObjectInstance instance = null;
		
		try {
			icoiHome = (ICObjectInstanceHome) IDOLookup.getHome(ICObjectInstance.class);
			icoHome = (ICObjectHome)IDOLookup.getHome(ICObject.class);
			for (Iterator it = allElements; it.hasNext(); ) {
				o = it.next();
				moduleID = null;
				icObjectId = -1;
				if (o instanceof Element) {
					e = (Element) o;
					if (MODULE_ELEMENT_NAME.equals(e.getName())) {
						icObjectId = getICObjectId(e.getAttributeValue(ELEMENT_CLASS_ATTRIBUTE), icoHome);
						if (icObjectId == -1) {
							log.log(Level.WARNING, "Didn't get ICObject for: "+e.getAttributeValue(ELEMENT_CLASS_ATTRIBUTE));
							log.log(Level.WARNING, "Generating unique module id");
							moduleID = getUniqueIdByNumberAndDate(MODULE_ID_SCOPE);
							while (moduleIds.contains(moduleID)) {
								moduleID = getUniqueIdByNumberAndDate(MODULE_ID_SCOPE);
							}
							moduleIds.add(moduleID);
							moduleID = new StringBuffer(ICObjectBusiness.UUID_PREFIX).append(moduleID).toString();
						}
						else {
							instance = icoiHome.create();
							instance.setICObjectID(icObjectId);
							instance.setIBPageByKey(pageKey);
							instance.store();
							moduleID = new StringBuffer(ICObjectBusiness.UUID_PREFIX).append(instance.getUniqueId()).toString();
						}
						moduleIdAttribute = e.getAttribute(id);
						if (moduleIdAttribute != null) {
							moduleIdAttribute.setValue(moduleID);
						}
						else {
							log.log(Level.WARNING, "Didn't find module id attribute for: " + e.getAttributeValue(ELEMENT_CLASS_ATTRIBUTE));
						}
					}
				}
			}
		} catch (Exception ex) {
			ex.printStackTrace();
			return false;
		}
		return true;
	}
	
	private int getICObjectId(String className, ICObjectHome icoHome) {
		if (className == null || icoHome == null) {
			return -1;
		}
		
		ICObject object = null;
		try {
			object = icoHome.findByClassName(className);
		} catch (FinderException e) {
			e.printStackTrace();
			return -1;
		}
		String key = object.getPrimaryKey().toString();
		try {
			return Integer.valueOf(key);
		} catch (NumberFormatException e) {
			e.printStackTrace();
			return -1;
		}
	}
	
	private String getArticle() {
		StringBuffer article = new StringBuffer();
		article.append(getArticleImageTag());
		article.append(ThemesConstants.DUMMY_ARTICLES.get(getRandomNumber(ThemesConstants.DUMMY_ARTICLES.size())));
		return article.toString();
	}
	
	private String getArticleImageTag() {
		StringBuffer img = new StringBuffer();
		img.append("<img vspace=\"0\" hspace=\"5px\" border=\"0\" align=\"");
		img.append(ThemesConstants.IMAGE_POSITIONS.get(getRandomNumber(ThemesConstants.IMAGE_POSITIONS.size())));
		img.append("\" src=\"");
		img.append(ThemesConstants.BASE_THEME_IMAGES);
		img.append(ThemesConstants.THEME_IMAGES.get(getRandomNumber(ThemesConstants.THEME_IMAGES.size())));
		img.append("\" />");
		return img.toString();
	}
	
	public String getFullServerName(IWContext iwc) {
		StringBuffer server = new StringBuffer();
		if (iwc == null) {
			iwc = CoreUtil.getIWContext();
		}
		
		if (iwc == null) {
			return getWebRootWithoutContent();
		}
		else {
			try {
				ICDomain cachedDomain = iwc.getApplicationContext().getDomain();
				if (cachedDomain == null) {
					return getWebRootWithoutContent();
				}
				else {
					server.append(cachedDomain.getServerProtocol()).append("://").append(cachedDomain.getServerName());
				}
			} catch (Exception e) {
				e.printStackTrace();
				return getWebRootWithoutContent();
			}
		}
		return server.toString();
	}
	
	public boolean setNewLinkInArticleFile(IWContext iwc, String link, String language, String baseDirectory, String pageUri) {
		if (iwc == null || link == null || language == null || baseDirectory == null || pageUri == null) {
			return false;
		}
		
		StringBuffer fileName = new StringBuffer(language).append(CoreConstants.DOT).append(ThemesConstants.XML_EXTENSION);
		StringBuffer articleLink = new StringBuffer(link).append(fileName.toString());
		Document d = getXMLDocument(articleLink.toString());
		if (d == null) {
			return false;
		}
		Element root = d.getRootElement();
		Namespace atom = Namespace.getNamespace("http://www.w3.org/2005/Atom");
		Element entry = root.getChild("entry", atom);
		if (entry == null) {
			return false;
		}
		Element linkToPage = entry.getChild("link", atom);
		if (linkToPage == null) {
			return false;
		}
		Attribute href = linkToPage.getAttribute("href");
		if (href == null) {
			return false;
		}
		StringBuffer newLink = new StringBuffer(getFullServerName(iwc)).append(ContentConstants.PAGES_START_URI).append(pageUri);
		href.setValue(newLink.toString());
		Element id = entry.getChild("id", atom);
		if (id != null) {
			id.setText(newLink.toString());
		}
		try {
			getSlideService(iwc).uploadFileAndCreateFoldersFromStringAsRoot(baseDirectory, fileName.toString(), getThemeChanger().getXMLOutputter().outputString(d), ContentConstants.XML_MIME_TYPE, true);
		} catch (Exception e) {
			e.printStackTrace();
			return false;
		}
		return true;
	}
	
	private boolean existFileInSlide(String path, boolean printError) {
		if (path == null) {
			return false;
		}
		InputStream stream = null;
		StringBuffer url = null;
		if (path.startsWith(CoreConstants.WEBDAV_SERVLET_URI)) {
			url = new StringBuffer(getWebRootWithoutContent());
		}
		else {
			url = new StringBuffer(getFullWebRoot());
		}
		stream = getInputStream(url.append(path).toString());
		if (stream == null) {
			return false;
		}
		closeInputStream(stream);
		return true;
	}
	
	public boolean existFileInSlide(String path) {
		return existFileInSlide(path, false);
	}
	
	public String getUniqueIdByNumberAndDate(String scope) {
		StringBuffer id = new StringBuffer();
		id.append(getRandomNumber(Integer.MAX_VALUE)).append(getSlideService().createUniqueFileName(scope));
		return id.toString();
	}
	
	public String getLocalizedText(String key) {
		try {
			return ContentUtil.getBundle().getLocalizedString(key);
		} catch (Exception e) {
			e.printStackTrace();
			return key;
		}
	}
	
	public String getCurrentLanguage(IWContext iwc) {
		if (iwc == null) {
			iwc = CoreUtil.getIWContext();
		}
		if (iwc == null) {
			return Locale.ENGLISH.getLanguage();
		}
		Locale l = iwc.getCurrentLocale();
		if (l == null) {
			return Locale.ENGLISH.getLanguage();
		}
		return l.getLanguage();
	}
	
	protected void addLoadedTheme(String id) {
		if (loadedThemes.contains(id)) {
			return;
		}
		loadedThemes.add(id);
	}
	
	public int getLoadedThemesCount() {
		return loadedThemes.size();
	}
	
	public boolean isCheckedFromSlide() {
		return checkedFromSlide;
	}
	
	public boolean clearVariationFromCache(String themeID) {
		if (themeID == null) {
			return false;
		}
		Theme theme = getTheme(themeID);
		if (theme == null) {
			return false;
		}
		List<String> keys = theme.getStyleVariationsCacheKeys();
		if (keys == null) {
			return false;
		}
		IWContext iwc = CoreUtil.getIWContext();
		if (iwc == null) {
			return false;
		}
		
		//	Removing Block from cache
		for (int i = 0; i < keys.size(); i++) {
			getThemesService().getBuilderService().removeBlockObjectFromCache(iwc, keys.get(i));
		}
		
		//	Removing cache keys
		theme.clearStyleVariationsCacheKeys();
		
		//	Removing rendered variations from cache
		try {
			getThemesEngine().clearVariationFromCache(themeID, iwc);
		} catch(Exception e) {
			e.printStackTrace();
			return false;
		}
		
		return true;
	}
	
	/**
	 * Generates big and small previews for single theme
	 * @param theme
	 * @param useDraft
	 * @param isJpg
	 * @param quality
	 * @return true - success, false - failure
	 */
	protected boolean generatePreviewsForTheme(Theme theme, boolean useDraft, boolean isJpg, float quality, boolean generateOnlyBigPreview) {
		String url = getFullWebRoot();
		String bigPreviewName = null;
		String smallPreviewName = new StringBuffer(theme.getName()).append(ThemesConstants.THEME_SMALL_PREVIEW).toString();
		if (useDraft) {
			url = new StringBuffer(url).append(theme.getLinkToDraft()).toString();
			bigPreviewName = new StringBuffer(theme.getName()).append(ThemesConstants.DRAFT_PREVIEW).toString();
		}
		else {
			url = new StringBuffer(url).append(theme.getLinkToSkeleton()).toString();
			bigPreviewName = new StringBuffer(theme.getName()).append(ThemesConstants.THEME_PREVIEW).toString();
		}
		
		List<Dimension> dimensions = new ArrayList<Dimension>(2);
		dimensions.add(new Dimension(ThemesConstants.PREVIEW_WIDTH, ThemesConstants.PREVIEW_HEIGHT));
		if (!generateOnlyBigPreview) {
			dimensions.add(new Dimension(ThemesConstants.SMALL_PREVIEW_WIDTH, ThemesConstants.SMALL_PREVIEW_HEIGHT));
		}
	
		ImageGenerator imageGenerator = getImageGenerator(null);
		
		List<BufferedImage> images = imageGenerator.generatePreviews(url, dimensions, isJpg, quality);
		if (images == null) {
			return false;
		}
		
		String extension = imageGenerator.getFileExtension();
		String mimeType = new StringBuffer(ThemesConstants.DEFAULT_MIME_TYPE).append(extension).toString();
		bigPreviewName = new StringBuffer(bigPreviewName).append(CoreConstants.DOT).append(extension).toString();
		smallPreviewName = new StringBuffer(smallPreviewName).append(CoreConstants.DOT).append(extension).toString();
		
		BufferedImage image = null;
		InputStream stream = null;
		IWSlideService slide = getSlideService();
		for (int i = 0; i < images.size(); i++) {
			image = images.get(i);
			stream = imageGenerator.getImageInputStream(image, extension);
			try {
				if (image.getWidth() >= ThemesConstants.PREVIEW_WIDTH) {	// Is it a big image?
					if (slide.uploadFileAndCreateFoldersFromStringAsRoot(theme.getLinkToBaseAsItIs(), bigPreviewName, stream, mimeType, true)) {
						if (useDraft) {
							theme.setLinkToDraftPreview(bigPreviewName);
						}
						else {
							theme.setLinkToThemePreview(bigPreviewName);
						}
					}
				}
				else {	//	Small image
					if (slide.uploadFileAndCreateFoldersFromStringAsRoot(theme.getLinkToBaseAsItIs(), smallPreviewName, stream, mimeType, true)) {
						theme.setLinkToSmallPreview(smallPreviewName);
					}
				}
			} catch (RemoteException e) {
				e.printStackTrace();
				return false;
			} finally {
				closeInputStream(stream);
			}
		}
		
		return true;
	}
	
	public String getThemeColourFileName(Theme theme, String customName, String file, boolean markAsOriginalFile) {
		if (file == null) {
			return null;
		}
		
		if (customName == null) {
			if (theme != null) {
				customName = theme.getName();
			}
			if (customName == null) {
				customName = CoreConstants.EMPTY;
			}
		}
		customName = StringHandler.convertToUrlFriendly(customName);
	
		String cssFileName = file;
		if (file.indexOf(CoreConstants.DOT) != -1) {
			cssFileName = file.substring(0, file.lastIndexOf(CoreConstants.DOT));
		}
		
		StringBuffer name = new StringBuffer(cssFileName);
		if (!(name.toString().endsWith(customName))) {
			name.append(CoreConstants.UNDER).append(customName);
		}
		if (markAsOriginalFile) {
			name.append(CoreConstants.UNDER).append("original");
		}
		name.append(CoreConstants.DOT).append(getFileExtension(file));
		return name.toString();
	}
	
	public void addPredefinedThemeStyle(String uri) {
		if (predefinedThemeStyles.contains(uri)) {
			return;
		}
		
		predefinedThemeStyles.add(uri);
	}
	
	public List<String> getPredefinedThemeStyles() {
		return predefinedThemeStyles;
	}
	
	protected String getBuiltInThemeStyleId(Theme theme) {
		String id = String.valueOf(getRandomNumber(Integer.MAX_VALUE));
		while (theme.getBuiltInThemeStyle(id) != null) {
			id = String.valueOf(getRandomNumber(Integer.MAX_VALUE));
		}
		return id;
	}
}