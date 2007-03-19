package com.idega.content.themes.helpers;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.Reader;
import java.io.UnsupportedEncodingException;
import java.net.MalformedURLException;
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
import java.util.SortedMap;
import java.util.TreeMap;

import javax.ejb.CreateException;
import javax.ejb.FinderException;
import javax.faces.context.FacesContext;

import org.apache.commons.httpclient.HttpException;
import org.apache.commons.httpclient.HttpURL;
import org.apache.commons.httpclient.URIException;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.jdom.Attribute;
import org.jdom.Document;
import org.jdom.Element;
import org.jdom.JDOMException;
import org.jdom.Namespace;
import org.jdom.input.SAXBuilder;
import org.xml.sax.EntityResolver;

import com.idega.business.IBOLookup;
import com.idega.business.IBOLookupException;
import com.idega.content.bean.ContentItemFeedBean;
import com.idega.content.business.ContentConstants;
import com.idega.content.business.ContentSearch;
import com.idega.content.business.ContentUtil;
import com.idega.content.themes.bean.ThemesManagerBean;
import com.idega.content.themes.business.ThemesEngine;
import com.idega.content.themes.business.ThemesService;
import com.idega.core.builder.data.ICDomain;
import com.idega.core.builder.data.ICPage;
import com.idega.core.component.business.ICObjectBusiness;
import com.idega.core.component.data.ICObject;
import com.idega.core.component.data.ICObjectHome;
import com.idega.core.component.data.ICObjectInstance;
import com.idega.core.component.data.ICObjectInstanceHome;
import com.idega.core.search.business.SearchResult;
import com.idega.data.IDOLookup;
import com.idega.graphics.Generator;
import com.idega.graphics.ImageGenerator;
import com.idega.idegaweb.IWMainApplication;
import com.idega.idegaweb.IWMainApplicationSettings;
import com.idega.presentation.IWContext;
import com.idega.repository.data.Singleton;
import com.idega.slide.business.IWSlideService;
import com.idega.webface.WFUtil;

public class ThemesHelper implements Singleton {
	
	private static Log log = LogFactory.getLog(ThemesHelper.class);
	
	private volatile static ThemesHelper helper = null;
	private volatile Generator generator = null;
	private volatile ThemeChanger changer = null;
	private volatile ThemeStyleVariations variations = null;
	private volatile ThemesPropertiesExtractor extractor = null;
	private volatile ThemesLoader loader = null;
	private volatile IWSlideService service = null;
	private volatile ThemesService themesService = null;
	private volatile ThemesEngine themesEngine = null;
	private volatile ContentItemFeedBean feedBean = null;
	
	private SortedMap <String, Theme> themes = null;
	private Map <String, Setting> themeSettings = null;
	private Map <String, Setting> pageSettings = null;
	private Map <String, Document> pages = null;
	private List <String> themeQueue = null;
	private List <String> urisToThemes = null;
	
	private boolean checkedFromSlide = false;
	private boolean loadedThemeSettings = false;
	private boolean loadedPageSettings = false;
	
	private String fullWebRoot; // For cache
	private String webRoot;
	
	private static final String RESOURCE_PATH_END = ThemesConstants.DOT + "article";
	private static final String ATTRIBUTE_NAME = "property";
	private static final String ATTRIBUTE_PROPERTY = "value";
	private static final String ROOT_PAGE_ARTICLE = "root_page_article";
	
	private Random numberGenerator = null;
	
	private ThemesHelper(boolean canUseSlide) {
		themes = Collections.synchronizedSortedMap(new TreeMap <String, Theme> ());
		themeSettings = Collections.synchronizedMap(new TreeMap<String, Setting>());
		pageSettings = new HashMap <String, Setting> ();
		pages = new HashMap <String, Document> ();
		themeQueue = new ArrayList <String> ();
		urisToThemes = new ArrayList <String> ();
		numberGenerator = new Random();
		if (canUseSlide) {
			searchForThemes();
		}
	}
	
	public static ThemesHelper getInstance() {
		if (helper == null) {
			synchronized (ThemesHelper.class) {
				if (helper == null) {
					helper = new ThemesHelper(true);
				}
			}
		}
		return helper;
	}
	
	public static ThemesHelper getInstance(boolean searchForThemes) {
		if (helper == null) {
			synchronized (ThemesHelper.class) {
				if (helper == null) {
					helper = new ThemesHelper(searchForThemes);
				}
			}
		}
		return helper;
	}
	
	protected Generator getImageGenerator() {
		if (generator == null) {
			synchronized (ThemesHelper.class) {
				if (generator == null) {
					generator = new ImageGenerator();
				}
			}
		}
		return generator;
	}
	
	public ThemeChanger getThemeChanger() {
		if (changer == null) {
			synchronized (ThemesHelper.class) {
				if (changer == null) {
					changer = new ThemeChanger();
				}
			}
		}
		return changer;
	}
	
	public ThemeStyleVariations getThemeStyleVariations() {
		if (variations == null) {
			synchronized (ThemesHelper.class) {
				if (variations == null) {
					variations = new ThemeStyleVariations();
				}
			}
		}
		return variations;
	}
	
	public ThemesPropertiesExtractor getThemesPropertiesExtractor() {
		if (extractor == null) {
			synchronized (ThemesHelper.class) {
				if (extractor == null) {
					extractor = new ThemesPropertiesExtractor();
				}
			}
		}
		return extractor;
	}
	
	protected IWSlideService getSlideService() {
		return getSlideService(null);
	}
	
	public IWSlideService getSlideService(IWContext iwc) {
		if (service == null) {
			synchronized (ThemesHelper.class) {
				if (iwc == null) {
					iwc = getIWContext();
				}
				if (service == null) {
					try {
						service = (IWSlideService) IBOLookup.getServiceInstance(iwc, IWSlideService.class);
					} catch (IBOLookupException e) {
						log.error(e);
					}
				}
			}
		}
		return service;
	}
	
	public ThemesLoader getThemesLoader() {
		if (loader == null) {
			synchronized (ThemesHelper.class) {
				if (loader == null) {
					loader = new ThemesLoader(this);
				}
			}
		}
		return loader;
	}
	
	private ContentItemFeedBean getFeedBean() {
		if (feedBean == null) {
			synchronized (ThemesHelper.class) {
				if (feedBean == null) {
					feedBean = new ContentItemFeedBean(null, ContentItemFeedBean.FEED_TYPE_ATOM_1);
				}
			}
		}
		return feedBean;
	}
	
	public void searchForThemes() {
		if (checkedFromSlide) {
			return;
		}
		checkedFromSlide = true;
		ContentSearch search = new ContentSearch(IWMainApplication.getDefaultIWMainApplication());
		Collection results = search.doSimpleDASLSearch(ThemesConstants.THEME_SEARCH_KEY, ContentConstants.CONTENT + ThemesConstants.THEMES_PATH);
		if (results == null) {
			log.error("ContentSearch.doSimpleDASLSearch returned results Collection, which is null: " + results);
			return;
		}
		List <String> urisToThemes = new ArrayList<String>();
		String uri = null;
		Object o = null;
		for (Iterator it = results.iterator(); it.hasNext(); ) {
			o = it.next();
			if (o instanceof SearchResult) {
				uri = ((SearchResult) o).getSearchResultURI();
				if (isCorrectFile(uri)) {
					urisToThemes.add(uri);
				}
			}
		}
		checkedFromSlide = getThemesLoader().loadThemes(urisToThemes, false, true);
	}
	
	protected String getFileName(String uri) {
		String name = null;
		int begin = uri.lastIndexOf(ContentConstants.SLASH);
		int end = uri.lastIndexOf(ThemesConstants.DOT);
		if (begin == -1) {
			name = extractValueFromString(uri, 0, end);
		}
		else {
			name = extractValueFromString(uri, begin + 1, end);
		}
		return name;
	}
	
	protected String getFileNameWithExtension(String uri) {
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
		String type = null;
		int begin = uri.lastIndexOf(ThemesConstants.DOT);
		if (begin != -1) {
			type = uri.substring(begin + 1).toLowerCase();
		}
		return type;
	}
	
	protected List getFiles(String folderURI) {
		List files = null;
		try {
			files = getSlideService().getChildPathsExcludingFoldersAndHiddenFiles(folderURI);
		} catch(RemoteException e) {
			e.printStackTrace();
		}
		return files;
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
			log.error(e);
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
			log.error(e);
			return null;
		}
		try {
			fullWebRoot = root.getURI();
		} catch (URIException e) {
			log.error(e);
			return null;
		}
		return fullWebRoot;
	}
	
	protected boolean isCorrectFile(String fileName, String nameTemplate) {
		if (fileName == null || nameTemplate == null) {
			return false;
		}
		return fileName.equals(nameTemplate);
	}
	
	public boolean isCorrectFile(String fileName) {
		boolean result = false;
		if (fileName == null) {
			return result;
		}
		if (isSystemFile(fileName)) {
			return false;
		}
		if (isDraft(fileName)) {
			return false;
		}
		
		int index = fileName.lastIndexOf(ThemesConstants.DOT);
		if (index == -1) {
			return result;
		}
		String fileExtension = fileName.substring(index + 1).toLowerCase();
		for (int i = 0; (i < ThemesConstants.FILTER.size() && !result); i++) {
			if (isCorrectFile(fileExtension, ThemesConstants.FILTER.get(i))) {
				result = true;
			}
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
		if (getFileNameWithExtension(fileName).startsWith(ThemesConstants.DOT)) {
			return true;
		}
		return false;
	}
	
	protected boolean isPropertiesFile(String uri) {
		if (ThemesConstants.PROPERTIES_FILES.contains(uri)) {
			return true;
		}
		return false;
	}
	
	protected void addTheme(Theme theme) {
		themes.put(theme.getId(), theme);
	}
	
	public Collection <Theme> getThemesCollection() {
		return themes.values();
	}
	
	public List<Theme> getSortedThemes() {
		List<Theme> sorted = new ArrayList<Theme>();
		List<Theme> notSorted = new ArrayList<Theme>(getThemesCollection());
		if (notSorted == null) {
			return sorted;
		}
		
		SortedMap<String, Theme> sortedMap = Collections.synchronizedSortedMap(new TreeMap<String, Theme>());
		for (int i = 0; i < notSorted.size(); i++) {
			sortedMap.put(notSorted.get(i).getName(), notSorted.get(i));
		}
		sorted = new ArrayList<Theme>(sortedMap.values());
		return sorted;
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
		if (url == null) {
			return null;
		}
		
		return getXMLDocument(getInputStream(url));
	}
	
	public Document getXMLDocument(InputStream stream) {
		if(stream == null){
			log.info("Stream is null");
			return null;
		}

		Reader r = null;
		try {
			r = new InputStreamReader(stream, ThemesConstants.ENCODING);
		} catch (UnsupportedEncodingException e) {
			log.error(e);
			return null;
		}
		
		Document document = null;
		
		SAXBuilder builder = new SAXBuilder(false);
		EntityResolver resolver = null;
		//Creating our EntityResolver to avoid IOException trying to load DTD file, defined in every Theme.plist file:
		//<!DOCTYPE plist PUBLIC "-//Apple Computer//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
		resolver = new ThemesEntityResolver();
		builder.setEntityResolver(resolver);
		try {
			document = builder.build(r);
		} catch (JDOMException e) {
			log.info("JDOM exception");
			log.error(e);
			return null;
		} catch (IOException e) {
			log.info("IOException trying to build a JDOM Document");
			log.error(e);
			return null;
		} finally {
			closeInputStream(stream);
		}
		try {
			r.close();
		} catch (IOException e) {
			log.error(e);
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
	
	public Theme getTheme(String themeID) {
		if (themeID == null) {
			return null;
		}
		return themes.get(themeID);
	}
	
	public void removeTheme(String uri, String themeID) {
		if (uri == null || themeID == null) {
			return;
		}
		urisToThemes.remove(uri);
		themes.remove(themeID);
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
			closeInputStream(stream);
			return;
		}
		loadSettings(themeSettings, getXMLDocument(stream));
		loadedThemeSettings = true;
	}
	
	public void loadPageSettings(String url) {
		if (loadedPageSettings) {
			return;
		}
		loadSettings(pageSettings, getXMLDocument(getInputStream(url)));
		loadedPageSettings = true;
	}
	
	private void loadSettings(Map <String, Setting> settings, Document doc) {
		if (doc == null) {
			return;
		}
		Element root = doc.getRootElement();
		if (root == null) {
			return;
		}
		List keys = root.getChildren();
		if (keys == null) {
			return;
		}
		Element key = null;
		Setting setting = null;
		for (int i = 0; i < keys.size(); i++) {
			key = (Element) keys.get(i);
			setting = new Setting();
			
			setting.setCode(key.getChildTextNormalize(ThemesConstants.SETTING_CODE));
			setting.setLabel(key.getChildTextNormalize(ThemesConstants.SETTING_LABEL));
			setting.setDefaultValue(key.getChildTextNormalize(ThemesConstants.SETTING_DEFAULT_VALUE));
			setting.setType(key.getChildTextNormalize(ThemesConstants.SETTING_TYPE));
			setting.setMethod(key.getChildTextNormalize(ThemesConstants.SETTING_METHOD));
			
			settings.put(setting.getCode(), setting);
		}
	}
	
	protected InputStream getInputStream(String link) {
		InputStream is = null;
        try {
        	URL url = getUrl(link);
        	if (url == null) {
        		return null;
        	}
            is = url.openStream();
        } catch (java.net.MalformedURLException e) {
        	log.info("Error getting: " + link);
            log.error(e);
        } catch (java.io.IOException e) {
        	log.info("Error getting: " + link);
            log.error(e);
        }
        return is;
	}
	
	protected boolean closeInputStream(InputStream is) {
		try {
			is.close();
		} catch (IOException e) {
			log.error(e);
			return false;
		}
		return true;
	}
	
	protected boolean closeOutputStream(OutputStream os) {
		try {
			os.close();
		} catch (IOException e) {
			log.error(e);
			return false;
		}
		return true;
	}
	
	protected URL getUrl(String link) {
		URL url = null;
		try {
			url = new URL(link);
		} catch (MalformedURLException e) {
			log.error(e);
		}
		return url;
	}
	
	public String encode(String value, boolean fullyEncode) {
		if (value == null) {
			return null;
		}
		if (fullyEncode) {
			try {
				value = URLEncoder.encode(value, ThemesConstants.ENCODING);
			} catch (UnsupportedEncodingException e) {
				log.error(e);
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
					encoded.append(URLEncoder.encode(fileParts[i], ThemesConstants.ENCODING));
				} catch (UnsupportedEncodingException e) {
					log.error(e);
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
				value = URLDecoder.decode(value, ThemesConstants.ENCODING);
			} catch (UnsupportedEncodingException e) {
				log.error(e);
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
					encoded.append(URLDecoder.decode(fileParts[i], ThemesConstants.ENCODING));
				} catch (UnsupportedEncodingException e) {
					log.error(e);
					return url;
				}
				if (i + 1 < fileParts.length) {
					encoded.append(ContentConstants.SLASH);
				}
			}
		}
		return encoded.toString();
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
		String extension = helper.getFileExtension(uriToImage).toLowerCase();
		String mimeType = ThemesConstants.DEFAULT_MIME_TYPE + extension;
		InputStream input = null;
		
		// Reducing and encoding original image, saving as new image
		input = getInputStream(getFullWebRoot() + theme.getLinkToBase() + encodedUriToImage);
		String newName = theme.getName() + ThemesConstants.THEME_SMALL_PREVIEW + ThemesConstants.DOT + extension;
		getImageGenerator().encodeAndUploadImage(theme.getLinkToBaseAsItIs(), newName, mimeType, input, ThemesConstants.SMALL_PREVIEW_WIDTH, ThemesConstants.SMALL_PREVIEW_HEIGHT);
		theme.setLinkToSmallPreview(newName);
		closeInputStream(input);
		
		return true;
	}
	
	public ThemesService getThemesService() {
		if (themesService == null) {
			synchronized (ThemesHelper.class) {
				try {
					themesService = (ThemesService) IBOLookup.getServiceInstance(getIWContext(), ThemesService.class);
				} catch (IBOLookupException e) {
					log.error(e);
				}
			}
		}
		return themesService;
	}

	protected boolean createThemeConfig(Theme theme) {
		Document doc = new Document();
		Element root = new Element(ThemesConstants.CON_THEME);
		Collection <Element> rootElements = new ArrayList<Element>();
		
		Element name = new Element(ThemesConstants.CON_NAME);
		name.setText(theme.getName());
		rootElements.add(name);
		
		Element styles = new Element(ThemesConstants.CON_STYLES);
		Collection <Element> stylesElements = new ArrayList<Element>();
		
		List <ThemeStyleGroupMember> enabled = getThemeChanger().getEnabledStyles(theme);
		ThemeStyleGroupMember member = null;
		
		Element style = null;
		Collection <Element> styleElements = null;
		Element groupName = null;
		Element variation = null;
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

			style.setContent(styleElements);
			stylesElements.add(style);
		}
		styles.setContent(stylesElements);
		rootElements.add(styles);
		
		Element preview = new Element(ThemesConstants.CON_PREVIEW);
		preview.setText(theme.getLinkToThemePreview());
		rootElements.add(preview);
		
		Element smallPreview = new Element(ThemesConstants.CON_SMALL_PREVIEW);
		smallPreview.setText(theme.getLinkToSmallPreview());
		rootElements.add(smallPreview);
		
		Element pageId = new Element(ThemesConstants.CON_PAGE_ID);
		pageId.setText(String.valueOf(theme.getIBPageID()));
		rootElements.add(pageId);
		
		root.setContent(rootElements);
		doc.setRootElement(root);
		return getThemeChanger().uploadDocument(doc, theme.getLinkToBaseAsItIs(), removeSpaces(theme.getName()) + ThemesConstants.IDEGA_THEME_INFO, theme, false);
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
			log.error(e);
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
			log.error(e);
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
		List <Theme> themes = new ArrayList<Theme>(getThemesCollection());
		Theme theme = null;
		for (int i = 0; i < themes.size(); i++) {
			theme = themes.get(i);
			if (theme.getLinkToBaseAsItIs().startsWith(linkToBase)) {
				theme.setLoading(false);
			}
		}
		themeQueue.remove(linkToBase);
	}
	
	public ThemesEngine getThemesEngine() {
		if (themesEngine == null) {
			synchronized (ThemesHelper.class) {
				try {
					themesEngine = (ThemesEngine) IBOLookup.getServiceInstance(getIWContext(), ThemesEngine.class);
				} catch (IBOLookupException e) {
					log.error(e);
				}
			}
		}
		return themesEngine;
	}
	
	public String removeSpaces(String value) {
		if (value == null) {
			return null;
		}
		value = value.trim();
		while (value.indexOf(ThemesConstants.SPACE) != -1) {
			value = value.replace(ThemesConstants.SPACE, ThemesConstants.UNDER);
		}
		return value;
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
		List <Theme> themes = new ArrayList<Theme>(getThemesCollection());
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
	
	public void setLastUsedTheme(int id) {
		if (id == -1) {
			return;
		}
		IWMainApplicationSettings settings = ContentUtil.getBundle().getApplication().getSettings();
		try {
			settings.setProperty(ThemesConstants.LAST_USED_THEME, String.valueOf(id));
		} catch (NumberFormatException e) {
			log.error(e);
		}
	}
	
	public String getDefaultTheme() {
		IWMainApplicationSettings settings  = ContentUtil.getBundle().getApplication().getSettings();
		if (settings == null) {
			return null;
		}
		return settings.getProperty(ThemesConstants.LAST_USED_THEME);
	}
	
	private Document preparePageDocument(Document doc, String type, String articlePath, int pageID) {
		if (ThemesConstants.ARTICLE_PAGE_TYPE.contains(type) && articlePath != null && !ThemesConstants.MINUS_ONE.equals(articlePath)) {
			Object o = null;
			Element e = null;
			Attribute a = null;
			boolean changedValue = false;
			for (Iterator it = doc.getDescendants(); (it.hasNext() && !changedValue);) {
				o = it.next();
				if (o instanceof Element) {
					e = (Element) o;
					if (ATTRIBUTE_NAME.equals(e.getName())) {
						a = e.getAttribute(ATTRIBUTE_PROPERTY);
						if (a != null) {
							a.setValue(articlePath);
							changedValue = true;
						}
					}
				}
			}
		}
		addIDsToModules(doc.getRootElement(), pageID);
		return doc;
	}
	
	private String getPageDocument(String type, String articlePath, String fileName, int pageID) {
		Document doc = pages.get(type);
		if (doc != null) {
			doc = preparePageDocument(doc, type, articlePath, pageID);
			return getThemeChanger().getXMLOutputter().outputString(doc);
		}
		doc = getXMLDocument(getWebRootWithoutContent() + ThemesConstants.PAGES_PATH_APPL + fileName);
		if (doc == null) {
			return null;
		}
		pages.put(type, doc);
		doc = preparePageDocument(doc, type, articlePath, pageID);
		return getThemeChanger().getXMLOutputter().outputString(doc);
	}
	
	public String loadPageToSlide(String type, String fileName, String articlePath, int pageID) {
		if (type == null || fileName == null) {
			return null;
		}
		
		ICPage page = getThemesService().getICPage(pageID);
		if (page == null) {
			return null;
		}
		
		String docContent = getPageDocument(type, articlePath, fileName, pageID);
		if (docContent == null) {
			return null;
		}
		
		String fullUrl = changeUploadFileName(ThemesConstants.PAGES_PATH_SLIDE + fileName);
		String base = extractValueFromString(fullUrl, 0, fullUrl.lastIndexOf(ContentConstants.SLASH));
		if (!base.endsWith(ContentConstants.SLASH)) {
			base += ContentConstants.SLASH;
		}
		String changedFileName = extractValueFromString(fullUrl, fullUrl.lastIndexOf(ContentConstants.SLASH) + 1, fullUrl.length());

		try {
			getSlideService().uploadFileAndCreateFoldersFromStringAsRoot(base, changedFileName, docContent, ContentConstants.XML_MIME_TYPE, true);
		} catch (RemoteException e) {
			log.error(e);
		}
		
		return ContentConstants.CONTENT + base + changedFileName;
	}
	
	private boolean existInSlide(String path) {
		try {
			return getSlideService().getExistence(path);
		} catch (HttpException e) {
			log.error(e);
		} catch (RemoteException e) {
			log.error(e);
		} catch (IOException e) {
			log.error(e);
		}
		return false;
	}
	
	protected String changeUploadFileName(String fileName) {
		if (fileName == null) {
			return null;
		}
		fileName = removeSpaces(fileName);
		String fileRoot = fileName;
		String fileType = ThemesConstants.EMPTY;
		if (fileName.indexOf(ThemesConstants.DOT) != -1) {
			fileRoot = extractValueFromString(fileName, 0, fileName.lastIndexOf(ThemesConstants.DOT));
			fileType = getFileExtension(fileName);
		}
		int i = 1;
		String path = fileRoot + ThemesConstants.DOT + fileType;
		while (existInSlide(path)) {
			path = fileRoot + i + ThemesConstants.DOT + fileType;
			i++;
		}
		return path;
	}
	
	public String changeFileUploadPath(String path) {
		if (path == null) {
			return null;
		}
		path = removeSpaces(path);
		int i = 1;
		String tempPath = path;
		while (existInSlide(tempPath)) {
			tempPath = path + i;
			i++;
		}
		path = tempPath;
		return path;
	}
	
	protected int getRandomNumber(int maxValue) {
		int number;
		try {
			number = numberGenerator.nextInt(maxValue);
		} catch (IllegalArgumentException e) {
			log.error(e);
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
	
	public String createArticle(String type, int id) {
		if (type == null) {
			return null;
		}
		if (id == -1) {
			return null;
		}
		if (!ThemesConstants.ARTICLE_PAGE_TYPE.contains(type)) {
			return ThemesConstants.MINUS_ONE;
		}
		
		ICPage page = getThemesService().getICPage(id);
		if (page == null) {
			return null;
		}
		String uri = page.getDefaultPageURI();
		if (uri == null) {
			return null;
		}
		
		IWContext iwc = getIWContext();
		if (iwc == null) {
			return null;
		}
		
		String language = getCurrentLanguage(iwc);
		
		String article = getArticleDocument(language, uri, iwc);
		if (article == null) {
			return null;
		}

		if (uri.endsWith(ContentConstants.SLASH)) {
			uri = extractValueFromString(uri, 0, uri.lastIndexOf(ContentConstants.SLASH));
		}

		StringBuffer file = new StringBuffer(language);
		file.append(ThemesConstants.DOT).append(ThemesConstants.XML_EXTENSION);
		StringBuffer base = new StringBuffer(ContentConstants.ARTICLE_PATH_START);
		base.append(uri);
		if (uri.equals(ContentConstants.EMPTY)) {
			if (!base.toString().endsWith(ContentConstants.SLASH)) {
				base.append(ContentConstants.SLASH);
			}
			base.append(ROOT_PAGE_ARTICLE);
		}
		base.append(getSlideService().createUniqueFileName(ContentConstants.ARTICLE_SCOPE));
		base.append(RESOURCE_PATH_END).append(ContentConstants.SLASH);
		try {
			getSlideService().uploadFileAndCreateFoldersFromStringAsRoot(base.toString(), file.toString(), article, ContentConstants.XML_MIME_TYPE, true);
			return base.toString();
		} catch (RemoteException e) {
			log.error(e);
			return null;
		}
	}
	
	private String getArticleDocument(String language, String uri, IWContext iwc) {
		String article = getArticle();
		StringBuffer summary = new StringBuffer();
		if (article.length() >= 200) {
			summary.append(article.substring(0, 200)).append(ThemesConstants.DOT).append(ThemesConstants.DOT);
			summary.append(ThemesConstants.DOT);
		}
		else {
			summary = new StringBuffer(article);
		}
		String server = getFullServerName(iwc);
		StringBuffer link = new StringBuffer(server);
		link.append(ContentConstants.PAGES_START_URI);
		link.append(uri);
		String linkToComments = getArticleCommentLink(uri);
		String user = iwc.getCurrentUser().getName();
		return getFeedBean().getFeedEntryAsXML(ThemesConstants.ARTICLE_TITLE, server, null, ThemesConstants.ARTICLE_TITLE,
				new Timestamp(System.currentTimeMillis()), null, summary.toString(), article, user, language, null, link.toString(),
				null, null, linkToComments);
	}
	
	public String getArticleCommentLink(String pageURI) {
		StringBuffer commentPath = new StringBuffer(ContentConstants.ARTICLE_PATH_START);
		if (pageURI == null) {
			commentPath.append(ContentConstants.SLASH).append(ContentUtil.getYearMonthPath());
			commentPath.append(ContentConstants.SLASH);
		} 
		else {
			if (pageURI.equals(ContentConstants.SLASH)) {
				pageURI += ROOT_PAGE_ARTICLE;
			}
			if (pageURI.endsWith(ContentConstants.SLASH)) {
				pageURI = pageURI.substring(0, pageURI.lastIndexOf(ContentConstants.SLASH));
			}
			commentPath.append(pageURI);
		}
		commentPath.append(getSlideService().createUniqueFileName(ContentConstants.COMMENT_SCOPE));
		commentPath.append(ContentConstants.COMMENT_PREFIX).append(ContentConstants.SLASH).append(ContentConstants.COMMENT_SCOPE);
		commentPath.append(ThemesConstants.DOT).append(ThemesConstants.XML_EXTENSION);
		return commentPath.toString();
	}
	
	private void addIDsToModules(Element root, int pageID) {
		if (root == null || pageID < 1) {
			return;
		}
		Iterator allElements = root.getDescendants();
		if (allElements == null) {
			return;
		}
		Element e = null;
		Object o = null;
		Attribute moduleId = null;
		
		String module = "module";
		String id = "id";
		String className = "class";
		String pageKey = String.valueOf(pageID);
		String moduleID = null;
		
		int icObjectId = -1;
		
		ICObjectInstanceHome icoiHome = null;
		ICObjectHome icoHome = null;
		ICObjectInstance instance = null;
		
		try {
			icoiHome = (ICObjectInstanceHome) IDOLookup.getHome(ICObjectInstance.class);
			icoHome = (ICObjectHome)IDOLookup.getHome(ICObject.class);
		} catch (Exception ex) {
			log.error(ex);
			return;
		}

		for (Iterator it = allElements; it.hasNext(); ) {
			o = it.next();
			if (o instanceof Element) {
				e = (Element) o;
				if (module.equals(e.getName())) {
					icObjectId = getICObjectId(e.getAttributeValue(className), icoHome);
					if (icObjectId != -1) {
						try {
							instance = icoiHome.create();
							instance.setICObjectID(icObjectId);
							instance.setIBPageByKey(pageKey);
							instance.store();
							moduleID = ICObjectBusiness.UUID_PREFIX + instance.getUniqueId();
							moduleId = e.getAttribute(id);
							if (moduleId != null) {
								moduleId.setValue(moduleID);
								System.out.println("Set uuid: " + moduleID);
							}
						} catch (CreateException ce) {
							log.error(ce);
						}
					}
				}
			}
		}
	}
	
	private int getICObjectId(String className, ICObjectHome icoHome) {
		ICObject object = null;
		try {
			object = icoHome.findByClassName(className);
		} catch (FinderException e) {
			log.error(e);
			return -1;
		}
		String key = object.getPrimaryKey().toString();
		try {
			return Integer.valueOf(key);
		} catch (NumberFormatException e) {
			log.error(e);
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
			iwc = getIWContext();
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
				log.error(e);
				return getWebRootWithoutContent();
			}
		}
		return server.toString();
	}
	
	public boolean setNewLinkInArticleFile(IWContext iwc, String link, String language, String baseDirectory, String pageUri) {
		if (iwc == null || link == null || language == null || baseDirectory == null || pageUri == null) {
			return false;
		}
		
		StringBuffer fileName = new StringBuffer(language).append(ThemesConstants.DOT).append(ThemesConstants.XML_EXTENSION);
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
		} catch (RemoteException e) {
			log.error(e);
			return false;
		}
		return true;
	}
	
	public IWContext getIWContext() {
		FacesContext context = FacesContext.getCurrentInstance();
		if (context == null) {
			return null;
		}
		IWContext iwc = IWContext.getIWContext(context);
		if (iwc == null) {
			return null;
		}
		return iwc;
	}
	
	public boolean existFileInSlide(String path) {
		try {
			return getSlideService().getExistence(path);
		} catch (HttpException e) {
			log.error(e);
			return false;
		} catch (RemoteException e) {
			log.error(e);
			return false;
		} catch (IOException e) {
			log.error(e);
			return false;
		}
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
			log.error(e);
			return key;
		}
	}
	
	public String getCurrentLanguage(IWContext iwc) {
		if (iwc == null) {
			iwc = getIWContext();
		}
		if (iwc == null) {
			return Locale.ENGLISH.toString();
		}
		Locale l = iwc.getCurrentLocale();
		if (l == null) {
			return Locale.ENGLISH.toString();
		}
		return l.toString();
	}

}