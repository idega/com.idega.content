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
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.apache.commons.httpclient.HttpURL;
import org.apache.commons.httpclient.URIException;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.jdom.Document;
import org.jdom.Element;
import org.jdom.JDOMException;
import org.jdom.input.SAXBuilder;

import com.idega.business.IBOLookup;
import com.idega.business.IBOLookupException;
import com.idega.content.business.ContentSearch;
import com.idega.content.themes.business.ThemesEngine;
import com.idega.content.themes.business.ThemesService;
import com.idega.core.search.business.SearchResult;
import com.idega.graphics.Generator;
import com.idega.graphics.ImageGenerator;
import com.idega.idegaweb.IWMainApplication;
import com.idega.presentation.IWContext;
import com.idega.repository.data.Singleton;
import com.idega.slide.business.IWSlideService;

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
	
	private Map <String, Theme> themes = null;
	private Map <String, Setting> themeSettings = null;
	private Map <String, Setting> pageSettings = null;
	private List <String> themeQueue = null;
	private List <String> urisToThemes = null;
	
	private boolean checkedFromSlide = false;
	private boolean loadedThemeSettings = false;
	private boolean loadedPageSettings = false;
	
	private String fullWebRoot; // For cache
	private String webRoot;
	
	private ThemesHelper(boolean searchForThemes) {
		themes = new HashMap <String, Theme> ();
		themeSettings = new HashMap <String, Setting> ();
		pageSettings = new HashMap <String, Setting> ();
		themeQueue = new ArrayList <String> ();
		urisToThemes = new ArrayList <String> ();
		if (searchForThemes) {
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
		if (service == null) {
			synchronized (ThemesHelper.class) {
				try {
					service = (IWSlideService) IBOLookup.getServiceInstance(IWContext.getInstance(), IWSlideService.class);
				} catch (IBOLookupException e) {
					log.error(e);
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
	
	public void searchForThemes() {
		if (!checkedFromSlide) {
			checkedFromSlide = true;
			ContentSearch search = new ContentSearch(IWMainApplication.getDefaultIWMainApplication());
			Collection results = search.doSimpleDASLSearch(ThemesConstants.THEME_SEARCH_KEY, ThemesConstants.CONTENT + ThemesConstants.THEMES_PATH);
			if (results == null) {
				return;
			}
			Iterator it = results.iterator();
			List <String> urisToThemes = new ArrayList<String>();
			String uri = null;
			while (it.hasNext()) {
				uri = ((SearchResult) it.next()).getSearchResultURI();
				if (isCorrectFile(uri)) {
					urisToThemes.add(uri);
				}
			}
			checkedFromSlide = getThemesLoader().loadThemes(urisToThemes, false, true);
		}
	}
	
	protected String getFileName(String uri) {
		String name = null;
		int begin = uri.lastIndexOf(ThemesConstants.SLASH);
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
		int begin = uri.lastIndexOf(ThemesConstants.SLASH);
		if (begin == -1) {
			return uri;
		}
		else {
			name = extractValueFromString(uri, begin + 1, uri.length());
		}
		return name;
	}
	
	protected String extractValueFromString(String fullString, int beginIndex, int endIndex) {
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
	
	protected void addTheme(Theme themeInfo) {
		themes.put(themeInfo.getId(), themeInfo);
	}
	
	public Collection <Theme> getThemesCollection() {
		return themes.values();
	}
	
	protected void addUriToTheme(String uri) {
		urisToThemes.add(uri);
	}
	
	public boolean existTheme(String uri) {
		if (urisToThemes.contains(uri)) {
			return true;
		}
		return false;
	}
	
	public Document getXMLDocument(String url) {
		if (url == null) {
			return null;
		}
		
		return getXMLDocument(getInputStream(url));
	}
	
	protected Document getXMLDocument(InputStream stream) {
		if(stream == null){
			return null;
		}
		
		Reader r = null;
		try {
			r = new InputStreamReader(stream, ThemesConstants.ENCODING);
		} catch (UnsupportedEncodingException e) {
			log.error(e);
			return null;
		}
		
		SAXBuilder builder = new SAXBuilder();
		Document document = null;
		try {
			document = builder.build(r);
		} catch (JDOMException e) {
			log.error(e);
			return null;
		} catch (IOException e) {
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
		int index = uri.lastIndexOf(ThemesConstants.SLASH);
		String link = extractValueFromString(uri, 0, index);
		if (!link.endsWith(ThemesConstants.SLASH)) {
			link += ThemesConstants.SLASH;
		}
		return link;
	}
	
	protected Theme getTheme(String themeID) {
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
            log.error(e);
        } catch (java.io.IOException e) {
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
		String[] fileParts = url.split(ThemesConstants.SLASH);
		StringBuffer encoded = new StringBuffer();
		for (int i = 0; i < fileParts.length; i++) {
			if (fileParts[i].equals(ThemesConstants.EMPTY)) {
				encoded.append(ThemesConstants.SLASH);
			}
			else {
				try {
					encoded.append(URLEncoder.encode(fileParts[i], ThemesConstants.ENCODING));
				} catch (UnsupportedEncodingException e) {
					log.error(e);
					return url;
				}
				if (i + 1 < fileParts.length) {
					encoded.append(ThemesConstants.SLASH);
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
		String[] fileParts = url.split(ThemesConstants.SLASH);
		StringBuffer encoded = new StringBuffer();
		encoded.append(ThemesConstants.SLASH);
		for (int i = 0; i < fileParts.length; i++) {
			if (!fileParts[i].equals(ThemesConstants.EMPTY)) {
				try {
					encoded.append(URLDecoder.decode(fileParts[i], ThemesConstants.ENCODING));
				} catch (UnsupportedEncodingException e) {
					log.error(e);
					return url;
				}
				if (i + 1 < fileParts.length) {
					encoded.append(ThemesConstants.SLASH);
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
					themesService = (ThemesService) IBOLookup.getServiceInstance(IWContext.getInstance(), ThemesService.class);
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
		return getThemeChanger().uploadDocument(doc, theme.getLinkToBaseAsItIs(), theme.getName() + ThemesConstants.IDEGA_THEME_INFO, theme, false);
	}
	
	public String[] getPageValues(Setting s, String value) {
		if (ThemesConstants.EMPTY.equals(s.getDefaultValue()) && value == null) {
			return new String[] {ThemesConstants.EMPTY};
		}
		String[] settingValues = null;
		if (s.getDefaultValue() != null) {
			if (!ThemesConstants.EMPTY.equals(s.getDefaultValue())) {
				settingValues = s.getDefaultValue().split(ThemesConstants.SEPARATOR);
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
					themesEngine = (ThemesEngine) IBOLookup.getServiceInstance(IWContext.getInstance(), ThemesEngine.class);
				} catch (IBOLookupException e) {
					log.error(e);
				}
			}
		}
		return themesEngine;
	}

}