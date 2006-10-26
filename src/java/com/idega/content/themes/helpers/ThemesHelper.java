package com.idega.content.themes.helpers;

import java.io.IOException;
import java.io.InputStream;
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
import com.idega.core.search.business.SearchResult;
import com.idega.graphics.PreviewGenerator;
import com.idega.graphics.WebPagePreviewGenerator;
import com.idega.idegaweb.IWMainApplication;
import com.idega.presentation.IWContext;
import com.idega.repository.data.Singleton;
import com.idega.slide.business.IWSlideService;

public class ThemesHelper implements Singleton {
	
	private static Log log = LogFactory.getLog(ThemesHelper.class);
	
	private volatile static ThemesHelper helper = null;
	
	private volatile PreviewGenerator generator = null;
	private volatile ThemeChanger changer = null;
	private volatile ThemeStyleVariations variations = null;
	private volatile ThemesPropertiesExtractor extractor = null;
	private volatile ThemesLoader loader = null;
	
	private Map <String, Theme> themes = null;
	private Map <String, ThemeSettings> settings = null;
	private List <String> urisToThemes = null;
	
	private IWSlideService service;
	private boolean checkedFromSlide;
	private boolean loadedThemeSettings;
	
	private String fullWebRoot; // For cache
	private String webRoot;
	
	private ThemesHelper() {
		themes = new HashMap <String, Theme> ();
		settings = new HashMap <String, ThemeSettings> ();
		urisToThemes = new ArrayList <String> ();
		loadThemeSettings();
		searchForThemes();
	}
	
	public static ThemesHelper getInstance() {
		if (helper == null) {
			synchronized (ThemesHelper.class) {
				if (helper == null) {
					helper = new ThemesHelper();
				}
			}
		}
		return helper;
	}
	
	public PreviewGenerator getPreviewGenerator() {
		if (generator == null) {
			synchronized (ThemesHelper.class) {
				if (generator == null) {
					generator = new WebPagePreviewGenerator();
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
	
	/**
	 * Returns instance of IWSlideService
	 */
	public IWSlideService getSlideService() {
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
	
	private void searchForThemes() {
		if (!checkedFromSlide) {
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
			checkedFromSlide = getThemesLoader().loadThemes(urisToThemes, false);
		}
	}
	
	public String getFileName(String uri) {
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
	
	public String getFileNameWithExtension(String uri) {
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
	
	public String getFileExtension(String uri) {
		String type = null;
		int begin = uri.lastIndexOf(ThemesConstants.DOT);
		if (begin != -1) {
			type = uri.substring(begin + 1).toLowerCase();
		}
		return type;
	}
	
	public List getFiles(String folderURI) {
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
	
	public String getWebRootWithoutContent(String fullWebRoot) {
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
	
	public boolean isCorrectFile(String fileName, String nameTemplate) {
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
	
	public boolean isDraft(String fileName) {
		if (fileName == null) {
			return true;
		}
		if (fileName.endsWith(ThemesConstants.DRAFT)) {
			return true;
		}
		return false;
	}
	
	public boolean isSystemFile(String fileName) {
		if (fileName == null) {
			return true; // Not a system file, but invalid also
		}
		if (getFileNameWithExtension(fileName).startsWith(ThemesConstants.DOT)) {
			return true;
		}
		return false;
	}
	
	public boolean isPropertiesFile(String uri) {
		if (ThemesConstants.PROPERTIES_FILES.contains(uri)) {
			return true;
		}
		return false;
	}
	
	public void addTheme(Theme themeInfo) {
		themes.put(themeInfo.getThemeId(), themeInfo);
	}
	
	public Collection <Theme> getThemesCollection() {
		return themes.values();
	}
	
	public void addUriToTheme(String uri) {
		urisToThemes.add(uri);
	}
	
	public boolean existTheme(String uri) {
		if (urisToThemes.contains(uri)) {
			return true;
		}
		return false;
	}
	
	public Document getXMLDocument(String link) {
		URL url = null;
		try {
			url = new URL(link);
		} catch (MalformedURLException e) {
			log.error(e);
			return null;
		}
		SAXBuilder builder = new SAXBuilder();
		Document document = null;
		try {
			document = builder.build(url);
		} catch (JDOMException e) {
			log.error(e);
			return null;
		} catch (IOException e) {
			log.error(e);
			return null;
		}
		return document;
	}
	
	public String getLinkToBase(String uri) {
		int index = uri.lastIndexOf(ThemesConstants.SLASH);
		String link = extractValueFromString(uri, 0, index);
		if (!link.endsWith(ThemesConstants.SLASH)) {
			link += ThemesConstants.SLASH;
		}
		return link;
	}
	
	public Theme getTheme(String themeID) {
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
	
	protected Map <String, ThemeSettings> getSettings() {
		return settings;
	}
	
	private void loadThemeSettings() {
		if (loadedThemeSettings) {
			return;
		}
		String url = getWebRootWithoutContent() + ThemesConstants.THEME_SETTINGS;
		Document doc = getXMLDocument(url);
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
		ThemeSettings setting = null;
		for (int i = 0; i < keys.size(); i++) {
			key = (Element) keys.get(i);
			setting = new ThemeSettings();
			
			setting.setCode(key.getChildTextNormalize(ThemesConstants.THEME_SETTING_CODE));
			setting.setLabel(key.getChildTextNormalize(ThemesConstants.THEME_SETTING_LABEL));
			setting.setDefaultValue(key.getChildTextNormalize(ThemesConstants.THEME_SETTING_DEFAULT_VALUE));
			setting.setType(key.getChildTextNormalize(ThemesConstants.THEME_SETTING_TYPE));
			setting.setMethod(key.getChildTextNormalize(ThemesConstants.THEME_SETTING_METHOD));
			
			settings.put(setting.getCode(), setting);
		}
		loadedThemeSettings = true;
	}
	
	public InputStream getInputStream(String webRoot, String link) {
		InputStream is = null;
        try {
        	URL url = getUrl(webRoot + link);
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
	
	public boolean closeInputStream(InputStream is) {
		try {
			is.close();
		} catch (IOException e) {
			log.error(e);
			return false;
		}
		return true;
	}
	
	public URL getUrl(String link) {
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
		encoded.append(ThemesConstants.SLASH);
		for (int i = 0; i < fileParts.length; i++) {
			if (!fileParts[i].equals(ThemesConstants.EMPTY)) {
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
	
	public String decode(String value, boolean fullyDecode) {
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

}
