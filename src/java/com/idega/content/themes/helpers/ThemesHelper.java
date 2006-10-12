package com.idega.content.themes.helpers;

import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
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
	
	private Map <String, ThemeInfo> themes = null;
	private List <String> urisToThemes = null;
	
	private IWSlideService service;
	private boolean checkedFromSlide;
	
	private ThemesHelper() {
		themes = new HashMap <String, ThemeInfo> ();
		urisToThemes = new ArrayList <String> ();
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
					loader = new ThemesLoader();
				}
			}
		}
		return loader;
	}
	
	public void searchForThemes() {
		if (!checkedFromSlide) {
			ContentSearch search = new ContentSearch(IWMainApplication.getDefaultIWMainApplication());
			Collection results = search.doSimpleDASLSearch(ThemesConstants.THEME_SEARCH_KEY, ThemesConstants.CONTENT + ThemesConstants.THEMES_PATH);
			if (results == null) {
				return;
			}
			Iterator it = results.iterator();
			List <String> urisToThemes = new ArrayList<String>();
			while (it.hasNext()) {
				urisToThemes.add(((SearchResult) it.next()).getSearchResultURI());
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
	
	public String getFileType(String uri) {
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
	
	public String getWebRootWithoutContent(String webRoot) {
		String webDAVServerURI = ThemesConstants.EMPTY;
		try {
			webDAVServerURI = getSlideService().getWebdavServerURI();
		} catch (RemoteException e) {
			log.error(e);
		}
		int contentIndex = webRoot.indexOf(webDAVServerURI);
		webRoot = extractValueFromString(webRoot, 0, contentIndex);
		return webRoot;
	}
	
	public String getFullWebRoot() {
		String webRoot = null;
		HttpURL root = null;
		try {
			root = getSlideService().getWebdavServerURL();
		} catch (RemoteException e) {
			log.error(e);
			return null;
		}
		try {
			webRoot = root.getURI();
		} catch (URIException e) {
			log.error(e);
			return null;
		}
		return webRoot;
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
		int index = fileName.lastIndexOf(ThemesConstants.DOT);
		if (index == -1) {
			return result;
		}
		String fileExtension = fileName.substring(index + 1).toLowerCase();
		for (int i = 0; (i < ThemesConstants.FILTER.length && !result); i++) {
			if (isCorrectFile(fileExtension, ThemesConstants.FILTER[i])) {
				result = true;
			}
		}
		return result;
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
		boolean result = false;
		String fileName = getFileNameWithExtension(uri);
		for (int i = 0; (i < ThemesConstants.PROPERTIES_FILES.length && !result); i++) {
			if (fileName.equals(ThemesConstants.PROPERTIES_FILES[i])) {
				result = true;
			}
		}
		return result;
	}
	
	public void addTheme(ThemeInfo themeInfo) {
		themes.put(themeInfo.getThemeId(), themeInfo);
	}
	
	public Collection <ThemeInfo> getThemesCollection() {
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
	
	public ThemeInfo getThemeInfo(String themeInfoID) {
		return themes.get(themeInfoID);
	}
	
	public void removeTheme(String uri, String themeID) {
		if (uri == null || themeID == null) {
			return;
		}
		urisToThemes.remove(uri);
		themes.remove(themeID);
	}

	protected Map<String, ThemeInfo> getThemes() {
		return themes;
	}

	protected List<String> getUrisToThemes() {
		return urisToThemes;
	}

}
