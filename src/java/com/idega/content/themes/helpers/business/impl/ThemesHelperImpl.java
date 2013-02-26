package com.idega.content.themes.helpers.business.impl;

import java.awt.Dimension;
import java.awt.image.BufferedImage;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.UnsupportedEncodingException;
import java.net.URL;
import java.net.URLDecoder;
import java.net.URLEncoder;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Random;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.ejb.FinderException;
import javax.jcr.RepositoryException;

import org.jdom2.Attribute;
import org.jdom2.Document;
import org.jdom2.Element;
import org.jdom2.Namespace;
import org.springframework.beans.factory.annotation.Autowired;

import com.idega.block.rss.business.EntryData;
import com.idega.builder.bean.AdvancedProperty;
import com.idega.content.bean.ContentItemFeedBean;
import com.idega.content.business.ContentConstants;
import com.idega.content.business.ContentUtil;
import com.idega.content.themes.bean.ThemesManagerBean;
import com.idega.content.themes.business.ThemesService;
import com.idega.content.themes.helpers.bean.Setting;
import com.idega.content.themes.helpers.bean.Theme;
import com.idega.content.themes.helpers.bean.ThemeStyleGroupMember;
import com.idega.content.themes.helpers.business.ThemesComparator;
import com.idega.content.themes.helpers.business.ThemesConstants;
import com.idega.content.themes.helpers.business.ThemesHelper;
import com.idega.content.themes.helpers.business.ThemesLoader;
import com.idega.core.builder.data.ICDomain;
import com.idega.core.builder.data.ICPage;
import com.idega.core.business.DefaultSpringBean;
import com.idega.core.component.business.ICObjectBusiness;
import com.idega.core.component.data.ICObject;
import com.idega.core.component.data.ICObjectHome;
import com.idega.core.component.data.ICObjectInstance;
import com.idega.core.component.data.ICObjectInstanceHome;
import com.idega.core.file.util.MimeTypeUtil;
import com.idega.data.IDOLookup;
import com.idega.graphics.image.business.ImageGenerator;
import com.idega.graphics.image.business.ImageGeneratorImpl;
import com.idega.idegaweb.IWMainApplication;
import com.idega.idegaweb.IWMainApplicationSettings;
import com.idega.presentation.IWContext;
import com.idega.repository.RepositoryService;
import com.idega.servlet.filter.IWBundleResourceFilter;
import com.idega.util.CoreConstants;
import com.idega.util.CoreUtil;
import com.idega.util.IOUtil;
import com.idega.util.ListUtil;
import com.idega.util.StringHandler;
import com.idega.util.StringUtil;
import com.idega.util.xml.XmlUtil;
import com.idega.webface.WFUtil;

public class ThemesHelperImpl extends DefaultSpringBean implements ThemesHelper {

	private static Logger LOGGER = Logger.getLogger(ThemesHelperImpl.class.getName());

	@Autowired
	private RepositoryService repositoryService;

	private volatile ImageGenerator generator = null;
	private volatile ThemesService themesService = null;
	private volatile ContentItemFeedBean feedBean = null;

	private Map<String, Theme> themes = null;
	private List<Setting> pageSettings = null;
	private List<Setting> themeSettings = null;
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

	private ThemesHelperImpl() {
		themes = new HashMap<String, Theme>();

		pageSettings = Collections.synchronizedList(new ArrayList<Setting>());
		themeSettings = Collections.synchronizedList(new ArrayList<Setting>());

		themeQueue = new ArrayList <String> ();
		urisToThemes = new ArrayList <String> ();
		loadedThemes = new ArrayList<String>();
		moduleIds = new ArrayList<String>();
		predefinedThemeStyles = new ArrayList<String>();

		numberGenerator = new Random();
	}

	@Override
	public ImageGenerator getImageGenerator(IWContext iwc) {
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

	@Override
	public RepositoryService getRepositoryService() {
		return repositoryService;
	}

	private ContentItemFeedBean getFeedBean() {
		if (feedBean == null) {
			feedBean = new ContentItemFeedBean((IWContext)null, ContentItemFeedBean.FEED_TYPE_ATOM_1);
		}
		return feedBean;
	}

	@Override
	public synchronized void searchForThemes(Collection<ICPage> templates) {
		List<String> themesSkeletons = loadSearchResults(templates, ThemesConstants.THEME_SKELETONS_FILTER);
		try {
			ThemesLoader themesLoader = new ThemesLoader();
			themesLoader.loadThemes(themesSkeletons, false, true);
		} catch (Exception e) {
			LOGGER.log(Level.WARNING, "Error loading themes: " + themesSkeletons, e);
		}
	}

	@Override
	public List<String> loadSearchResults(Collection<ICPage> searchResults, List<String> filter) {
		List<String> loadedResults = new ArrayList<String>();
		if (searchResults == null)
			return loadedResults;

		for (ICPage template: searchResults) {
			String uri = template.getWebDavUri();
			if (isCorrectThemeTemplateFile(uri, filter) && !loadedResults.contains(uri)) {
				loadedResults.add(uri);
			}
		}
		return loadedResults;
	}

	@Override
	public String getFileName(String uri) {
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

	@Override
	public String getFileNameWithExtension(String uri) {
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

	@Override
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

	@Override
	public String getFileExtension(String uri) {
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

	@Override
	public String getWebRootWithoutContent() {
		return getWebRootWithoutContent(getFullWebRoot());
	}

	@Override
	public String getWebRootWithoutContent(String fullWebRoot) {
		if (webRoot != null) {
			return webRoot;
		}
		String webDAVServerURI = getRepositoryService().getWebdavServerURL();
		int contentIndex = fullWebRoot.indexOf(webDAVServerURI);
		webRoot = extractValueFromString(fullWebRoot, 0, contentIndex);
		return webRoot;
	}

	@Override
	public String getFullWebRoot() {
		if (fullWebRoot != null) {
			return fullWebRoot;
		}

		String serverURL = getRepositoryService().getWebdavServerURL();
		fullWebRoot = serverURL;

		return fullWebRoot;
	}

	@Override
	public boolean isCorrectFile(String fileName, String nameTemplate) {
		if (fileName == null || nameTemplate == null) {
			return false;
		}
		return fileName.equals(nameTemplate);
	}

	@Override
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

	@Override
	public boolean isCreatedManually(String fileName) {
		if (fileName == null) {
			return true;
		}
		if (fileName.endsWith(ThemesConstants.THEME)) {
			return true;
		}
		return false;
	}

	@Override
	public boolean isDraft(String fileName) {
		if (fileName == null) {
			return true;
		}
		if (fileName.endsWith(ThemesConstants.DRAFT)) {
			return true;
		}
		return false;
	}

	@Override
	public boolean isSystemFile(String fileName) {
		if (fileName == null) {
			return true; // Not a system file, but invalid also
		}
		if (getFileNameWithExtension(fileName).startsWith(CoreConstants.DOT)) {
			return true;
		}
		return false;
	}

	@Override
	public boolean isPropertiesFile(String uri) {
		if (ThemesConstants.THEME_PROPERTIES_FILES.contains(uri)) {
			return true;
		}
		return false;
	}

	@Override
	public void addTheme(Theme theme) {
		if (theme == null) {
			return;
		}

		Collection<String> registeredThemesIds = new ArrayList<String>(themes.keySet());
		boolean validTheme = true;
		for (Iterator<String> idsIter = registeredThemesIds.iterator(); (validTheme && idsIter.hasNext());) {
			Theme themeInMap = themes.get(idsIter.next());
			if (themeInMap != null && themeInMap.getLinkToSkeleton().equals(theme.getLinkToSkeleton())) {
				validTheme = false;
			}
		}

		if (validTheme) {
			themes.put(theme.getId(), theme);
		}
	}

	@Override
	public List<Theme> getAvailableThemes() {
		Iterator<Theme> it = themes.values().iterator();
		if (it == null)
			return null;

		Theme theme = null;
		List<Theme> availableThemes = new ArrayList<Theme>();
		for (Iterator<Theme> i = it; i.hasNext();) {
			theme = i.next();

			if (!theme.isLoading() && !theme.isLocked() && theme.isPropertiesExtracted())
				availableThemes.add(theme);
			else {
				if (theme.isLoading())
					getLogger().info("Theme " + theme + " is being loaded");
				else if (theme.isLocked())
					getLogger().info("Theme " + theme + " is locked");
				else if (!theme.isPropertiesExtracted())
					getLogger().info("Properties for theme " + theme + " are not extracted");
			}
		}

		return availableThemes;
	}

	@Override
	public Collection<Theme> getAllThemes() {
		return themes.values();
	}

	@Override
	public List<Theme> getSortedThemes() {
		List<Theme> availableThemes = getAvailableThemes();

		if (ListUtil.isEmpty(availableThemes))
			return null;

		Locale locale = getCurrentLocale();
		Collections.sort(availableThemes, new ThemesComparator(locale == null ? Locale.ENGLISH : locale));

		return availableThemes;
	}

	@Override
	public synchronized void addUriToTheme(String uri) {
		urisToThemes.add(uri);
	}

	@Override
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

	@Override
	public Document getXMLDocument(String url) {
		return getXMLDocument(url, false, false);
	}

	private Document getXMLDocument(String url, boolean cleanWithHtmlCleaner, boolean useLog, boolean omitComments, boolean omitDocTypeDeclaration) {
		if (url == null) {
			return null;
		}

		InputStream stream = getInputStream(url, useLog);

		if (stream != null && cleanWithHtmlCleaner) {
			String content = getThemesService().getBuilderService().getCleanedHtmlContent(stream, omitDocTypeDeclaration, false, omitComments);
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
			IOUtil.closeInputStream(stream);
		}

		return null;
	}

	@Override
	public Document getXMLDocument(String url, boolean cleanWithHtmlCleaner, boolean omitComments) {
		return getXMLDocument(url, cleanWithHtmlCleaner, false, omitComments);
	}

	@Override
	public Document getXMLDocument(String url, boolean cleanWithHtmlCleaner, boolean omitComments, boolean omitDocTypeDeclaaration) {
		return getXMLDocument(url, cleanWithHtmlCleaner, false, omitComments, omitDocTypeDeclaaration);
	}

	@Override
	public Document getXMLDocument(InputStream stream) throws Exception {
		return XmlUtil.getJDOMXMLDocument(stream, false);
	}

	@Override
	public String getLinkToBase(String uri) {
		int index = uri.lastIndexOf(ContentConstants.SLASH);
		String link = extractValueFromString(uri, 0, index);
		if (!link.endsWith(ContentConstants.SLASH)) {
			link += ContentConstants.SLASH;
		}
		return link;
	}

	@Override
	public Theme getTheme(String themeKey) {
		if (themeKey == null) {
			return null;
		}
		return themes.get(themeKey);
	}

	@Override
	public void removeTheme(String uri, String themeKey) {
		if (uri == null || themeKey == null) {
			return;
		}
		urisToThemes.remove(uri);
		themes.remove(themeKey);
	}

	@Override
	public Map<String, Theme> getThemes() {
		return themes;
	}

	@Override
	public List<Setting> getThemeSettings() {
		return themeSettings;
	}

	@Override
	public List<Setting> getPageSettings() {
		return pageSettings;
	}

	@Override
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

	@Override
	public void loadPageSettings(String url) {
		if (loadedPageSettings) {
			return;
		}
		loadSettings(pageSettings, getXMLDocument(url));
		loadedPageSettings = true;
	}

	private void loadSettings(List<Setting> settings, Document doc) {
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

			settings.add(setting);
		}
	}

	private InputStream getInputStream(String uri, boolean printError) {
		if (uri == null) {
			return null;
		}

		if (uri.startsWith(IWBundleResourceFilter.BUNDLES_STANDARD_DIR)) {
			File file = IWBundleResourceFilter.copyResourceFromJarToWebapp(IWMainApplication.getDefaultIWMainApplication(), uri);
			if (file == null) {
				if (printError) {
					LOGGER.log(Level.WARNING, "Error getting file (file was not found): " + uri);
				}
				return null;
			}

			try {
				return new FileInputStream(file);
			} catch (FileNotFoundException e) {
				if (printError) {
	        		e.printStackTrace();
	        		LOGGER.log(Level.WARNING, "Error getting file: " + uri);
	        	}
			}
		}
		else {
			if (uri.indexOf(CoreConstants.WEBDAV_SERVLET_URI) == -1) {
				try {
					URL url = new URL(uri);
					return url.openStream();
				} catch(Exception e) {
					if (printError) {
						LOGGER.log(Level.WARNING, "Error getting InputStream from: " + uri, e);
					}
				}
			} else {
				String webServer = getFullWebRoot();
				if (uri.startsWith(webServer)) {
					uri = uri.replace(webServer, CoreConstants.EMPTY);
				}
				try {
					return getRepositoryService().getInputStreamAsRoot(uri);
				} catch (Exception e) {
					if (printError) {
						LOGGER.log(Level.WARNING, "Error getting InputStream from: " + uri, e);
					}
				}
			}
		}

		return null;
	}

	@Override
	public InputStream getInputStream(String link) {
		return getInputStream(link, false);
	}

	@Override
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

	@Override
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

	@Override
	public String decode(String value, boolean fullyDecode) {
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

	@Override
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

	@Override
	public boolean createSmallImage(Theme theme) {
		if (theme == null) {
			return false;
		}

		return generatePreviewsForTheme(theme, false, false, 1);
	}

	@Override
	public boolean createSmallImage(Theme theme, boolean useDraftPreview) {
		return createSmallImage(theme);
	}

	@Override
	public ThemesService getThemesService() {
		if (themesService == null) {
			try {
				themesService = getServiceInstance(ThemesService.class);
			} catch (Exception e) {
				e.printStackTrace();
			}
		}
		return themesService;
	}

	@Override
	public boolean createThemeConfig(Theme theme) {
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

		List<ThemeStyleGroupMember> enabled = null;
		try {
			enabled = theme.getEnabledStyles();
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
			return getRepositoryService().uploadFileAndCreateFoldersFromStringAsRoot(theme.getLinkToBaseAsItIs(), fileName + ThemesConstants.IDEGA_THEME_INFO,
					XmlUtil.getPrettyJDOMDocument(doc), MimeTypeUtil.MIME_TYPE_XML);
			//changer.uploadDocument(doc, theme.getLinkToBaseAsItIs(), fileName + ThemesConstants.IDEGA_THEME_INFO, theme, false);
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

	@Override
	public String getPreparedThemeNameToUseInRepository(Theme theme) {
		if (theme == null) {
			return null;
		}

		return getPreparedThemeNameToUseInRepository(theme.getName());
	}

	@Override
	public String getPreparedThemeNameToUseInRepository(String themeName) {
		if (themeName == null) {
			return null;
		}

		String fileName = StringHandler.removeCharacters(themeName, ContentConstants.SPACE, ContentConstants.UNDER);
		return StringHandler.replace(fileName, "'", CoreConstants.EMPTY);
	}

	@Override
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

	public boolean closeInputStreamReader(InputStreamReader stream) {
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

	public boolean closeBufferedReader(BufferedReader buffer) {
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

	@Override
	public synchronized void addThemeToQueue(String linkToBase) {
		if (!themeQueue.contains(linkToBase)) {
			themeQueue.add(linkToBase);
		}
	}

	@Override
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

	@Override
	public String getLastVisitedPage() {
		Object lastVisitedPage = WFUtil.invoke(ThemesManagerBean.THEMES_MANAGER_BEAN_ID, "getLastVisitedPageId");
		if (lastVisitedPage != null) {
			return lastVisitedPage.toString();
		}
		return null;
	}

	@Override
	public void setLastVisitedPage(String lastVisitedPage) {
		WFUtil.invoke(ThemesManagerBean.THEMES_MANAGER_BEAN_ID, "setLastVisitedPageId", lastVisitedPage, String.class);
	}

	@Override
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

	@Override
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

	@Override
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

	@Override
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

	private List<Element> getArticleViewerElements(Document doc) {
		if (doc == null) {
			return null;
		}

		List<Element> articleViewers = new ArrayList<Element>();
		Object o = null;
		Element e = null;
		Attribute classAttribute = null;
		for (@SuppressWarnings("rawtypes")
		Iterator it = doc.getDescendants(); it.hasNext();) {
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
			LOGGER.log(Level.WARNING, "Template file ("+templateFile+") wasn'tfound!");
			return null;
		}
		doc = preparePageDocument(doc, articlesPaths, pageId);
		try {
			return XmlUtil.getPrettyJDOMDocument(doc);
		} catch (Exception e) {
			e.printStackTrace();
			return null;
		}
	}

	@Override
	public String loadPageToRepository(String type, String templateFile, List<String> articlesPaths, int pageID) {
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
			if (!getRepositoryService().uploadFileAndCreateFoldersFromStringAsRoot(base, changedFileName, docContent, ContentConstants.XML_MIME_TYPE))
				return null;
		} catch (Exception e) {
			LOGGER.log(Level.WARNING, "Error uploading to " + base + changedFileName, e);
			return null;
		}

		return CoreConstants.WEBDAV_SERVLET_URI + base + changedFileName;
	}

	private boolean existInSlide(String path) {
		try {
			return getRepositoryService().getExistence(path);
		} catch (RepositoryException e) {
			e.printStackTrace();
		}
		return false;
	}

	@Override
	public String getFixedRepositoryFileName(String fileName) {
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

		String fileRoot = getFixedRepositoryFileName(fileName);
		String fileType = ThemesConstants.EMPTY;
		if (fileName.indexOf(CoreConstants.DOT) != -1) {
			fileRoot = extractValueFromString(fileName, 0, fileName.lastIndexOf(CoreConstants.DOT));
			fileType = getFileExtension(fileName);
		}

		StringBuffer path = new StringBuffer(fileRoot).append(ContentConstants.UNDER);
		path.append(getUniqueIdByNumberAndDate(path.toString(), IDEGA_PAGES_SCOPE)).append(CoreConstants.DOT).append(fileType);

		return path.toString();
	}

	@Override
	public String changeFileUploadPath(String path) {
		if (path == null) {
			return null;
		}

		path = getFixedRepositoryFileName(path);
		StringBuffer tempPath = new StringBuffer(path);

		int i = 1;
		while (existInSlide(tempPath.toString())) {
			tempPath = new StringBuffer(path).append(i);
			i++;
		}
		path = tempPath.toString();
		return path;
	}

	@Override
	public int getRandomNumber(int maxValue) {
		int number;
		try {
			number = numberGenerator.nextInt(maxValue);
		} catch (IllegalArgumentException e) {
			e.printStackTrace();
			return 0;
		}
		return number;
	}

	@Override
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

	@Override
	public List<String> createArticle(String templateFile, int id) {
		if (templateFile == null) {
			return null;
		}
		if (id == -1) {
			return null;
		}

		Document doc = getXMLDocument(templateFile);
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
			base.append(CoreConstants.UNDER).append(getRepositoryService().createUniqueFileName(base.toString(), ContentConstants.ARTICLE_SCOPE));
			base.append(RESOURCE_PATH_END);

			article = getArticleDocument(language, base.toString(), iwc);

			base.append(CoreConstants.SLASH);
			try {
				if (getRepositoryService().uploadFileAndCreateFoldersFromStringAsRoot(base.toString(), file.toString(), article, ContentConstants.XML_MIME_TYPE))
					paths.add(base.toString());
			} catch (Exception e) {
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

		EntryData entryData = new EntryData();
		entryData.setTitle(ThemesConstants.ARTICLE_TITLE);
		entryData.setUpdated(new Timestamp(System.currentTimeMillis()));
		entryData.setDescription(summary.toString());
		entryData.setBody(article);
		entryData.setAuthor(user);
		entryData.setLanguage(language);
		entryData.setLink(link.toString());
		entryData.setLinkToComments(linkToComments);
		entryData.setCreator(ThemesConstants.MINUS_ONE);

		return getFeedBean().getFeedEntryAsXML(ThemesConstants.ARTICLE_TITLE, server, null, entryData);

	}

	@Override
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
		commentPath.append(getRepositoryService().createUniqueFileName(commentPath.toString(), ContentConstants.COMMENT_SCOPE));

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

	private boolean addIDsToModules(Element root, int pageID) {
		if (root == null || pageID < 0) {
			return false;
		}
		@SuppressWarnings("rawtypes")
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
			for (@SuppressWarnings("rawtypes")
			Iterator it = allElements; it.hasNext(); ) {
				o = it.next();
				moduleID = null;
				icObjectId = -1;
				if (o instanceof Element) {
					e = (Element) o;
					if (MODULE_ELEMENT_NAME.equals(e.getName())) {
						icObjectId = getICObjectId(e.getAttributeValue(ELEMENT_CLASS_ATTRIBUTE), icoHome);
						if (icObjectId == -1) {
							LOGGER.log(Level.WARNING, "Didn't get ICObject for: "+e.getAttributeValue(ELEMENT_CLASS_ATTRIBUTE));
							LOGGER.log(Level.WARNING, "Generating unique module id");
							moduleID = getUniqueIdByNumberAndDate(null, MODULE_ID_SCOPE);
							while (moduleIds.contains(moduleID)) {
								moduleID = getUniqueIdByNumberAndDate(null, MODULE_ID_SCOPE);
							}
							moduleIds.add(moduleID);
							moduleID = new StringBuffer(ICObjectBusiness.UUID_PREFIX).append(moduleID).toString();
						} else {
							instance = icoiHome.create();
							instance.setICObjectID(icObjectId);
							instance.setIBPageByKey(pageKey);
							instance.store();
							moduleID = new StringBuffer(ICObjectBusiness.UUID_PREFIX).append(instance.getUniqueId()).toString();
						}
						moduleIdAttribute = e.getAttribute(id);
						if (moduleIdAttribute != null) {
							moduleIdAttribute.setValue(moduleID);
						} else {
							LOGGER.log(Level.WARNING, "Didn't find module id attribute for: " + e.getAttributeValue(ELEMENT_CLASS_ATTRIBUTE));
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

	@Override
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

	@Override
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
			return getRepositoryService().uploadFileAndCreateFoldersFromStringAsRoot(baseDirectory, fileName.toString(), XmlUtil.getPrettyJDOMDocument(d),
					ContentConstants.XML_MIME_TYPE);
		} catch (Exception e) {
			e.printStackTrace();
		}
		return false;
	}

	private boolean existFileInSlide(String path, boolean printError) {
		if (path == null) {
			return false;
		}

		try {
			return getRepositoryService().getExistence(path);
		} catch (Exception e) {
			if (printError) {
				LOGGER.log(Level.WARNING, "Error checking if file '" + path + "' exists", e);
			}
		}
		return false;
	}

	@Override
	public boolean existFileInRepository(String path) {
		return existFileInSlide(path, false);
	}

	@Override
	public String getUniqueIdByNumberAndDate(String path, String scope) {
		StringBuffer id = new StringBuffer();
		String postfix = null;
		if (StringUtil.isEmpty(path)) {
			postfix = String.valueOf(System.nanoTime());
		} else{
			postfix = getRepositoryService().createUniqueFileName(path, scope);
		}
		id.append(getRandomNumber(Integer.MAX_VALUE)).append(postfix);
		return id.toString();
	}

	@Override
	public String getLocalizedText(String key) {
		try {
			return ContentUtil.getBundle().getLocalizedString(key);
		} catch (Exception e) {
			e.printStackTrace();
			return key;
		}
	}

	@Override
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

	@Override
	public void addLoadedTheme(String id) {
		if (loadedThemes.contains(id)) {
			return;
		}
		loadedThemes.add(id);
	}

	@Override
	public int getLoadedThemesCount() {
		return loadedThemes.size();
	}

	@Override
	public boolean isCheckedFromRepository() {
		return checkedFromSlide;
	}

	/**
	 * Generates big and small previews for single theme
	 * @param theme
	 * @param useDraft
	 * @param isJpg
	 * @param quality
	 * @return true - success, false - failure
	 */
	@Override
	public boolean generatePreviewsForTheme(Theme theme, boolean useDraft, boolean isJpg, float quality) {
		String url = new StringBuilder(getFullWebRoot()).append(useDraft ? theme.getLinkToDraft() : theme.getLinkToSkeleton()).toString();
		String smallPreviewName = new StringBuffer(theme.getName()).append(ThemesConstants.THEME_SMALL_PREVIEW).toString();

		List<Dimension> dimensions = Arrays.asList(new Dimension(ThemesConstants.SMALL_PREVIEW_WIDTH, ThemesConstants.SMALL_PREVIEW_HEIGHT));

		ImageGenerator imageGenerator = getImageGenerator(null);

		List<BufferedImage> images = imageGenerator.generatePreviews(url, dimensions, isJpg, quality);
		if (images == null)
			return false;

		String extension = imageGenerator.getFileExtension();
		String mimeType = new StringBuffer(ThemesConstants.DEFAULT_MIME_TYPE).append(extension).toString();
		smallPreviewName = new StringBuffer(smallPreviewName).append(CoreConstants.DOT).append(extension).toString();

		BufferedImage image = null;
		InputStream stream = null;
		for (int i = 0; i < images.size(); i++) {
			image = images.get(i);
			stream = imageGenerator.getImageInputStream(image, extension);
			try {
				if (getRepositoryService().uploadFileAndCreateFoldersFromStringAsRoot(theme.getLinkToBaseAsItIs(), smallPreviewName, stream, mimeType))
					theme.setLinkToSmallPreview(smallPreviewName);
			} catch (Exception e) {
				e.printStackTrace();
				return false;
			} finally {
				IOUtil.closeInputStream(stream);
			}
		}

		return true;
	}

	@Override
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

	@Override
	public void addPredefinedThemeStyle(String uri) {
		if (predefinedThemeStyles.contains(uri)) {
			return;
		}

		predefinedThemeStyles.add(uri);
	}

	@Override
	public List<String> getPredefinedThemeStyles() {
		return predefinedThemeStyles;
	}

	@Override
	public String getBuiltInThemeStyleId(Theme theme) {
		String id = String.valueOf(getRandomNumber(Integer.MAX_VALUE));
		while (theme.getBuiltInThemeStyle(id) != null) {
			id = String.valueOf(getRandomNumber(Integer.MAX_VALUE));
		}
		return id;
	}

}