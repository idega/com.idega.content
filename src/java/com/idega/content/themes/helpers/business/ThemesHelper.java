package com.idega.content.themes.helpers.business;

import java.io.InputStream;
import java.util.Collection;
import java.util.List;
import java.util.Map;

import org.jdom.Document;

import com.idega.business.SpringBeanName;
import com.idega.content.themes.business.ThemesService;
import com.idega.content.themes.helpers.bean.Setting;
import com.idega.content.themes.helpers.bean.Theme;
import com.idega.core.search.business.SearchResult;
import com.idega.graphics.image.business.ImageGenerator;
import com.idega.presentation.IWContext;
import com.idega.repository.RepositoryService;

@SpringBeanName("themesHelper")
public interface ThemesHelper {

	public ImageGenerator getImageGenerator(IWContext iwc);

	public RepositoryService getRepositoryService();

	public void searchForThemes();

	public List<String> loadSearchResults(List<SearchResult> searchResults, List<String> filter);

	public List<SearchResult> search(String searchKey, String searchScope);

	public String getFileName(String uri);

	public String getFileNameWithExtension(String uri);

	public String extractValueFromString(String fullString, int beginIndex, int endIndex);

	public String getFileExtension(String uri);

	public String getWebRootWithoutContent();

	public String getWebRootWithoutContent(String fullWebRoot);

	public String getFullWebRoot();

	public boolean isCorrectFile(String fileName, String nameTemplate);

	public boolean isCorrectThemeTemplateFile(String fileName, List<String> filter);

	public boolean isCreatedManually(String fileName);

	public boolean isDraft(String fileName);

	public boolean isSystemFile(String fileName);

	public boolean isPropertiesFile(String uri);

	public void addTheme(Theme theme);

	public List<Theme> getAvailableThemes();

	public Collection<Theme> getAllThemes();

	public List<Theme> getSortedThemes();

	public void addUriToTheme(String uri);

	public  boolean existTheme(String uri);

	public Document getXMLDocument(String url);

	public Document getXMLDocument(String url, boolean cleanWithHtmlCleaner, boolean omitComments);
	public Document getXMLDocument(String url, boolean cleanWithHtmlCleaner, boolean omitComments, boolean omitDocTypeDeclaaration);

	public Document getXMLDocument(InputStream stream) throws Exception;

	public String getLinkToBase(String uri);

	public Theme getTheme(String themeKey);

	public void removeTheme(String uri, String themeKey);

	public Map<String, Theme> getThemes();

	public List<Setting> getThemeSettings();

	public List<Setting> getPageSettings();

	public void loadThemeSettings(InputStream stream);

	public void loadPageSettings(String url);

	public InputStream getInputStream(String link);

	public String encode(String value, boolean fullyEncode);

	public String urlEncode(String url);

	public String decode(String value, boolean fullyDecode);

	public String decodeUrl(String url);

	public boolean createSmallImage(Theme theme);

	public boolean createSmallImage(Theme theme, boolean useDraftPreview);

	public ThemesService getThemesService();

	public boolean createThemeConfig(Theme theme);

	public String getPreparedThemeNameToUseInRepository(Theme theme);

	public String getPreparedThemeNameToUseInRepository(String themeName);

	public String[] getPageValues(Setting s, String value);

	public void addThemeToQueue(String linkToBase);

	public void removeThemeFromQueue(String linkToBase);

	public String getLastVisitedPage();

	public void setLastVisitedPage(String lastVisitedPage);

	public String getLastUsedTheme();

	public Theme getThemeByTemplateKey(String templateKey);

	public void setLastUsedTheme(String templateId);

	public String getDefaultTheme();

	public String loadPageToSlide(String type, String templateFile, List<String> articlesPaths, int pageID);

	public String getFixedSlideFileName(String fileName);

	public String changeFileUploadPath(String path);

	public int getRandomNumber(int maxValue);

	public void removeLastUsedTheme(String templateID);

	public List<String> createArticle(String templateFile, int id);

	public String getArticleCommentLink(IWContext iwc, String pageURI);

	public String getFullServerName(IWContext iwc);

	public boolean setNewLinkInArticleFile(IWContext iwc, String link, String language, String baseDirectory, String pageUri);

	public boolean existFileInSlide(String path);

	public String getUniqueIdByNumberAndDate(String scope);

	public String getLocalizedText(String key);

	public String getCurrentLanguage(IWContext iwc);

	public void addLoadedTheme(String id);

	public int getLoadedThemesCount();

	public boolean isCheckedFromSlide();

	/**
	 * Generates big and small previews for single theme
	 * @param theme
	 * @param useDraft
	 * @param isJpg
	 * @param quality
	 * @return true - success, false - failure
	 */
	public boolean generatePreviewsForTheme(Theme theme, boolean useDraft, boolean isJpg, float quality);

	public String getThemeColourFileName(Theme theme, String customName, String file, boolean markAsOriginalFile);

	public void addPredefinedThemeStyle(String uri);

	public List<String> getPredefinedThemeStyles();

	public String getBuiltInThemeStyleId(Theme theme);

}