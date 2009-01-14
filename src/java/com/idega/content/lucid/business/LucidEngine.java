package com.idega.content.lucid.business;

import java.io.Serializable;
import java.rmi.RemoteException;
import java.util.List;

import javax.faces.model.SelectItem;

import org.jdom.Document;

import com.idega.content.lucid.bean.LucidApplicationInfo;
import com.idega.content.themes.helpers.bean.PageAccessibilityProperty;
import com.idega.content.themes.helpers.bean.TreeNodeStructure;
import com.idega.core.builder.business.BuilderService;
import com.idega.core.builder.data.ICDomain;
import com.idega.idegaweb.IWMainApplicationSettings;

public interface LucidEngine extends Serializable {

	public static final String SPRING_BEAN_IDENTIFIER = "lucidEngine";
	
	public String getJavaScriptResources();
	
	public String getStyleSheetResources();
	
	public String getJavaScriptResourcesForThemes();
	
	public String getStyleSheetResourcesForThemes();
	
	public List<String> getPermissionWindowResources();
	
	public List<String> getPropertiesWindowResources();
	
	public boolean isContentEditor();
	
	public boolean isSuperAdmin();
	
	public boolean setLocale(String locale);
	
	public List<SelectItem> getAvailableLocales();
	
	public String getCurrentLocaleValue();
	
	/** From ThemesEngine **/
	public String changePageUri(String pageKey, String pageTitle, boolean needSetPageTitle) throws RemoteException;

	public boolean setNewLinkInArticleFile(String pageKey, String moduleClass, String pageUri) throws RemoteException;

	public String savePageInfo(String pageKey, String[] keywords, String[] values) throws RemoteException;

	public String[] getPageInfoValues(String pageKey, String[] keywords) throws RemoteException;
	
	public String[] getPageInfoElements() throws RemoteException;
	
	public String[] getSiteInfoElements() throws RemoteException;

	public String[] getSiteInfoValues(String[] keywords, String language) throws RemoteException;

	public String getSiteInfoValue(String keyword, String language, IWMainApplicationSettings settings, ICDomain domain);

	public boolean saveSiteInfoValue(String keyword, String value) throws RemoteException;
	
	public List<String> createPage(List<TreeNodeStructure> struct, Boolean isTopLevelPage, String numberInLevel, List<String> followingNodes) throws RemoteException;

	public boolean deletePage(String pageKey, boolean deleteChildren) throws RemoteException;

	public boolean deletePageAndDecrease(String pageKey, boolean deleteChildren, List<String> followingNodes) throws RemoteException;

	public String getPageId() throws RemoteException;

	public boolean setPageId(String id) throws RemoteException;

	public boolean movePage(int newParentId, int nodeId, int numberInLevel, List<String> nodesToIncrease, List<String> nodesToDecrease) throws RemoteException;

	public String getPathToImageFolder() throws RemoteException;
	
	public boolean saveSiteInfo(String language, String[] keywords, String[] values);

	public boolean isStartPage(String pageKey) throws RemoteException;

	public boolean setAsStartPage(String pageKey) throws RemoteException;

	public String createRootTemplate(ICDomain domain, BuilderService builder, int domainID, String format);

	public boolean initializeCachedDomain(String domainName, ICDomain domain);
	
	public boolean canUserActAsBuilderUser();

	public String getPageUri(String pageKey);
	
	public boolean changePageName(int id, String newName);
	
	public boolean deleteArticlesFromDeletedPages(String pageKey);
	
	public boolean deleteArticle(String resourcePath);
	
	public Document getRenderedPageInfo(String pageKey, String id, String styleClass);
	
	public String changePageUriAfterPageWasMoved(String pageKey);
	
	public Document getReRenderedSiteInfo(String id, String styleClass);
	
	public String getPageIdByUri(String uri);
	
	public List<PageAccessibilityProperty> getPageAccessibilityProperties(String pageKey);
	
	public boolean setPageAccessibilityProperty(String pageKey, String code, String value, String columnName);
	/** END from ThemesEngine **/
	
	public LucidApplicationInfo getStartInfo(Boolean fullInfo);
}
