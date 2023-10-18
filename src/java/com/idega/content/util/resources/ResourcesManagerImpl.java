package com.idega.content.util.resources;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Scope;
import org.springframework.stereotype.Service;

import com.idega.core.business.DefaultSpringBean;
import com.idega.idegaweb.IWMainApplication;
import com.idega.idegaweb.include.ExternalLink;
import com.idega.idegaweb.include.JavaScriptLink;
import com.idega.idegaweb.include.RSSLink;
import com.idega.idegaweb.include.StyleSheetLink;
import com.idega.jackrabbit.security.RepositoryAccessManager;
import com.idega.presentation.IWContext;
import com.idega.servlet.filter.IWBundleResourceFilter;
import com.idega.util.CoreConstants;
import com.idega.util.CoreUtil;
import com.idega.util.FileUtil;
import com.idega.util.IOUtil;
import com.idega.util.ListUtil;
import com.idega.util.StringHandler;
import com.idega.util.StringUtil;
import com.idega.util.expression.ELUtil;
import com.idega.util.resources.AbstractMinifier;
import com.idega.util.resources.CSSMinifier;
import com.idega.util.resources.JavaScriptMinifier;
import com.idega.util.resources.ResourcesAdder;
import com.idega.util.resources.ResourcesManager;

@Scope("request")
@Service(ResourcesManager.SPRING_BEAN_IDENTIFIER)
public class ResourcesManagerImpl extends DefaultSpringBean implements ResourcesManager {

	private static final long serialVersionUID = -3876318831841387443L;
	private static final Logger LOGGER = Logger.getLogger(ResourcesManagerImpl.class.getName());

	private static final String WEB_PAGE_RESOURCES = "idegaCoreWebPageResources";
	static final String CONCATENATED_RESOURCES = "idegaCoreConcatenatedRecources";

	@Autowired
	private RepositoryAccessManager repositoryAccessManager;

	private List<JavaScriptLink> javaScriptActions;
	private List<JavaScriptLink> javaScriptResources;
	private List<StyleSheetLink> cssFiles;
	private List<RSSLink> feedLinks;

	private Map<String, String> mediaMap;

	private RepositoryAccessManager getRepositoryAccessManager() {
		if (repositoryAccessManager == null) {
			ELUtil.getInstance().autowire(this);
		}
		return repositoryAccessManager;
	}

	@Override
	public List<JavaScriptLink> getJavaScriptResources() {
		if (javaScriptResources == null) {
			javaScriptResources = new ArrayList<>();
		}
		return javaScriptResources;
	}

	@Override
	public List<JavaScriptLink> getJavaScriptActions() {
		if (javaScriptActions == null) {
			javaScriptActions = new ArrayList<>();
		}
		return javaScriptActions;
	}

	@Override
	public List<StyleSheetLink> getCSSFiles() {
		if (cssFiles == null) {
			cssFiles = new ArrayList<>();
		}
		return cssFiles;
	}

	@Override
	public Map<String, String> getMediaMap() {
		if (mediaMap == null) {
			mediaMap = new HashMap<>();
		}
		return mediaMap;
	}

	private String getCachedConcatenatedResources(String resourceName) {
		String concatenatedResourcesUri = getCachedResource(CONCATENATED_RESOURCES, resourceName);
		if (StringUtil.isEmpty(concatenatedResourcesUri))
			return null;

		String cachedContent = getCachedResource(CONCATENATED_RESOURCES, new StringBuilder(resourceName.toString()).append("_content").toString());
		if (StringUtil.isEmpty(cachedContent))
			return null;

		return concatenatedResourcesUri;
	}

	private String getUriToConcatenatedResourcesFromCache(List<? extends ExternalLink> resources, String fileType) {
		if (ListUtil.isEmpty(resources) || StringUtil.isEmpty(fileType))
			return null;

		//	Will try to get big file URI from requested resources
		StringBuilder cacheName = new StringBuilder();
		for (ExternalLink resource: resources) {
			cacheName.append(resource.getUrl());
		}

		String uriToConcatenatedResources = getCachedConcatenatedResources(cacheName.toString());
		if (!StringUtil.isEmpty(uriToConcatenatedResources)) {
			resources.clear();
			addNotifierAboutLoadedCSSFiles(resources, fileType);
		}

		return uriToConcatenatedResources;
	}

	@Override
	public String getConcatenatedResources(List<? extends ExternalLink> resources, String fileType, String serverName) {
		if (ListUtil.isEmpty(resources))
			return null;

		//	Checking if ALL resources were concatenated already
		String uriToAllConcatenatedResources = getUriToConcatenatedResourcesFromCache(resources, fileType);
		if (!StringUtil.isEmpty(uriToAllConcatenatedResources)) {
			return uriToAllConcatenatedResources;
		}

		//	Didn't find concatenated file for ALL resources, will check every resource separately if it's available to be minified
		String resourceContent = null;
		List<ExternalLink> resourcesToLoad = new ArrayList<>();
		Map<String, String> addedResources = new HashMap<>();
		IWContext iwc = CoreUtil.getIWContext();
		for (ExternalLink resource: resources) {
			resourceContent = getResource(iwc, WEB_PAGE_RESOURCES, resource, serverName);
			if (StringUtil.isEmpty(resourceContent)) {
				if (resourceContent == null) {
					LOGGER.warning(new StringBuilder("Impossible to concatenate file: ").append(resource.getUrl()).toString());
				} else {
					resourceContent = new StringBuilder("/* ").append(resource.getUrl()).append(resourceContent == null ? " not available */" : " is empty */")
						.toString();
				}
			}

			if (!StringUtil.isEmpty(resourceContent)) {
				//	Resource is available to be concatenated
				resourcesToLoad.add(resource);
				addedResources.put(resource.getUrl(), resourceContent);
			}
		}

		if (ListUtil.isEmpty(addedResources.values()))
			return null;	//	Nothing to concatenate

		//	Removing AVAILABLE resources from request. Available resources will be concatenated to one big file
		resources.removeAll(resourcesToLoad);

		//	Checking if there is concatenated resource from ONLY AVAILABLE resources
		String concatenatedResourcesUri = getUriToConcatenatedResourcesFromCache(resources, fileType);
		if (!StringUtil.isEmpty(concatenatedResourcesUri))
			return concatenatedResourcesUri;

		//	Nothing found in cache, creating big file
		StringBuilder allResources = null;
		if (isJavaScriptFile(fileType)) {
			allResources = new StringBuilder("var IdegaResourcesHandler = [");
			allResources = addResourcesToList(allResources, resourcesToLoad);
			allResources.append("];\n");
		} else {
			addNotifierAboutLoadedCSSFiles(resourcesToLoad, fileType);
		}

		if (allResources == null)
			allResources = new StringBuilder();
		StringBuilder key = new StringBuilder();
		for (ExternalLink resource: resourcesToLoad) {
			allResources.append(addedResources.get(resource.getUrl()));
			key.append(resource);
		}
		concatenatedResourcesUri = copyConcatenatedResourcesToWebApp(allResources.toString(), fileType);
		if (StringUtil.isEmpty(concatenatedResourcesUri))
			return null;

		//	Putting in cache
		setCachedResource(CONCATENATED_RESOURCES, key.toString(), concatenatedResourcesUri);
		setCachedResource(CONCATENATED_RESOURCES, key.append("_content").toString(), allResources.toString());

		return concatenatedResourcesUri;
	}

	private void addNotifierAboutLoadedCSSFiles(List<? extends ExternalLink> resources, String fileType) {
		if (isJavaScriptFile(fileType))
			return;

		//	Notifying about CSS files
		String addedCSSFilesNotifier = getJavaScriptActionForLoadedCSSFiles(resources);
		if (StringUtil.isEmpty(addedCSSFilesNotifier))
			return;

		JavaScriptLink loadedCSSFilesAction = new JavaScriptLink();
		loadedCSSFilesAction.addAction(addedCSSFilesNotifier);
		if (!getJavaScriptActions().contains(loadedCSSFilesAction))
			getJavaScriptActions().add(loadedCSSFilesAction);
	}

	@Override
	public boolean isJavaScriptFile(String fileType) {
		return ResourcesAdder.FILE_TYPE_JAVA_SCRIPT.equals(fileType);
	}

	private String getJavaScriptActionForLoadedCSSFiles(List<? extends ExternalLink> cssFiles) {
		if (ListUtil.isEmpty(cssFiles)) {
			return null;
		}

		StringBuilder addedCSSFilesNotifier = new StringBuilder("IWCORE.addLoadedResources(");
		addedCSSFilesNotifier = addResourcesToList(addedCSSFilesNotifier, cssFiles);
		addedCSSFilesNotifier.append(");");
		return addedCSSFilesNotifier.toString();
	}

	@SuppressWarnings("unchecked")
	private StringBuilder addResourcesToList(StringBuilder content, List<? extends ExternalLink> resources) {
		for (Iterator<ExternalLink> resourcesIter = (Iterator<ExternalLink>) resources.iterator(); resourcesIter.hasNext();) {
			content.append(CoreConstants.QOUTE_SINGLE_MARK).append(resourcesIter.next().getUrl()).append(CoreConstants.QOUTE_SINGLE_MARK);
			if (resourcesIter.hasNext()) {
				content.append(CoreConstants.COMMA);
			}
		}
		return content;
	}

	private String copyConcatenatedResourcesToWebApp(String content, String fileType) {
		if (StringUtil.isEmpty(content)) {
			return null;
		}

		String fileName = new StringBuilder().append(getCachePrefix()).append(ResourcesAdder.OPTIMIZIED_RESOURCES).append(System.currentTimeMillis())
											.append(fileType).toString();
		String uriToResources = IWMainApplication.getDefaultIWMainApplication().getBundle(CoreConstants.CORE_IW_BUNDLE_IDENTIFIER)
																			.getVirtualPathWithFileNameString(fileName);

		File file = IWBundleResourceFilter.copyResourceFromJarOrCustomContentToWebapp(IWMainApplication.getDefaultIWMainApplication(), uriToResources, content);
		return (file == null || !file.exists()) ? null : uriToResources;
	}

	private String getCachedResource(String cacheName, String resourceName) {
		String prefix = getCachePrefix();
		try {
			Map<String, String> cache = getCache(new StringBuilder().append(prefix).append(cacheName).toString());
			return cache.get(resourceName);
		} catch(Exception e) {
			LOGGER.log(Level.WARNING, "Error putting resource to cache: " + cacheName, e);
		}
		return null;
	}

	private boolean setCachedResource(String cacheName, String key, String value) {
		String prefix = getCachePrefix();
		try {
			Map<String, String> cache = getCache(new StringBuilder().append(prefix).append(cacheName).toString());
			String objectInCache = cache.put(key, value);
			return objectInCache != null;
		} catch(Exception e) {
			LOGGER.log(Level.WARNING, "Error putting resource to cache: " + cacheName, e);
		}
		return false;
	}

	static final String getCachePrefix() {
		try {
			return IWMainApplication.getDefaultIWMainApplication().getSettings().getProperty("idega_core.resources_prefix", CoreConstants.EMPTY);
		} catch(Exception e) {
			LOGGER.log(Level.WARNING, "Error getting application property", e);
		}
		return CoreConstants.EMPTY;
	}

	private String getResource(IWContext iwc, String cacheName, ExternalLink resource, String serverName) {
		String minifiedResource = getCachedResource(cacheName, resource.getUrl());
		if (!StringUtil.isEmpty(minifiedResource)) {
			return minifiedResource;
		}

		String fileContent = null;
		File resourceInWebApp = null;
		String path = resource.getUrl();
		String repoStart = CoreConstants.WEBDAV_SERVLET_URI + CoreConstants.PATH_FILES_ROOT;
		if (!StringUtil.isEmpty(path) && path.indexOf(repoStart) != -1) {
			path = path.substring(path.indexOf(repoStart));
			resource.setUrl(path);
			try {
				if (getRepositoryAccessManager().hasPermission(iwc, path)) {
					fileContent = StringHandler.getContentFromInputStream(getRepositoryService().getInputStreamAsRoot(path));
				}
				if (StringUtil.isEmpty(fileContent)) {
					LOGGER.warning("Error getting contents from file from repository: " + path);
				} else {
					LOGGER.info("Successfully got contents of file from repository: " + path);
				}
			} catch (Exception e) {
				LOGGER.log(Level.WARNING, "Error getting file from repository: " + path, e);
			}
		} else  {
			LOGGER.info("Proceeding file: " + path);
			resourceInWebApp = IWBundleResourceFilter.copyResourceFromJarToWebapp(IWMainApplication.getDefaultIWMainApplication(), path);
		}

		if (fileContent == null) {
			minifiedResource = (resourceInWebApp == null || !resourceInWebApp.exists()) ?
					getMinifiedResourceFromApplication(iwc, serverName, resource) :
					getMinifiedResourceFromWorkspace(resourceInWebApp, resource);
		} else {
			resource.setContent(fileContent);
			minifiedResource = getMinifiedResource(resource);
		}

		if (minifiedResource == null) {
			return null;
		}

		setCachedResource(cacheName, path, minifiedResource);
		return minifiedResource;
	}

	private InputStream getStreamFromRepository(IWContext iwc, String path) {
		if (StringUtil.isEmpty(path)) {
			return null;
		}

		if (path.startsWith(CoreConstants.WEBDAV_SERVLET_URI) || path.startsWith(CoreConstants.PATH_FILES_ROOT)) {
			try {
				if (getRepositoryAccessManager().hasPermission(iwc, path)) {
					return getRepositoryService().getInputStreamAsRoot(path);
				}
			} catch (Exception e) {
				LOGGER.log(Level.WARNING, "Error getting input stream from repository: " + path, e);
			}
		}

		return null;	//	Object not in repository or error occurred
	}

	private String getMinifiedResourceFromApplication(IWContext iwc, String serverURL, ExternalLink resource) {
		String resourceURI = resource.getUrl();

		InputStream input = getStreamFromRepository(iwc, resourceURI);
		if (input == null) {
			IWMainApplication iwma = IWMainApplication.getDefaultIWMainApplication();
			input = iwma.getResourceAsStream(resourceURI);
		}

		if (input == null) {
			if (resourceURI.startsWith(CoreConstants.SLASH)) {
				resourceURI = resourceURI.replaceFirst(CoreConstants.SLASH, CoreConstants.EMPTY);
			}

			String fullLink = new StringBuilder(serverURL).append(resourceURI).toString();
			URL url = null;
			try {
				url = new URL(fullLink);
			} catch (MalformedURLException e) {
				LOGGER.warning("Error getting resource from: " + fullLink);
			}
			if (url == null) {
				return null;
			}

			try {
				input = url.openStream();
			} catch (IOException e) {
				LOGGER.warning("Error getting resource from: " + fullLink);
			}
		}

		if (input == null) {
			return null;
		}

		resource.setContentStream(input);

		String minified = null;
		try {
			minified = getMinifiedResource(resource);
		} catch(Exception e) {
			LOGGER.log(Level.WARNING, "Error getting resource ('" + resourceURI + "') from stream!", e);
		} finally {
			IOUtil.closeInputStream(input);
		}

		return minified;
	}

	private String getMinifiedResourceFromWorkspace(File resource, ExternalLink resourceLink) {
		if (resource == null || !resource.exists()) {
			return null;
		}

		String fileContent = null;
		try {
			fileContent = FileUtil.getStringFromFile(resource);
		} catch (IOException e) {
			LOGGER.log(Level.WARNING, "Error getting content from file: " + resource.getName(), e);
		}
		if (StringUtil.isEmpty(fileContent)) {
			LOGGER.warning("No content in file " + resource.getName());
			return null;
		}

		resourceLink.setContent(fileContent);

		String minified = null;
		try {
			minified = getMinifiedResource(resourceLink);
		} catch(Exception e) {
			LOGGER.log(Level.WARNING, "Error while minifying resource: " + resource.getName(), e);
			return fileContent;
		}
		if (StringUtil.isEmpty(minified)) {
			LOGGER.log(Level.WARNING, "Error while minifying resource: " + resource.getName());
			return fileContent;
		}

		return minified;
	}

	private String getMinifiedResource(ExternalLink resource) {
		AbstractMinifier minifier = resource instanceof StyleSheetLink ? new CSSMinifier() : new  JavaScriptMinifier();
		return minifier.getMinifiedResource(resource);
	}

	@Override
	public List<RSSLink> getFeedLinks() {
		if (feedLinks == null) {
			feedLinks = new ArrayList<>();
		}
		return feedLinks;
	}
}