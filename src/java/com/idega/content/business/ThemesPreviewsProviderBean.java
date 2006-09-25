package com.idega.content.business;

import java.rmi.RemoteException;
import java.util.ArrayList;
import java.util.List;

import org.apache.commons.httpclient.HttpURL;
import org.apache.commons.httpclient.URIException;
import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import com.idega.business.IBOLookup;
import com.idega.business.IBOLookupException;
import com.idega.business.IBOServiceBean;
import com.idega.graphics.WebPagePreviewGenerator;
import com.idega.graphics.PreviewGenerator;
import com.idega.presentation.IWContext;
import com.idega.slide.business.IWSlideService;

public class ThemesPreviewsProviderBean extends IBOServiceBean implements ThemesPreviewsProvider {

	private static final long serialVersionUID = 5875353284352953688L;
	private static final Log log = LogFactory.getLog(ThemesPreviewsProviderBean.class);

	private static final String RESOURCE_ROOT = "/content";
	private static final String THEMES_PATH = "/files/public/themes/";
	private static final String THEMES_PREVIEW_PATH = THEMES_PATH + "preview/";
	private static final String JPEG_IMG_TYPE = ".jpeg";
	private static final String PNG_IMG_TYPE = ".png";
	private static final String PREVIEW_GENERATED = "2";
	private static final String ERROR_OCURRED = "1";
	private static final String NO_ACTION_NEEDED = "0";
	private static final String THEME_NAME = ContentUtil.getBundle().getLocalizableStringDefaultValue("theme_name");
	private static final String UNDEFINED_FILE_NAME = "undefined_";
	
	private volatile static PreviewGenerator generator = null;
	
	private List <String> needsPreview = null; // Files with no preview image
	private List <String> previewsNames = null; // Names of previews
	
	/**
	 * Returns jpeg image file extension
	 */
	public String getJpegImgType() {
		return JPEG_IMG_TYPE;
	}

	/**
	 * Returns png image file extension
	 */
	public String getPngImgType() {
		return PNG_IMG_TYPE;
	}

	/**
	 * Returns resource URI
	 */
	public String getResourcePath() {
		return RESOURCE_ROOT;
	}

	/**
	 * Returns instance of IWSlideService
	 */
	public IWSlideService getSlideService() {
		IWSlideService service = null;
		try {
			service = (IWSlideService) IBOLookup.getServiceInstance(IWContext.getInstance(), IWSlideService.class);
		} catch (IBOLookupException e) {
			e.printStackTrace();
			log.error(e);
		}
		return service;
	}

	/**
	 * Returns themes URI
	 */
	public String getThemesPath() {
		return THEMES_PATH;
	}

	/**
	 * Returns themes previews URI
	 */
	public String getThemesPreviewPath() {
		return THEMES_PREVIEW_PATH;
	}

	/**
	 * Returns info about themes previews in slide
	 */
	public String getThemesPreviewsInfo() {
		return getThemesPreviewsInfo(true);
	}
	
	/**
	 * Returns info about themes previews in slide
	 * @param checkFromSlide - checks if any preview should be generated before returning info
	 */
	private String getThemesPreviewsInfo(boolean checkFromSlide) {
		if (checkFromSlide) {
			if (ERROR_OCURRED.equals(getPagePreview())) {
				log.error("Unable to get info from slide");
			}
		}
		List images = getFiles(getThemesPreviewPath());
		StringBuffer info = new StringBuffer();
		if (images == null) {
			return info.toString();
		}
		String uri = null;
		String fileName = null;
		String webRoot = getWebRootWithoutContent(getFullWebRoot());
		for (int i = 0; i < images.size(); i++) {
			uri = images.get(i).toString();
			fileName = getFileName(uri);
			if (fileName == null) {
				fileName = UNDEFINED_FILE_NAME + i;
			}
			info.append(THEME_NAME);
			info.append(": ");
			info.append(fileName);
			info.append("@");
			info.append(webRoot);
			info.append(uri);
			if (i + 1 < images.size()) {
				info.append(";");
			}
		}
		return info.toString();
	}
	
	public String getFileName(String uri) {
		String name = null;
		int begin = uri.lastIndexOf("/");
		int end = uri.lastIndexOf(".");
		if (begin != -1 && end != -1) {
			name = uri.substring(begin + 1, end);
		}
		return name;
	}
	
	public String getFileType(String uri) {
		String type = null;
		int begin = uri.lastIndexOf(".");
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
	
	public String getWebRootWithoutContent(String webRoot) {
		int contentIndex = webRoot.indexOf(RESOURCE_ROOT);
		if (contentIndex != -1) {
			webRoot = webRoot.substring(0, contentIndex);
		}
		return webRoot;
	}
	
	public String getFullWebRoot() {
		String webRoot = null;
		HttpURL root = null;
		try {
			root = getSlideService().getWebdavServerURL();
		} catch (RemoteException e) {
			e.printStackTrace();
			return null;
		}
		try {
			webRoot = root.getURI();
		} catch (URIException e) {
			e.printStackTrace();
			return null;
		}
		return webRoot;
	}
	
	public String getPagePreview() {
		if (isNeededThemesPreviewGeneration()) {
//			needsPreview.add("http://www.fiba2006.com");
//			previewsNames.add("fiba2006_original");
//			needsPreview.add("http://www.w3.org");
//			previewsNames.add("w3_original");
			if (getPreviewGenerator().generatePreview(needsPreview, previewsNames, getThemesPreviewPath(), 320, 240)) {
				return PREVIEW_GENERATED; // Generated preview
			}
			return ERROR_OCURRED; // Error occured
		}
		return NO_ACTION_NEEDED; // No action needed
	}
	
	/**
	 * Checks if any theme is without preview image
	 */
	private boolean isNeededThemesPreviewGeneration() {
		List themes = getFiles(getThemesPath());
		if (themes == null) {
			return false;
		}
		if (themes.size() == 0) {
			return false;
		}
		return prepareThemesForPreview(themes, getFiles(getThemesPreviewPath()));
	}
	
	public PreviewGenerator getPreviewGenerator() {
		if (generator == null) {
			synchronized (ThemesPreviewsProviderBean.class) { // Using synhronization only once
				if (generator == null) {
					generator = new WebPagePreviewGenerator();
				}
			}
		}
		return generator;
	}
	
	/**
	 * Finds themes these do not have previews and makes a list of them
	 * @param themes - all themes
	 * @param previews - existing image files of themes
	 */
	private boolean prepareThemesForPreview(List themes, List previews) {
		String webRoot = getFullWebRoot();
		if (webRoot == null) {
			return false;
		}
		String webRootWithoutContent = getWebRootWithoutContent(webRoot);
		List <String> urls = new ArrayList <String> ();
		String uri = null;
		String fileName = null;
		String imgBase = null;
		previewsNames = new ArrayList <String> ();
		for (int i = 0; i < themes.size(); i++) {
			uri = themes.get(i).toString();
			fileName = getFileName(uri);
			if (fileName == null) {
				return false;
			}
			imgBase = getResourcePath() + getThemesPreviewPath() + fileName;
			boolean includeURI = true;
			if (previews != null) {
				String jpegImg = imgBase + getJpegImgType();
				String pngImg = imgBase + getPngImgType();
				includeURI = !(previews.contains(jpegImg) || previews.contains(pngImg));
				jpegImg = null;
				pngImg = null;
			}
			if (includeURI) {
				urls.add(webRootWithoutContent + uri);
				previewsNames.add(fileName);
			}
		}
		if (urls.size() > 0) { 
			setNeedsPreview(urls);
			return true;
		}
		return false;
	}
	
	private void setNeedsPreview(List <String> urls) {
		needsPreview = urls;
	}

}