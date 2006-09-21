package com.idega.content.business;

import java.rmi.RemoteException;
import java.util.List;

import org.apache.commons.httpclient.HttpURL;
import org.apache.commons.httpclient.URIException;

import com.idega.business.IBOServiceBean;
import com.idega.slide.business.IWSlideService;
import com.idega.slide.business.IWSlideServiceBean;

public class ThemesPreviewsProviderBean extends IBOServiceBean implements ThemesPreviewsProvider {

	private static final long serialVersionUID = 5875353284352953688L;

	private static final String RESOURCE_ROOT = "/content";
	
	private static final String THEMES_PATH = "/files/public/themes/";
	
	private static final String THEMES_PREVIEW_PATH = THEMES_PATH + "preview/";
	
	private static final String JPEG_IMG_TYPE = ".jpeg";
	
	private static final String PNG_IMG_TYPE = ".png";
	
	private static IWSlideService sldSrv = null;
	
	public String getJpegImgType() {
		return JPEG_IMG_TYPE;
	}

	public String getPngImgType() {
		return PNG_IMG_TYPE;
	}

	public String getResourcePath() {
		return RESOURCE_ROOT;
	}

	public synchronized IWSlideService getSldSrv() {
		if (sldSrv == null) {
			sldSrv = new IWSlideServiceBean();
		}
		return sldSrv;
	}

	public String getThemesPath() {
		return THEMES_PATH;
	}

	public String getThemesPreviewPath() {
		return THEMES_PREVIEW_PATH;
	}

	public String getImagesInfo() {
		List images = getFiles(getThemesPreviewPath());
		String info = "";
		if (images == null) {
			return info;
		}
		info = "";//"{'items': [";
		String uri = null;
		String fileName = null;
		//String fileType = null;
		String webRoot = getWebRootWithoutContent(getFullWebRoot());
		for (int i = 0; i < images.size(); i++) {
			uri = images.get(i).toString();
			fileName = getFileName(uri);
			//info += "{";
			if (fileName == null) {
				fileName = "undefined_" + i;
			}
			info += fileName + "@";
			/*fileType = getFileType(uri);
			if (fileType != null) {
				fileType = "'type': '" + fileType + "', ";
			}
			else {
				fileType = "'type': 'png', ";
			}
			info += fileType;
			info += "'source': '" + webRoot + uri + "'}";
			if (i + 1 < images.size()) {
				info += ", ";
			}*/
			info += webRoot + uri;
			if (i + 1 < images.size()) {
				info += ";";
			}
		}
		//info += "]}";
		return info;
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
			files = getSldSrv().getChildPathsExcludingFoldersAndHiddenFiles(folderURI);
		} catch(RemoteException e) {
			e.printStackTrace();
		}
		return files;
	}
	
	public String getWebRootWithoutContent(String webRoot) {
		int contentIndex = webRoot.indexOf("/content");
		if (contentIndex != -1) {
			webRoot = webRoot.substring(0, contentIndex);
		}
		return webRoot;
	}
	
	public String getFullWebRoot() {
		String webRoot = null;
		HttpURL root = null;
		try {
			root = getSldSrv().getWebdavServerURL();
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

}
