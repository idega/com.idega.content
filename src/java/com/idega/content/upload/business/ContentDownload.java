package com.idega.content.upload.business;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import com.idega.content.presentation.WebDAVListManagedBean;
import com.idega.content.repository.download.RepositoryItemDownloader;
import com.idega.idegaweb.IWMainApplication;
import com.idega.util.CoreConstants;
import com.idega.util.CoreUtil;
import com.idega.util.URIUtil;

public class ContentDownload {

	/**
	 * Retrieve full url to file.
	 */
	public static String getUrl(String file) {
		return getUrl(Arrays.asList(file)).get(0);
	}

	/**
	 * Retrieve full URL to uploaded files.
	 */
	public static List<String> getUrl(List<String> files) {
		String mediaServletURI = CoreUtil.getIWContext().getIWMainApplication().getMediaServletURI();
		String writableClass = IWMainApplication.getEncryptedClassName(RepositoryItemDownloader.class);
		List<String> urls = new ArrayList<String>(files.size());
		for (String file: files) {
			URIUtil url = new URIUtil(mediaServletURI);
			url.setParameter(WebDAVListManagedBean.PARAMETER_WEB_DAV_URL, CoreConstants.WEBDAV_SERVLET_URI.concat(file));
			url.setParameter(RepositoryItemDownloader.PRM_WRITABLE_CLASS, writableClass);
			url.setParameter("allowAnonymous", Boolean.TRUE.toString());
			urls.add(url.getUri());
		}

		return urls;
	}

}
