package com.idega.content.upload.business;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import com.idega.content.presentation.WebDAVListManagedBean;
import com.idega.content.repository.download.RepositoryItemDownloader;
import com.idega.idegaweb.IWMainApplication;
import com.idega.util.CoreConstants;
import com.idega.util.CoreUtil;


public class ContentUrl {

	/**
	 * Retrieve full url to file.
	 */
	public static String getUrl(String file) {
		return getUrl(Arrays.asList(file)).get(0);
	}

	
	/**
	 * Retrieve download url to file.
	 */
	public static String getDownloadUrl(String file) {
		return getDownloadUrl(Arrays.asList(file)).get(0);
	}
	
	/**
	 * Retrieve full url to file.
	 */
	public static List<String> getUrl(List<String> files) {
		List<String> urls = new ArrayList<String>(files.size());
		for (String file: files) {
			urls.add(new StringBuilder("/content/").append(file).toString());
		}
		return urls;
	}
	

	/**
	 * Retrieve download URL to uploaded files. 
	 */
	public static List<String> getDownloadUrl(List<String> files) {

		List<String> urls = new ArrayList<String>(files.size());
		for (String file: files) {

			StringBuilder url = new StringBuilder();
			url.append(CoreUtil.getIWContext().getIWMainApplication().getMediaServletURI());

			url.append("?")
				.append(WebDAVListManagedBean.PARAMETER_WEB_DAV_URL)
				.append("=")
				.append(CoreConstants.WEBDAV_SERVLET_URI)
				.append(file);

			String writableClass = IWMainApplication.getEncryptedClassName(RepositoryItemDownloader.class);
			url.append("&")
				.append(RepositoryItemDownloader.PRM_WRITABLE_CLASS)
				.append("=")
				.append(writableClass);

			url.append("&allowAnonymous=true");
			
			urls.add(url.toString());
		}

		return urls;
	}


}
