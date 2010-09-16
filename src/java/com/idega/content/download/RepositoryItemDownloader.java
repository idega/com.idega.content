package com.idega.content.download;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.servlet.http.HttpServletRequest;

import org.apache.webdav.lib.WebdavResource;

import com.idega.business.IBOLookup;
import com.idega.business.IBOLookupException;
import com.idega.content.presentation.WebDAVListManagedBean;
import com.idega.content.repository.bean.WebDAVItem;
import com.idega.core.file.util.MimeTypeUtil;
import com.idega.idegaweb.IWMainApplication;
import com.idega.io.DownloadWriter;
import com.idega.presentation.IWContext;
import com.idega.repository.bean.RepositoryItem;
import com.idega.slide.business.IWSlideService;
import com.idega.util.CoreConstants;
import com.idega.util.CoreUtil;
import com.idega.util.FileUtil;
import com.idega.util.IOUtil;
import com.idega.util.ListUtil;

public class RepositoryItemDownloader extends DownloadWriter {

	private static final Logger LOGGER = Logger.getLogger(RepositoryItemDownloader.class.getName());
	
	private String url, mimeType;
	
	private boolean folder;
	
	@Override
	public String getMimeType() {
		return mimeType;
	}

	@Override
	public void init(HttpServletRequest req, IWContext iwc) {
		if (!iwc.isLoggedOn() && !iwc.isSuperAdmin()) {
			return;
		}
		
		url = iwc.getParameter(WebDAVListManagedBean.PARAMETER_WEB_DAV_URL);
		folder = Boolean.valueOf(iwc.getParameter(WebDAVListManagedBean.PARAMETER_IS_FOLDER));
		mimeType = folder ? MimeTypeUtil.MIME_TYPE_ZIP : MimeTypeUtil.resolveMimeTypeFromFileName(url);
	}

	@Override
	public void writeTo(OutputStream out) throws IOException {
		IWSlideService repository = null;
		try {
			repository = getRepository();
		} catch (IBOLookupException e) {
			LOGGER.log(Level.SEVERE, "Error getting repository service!", e);
		}
		
		if (folder) {
			//	ZIP the contents of the folder and write to the output stream
			File zippedContents = getZippedContents(repository);
			try {
				FileUtil.streamToOutputStream(new FileInputStream(zippedContents), out);
			} finally {
				if (zippedContents != null) {
					zippedContents.delete();
				}
			}
		} else {
			//	Writing the contents of selected file to the output stream
			InputStream stream = repository.getInputStream(url);
			IWContext iwc = CoreUtil.getIWContext();
			if (iwc != null) {
				String fileName = getFileName(url);
				byte[] bytes = IOUtil.getBytesFromInputStream(stream);
				setAsDownload(iwc, fileName, bytes == null ? 0 : bytes.length);
				stream = new ByteArrayInputStream(bytes);
			}
			FileUtil.streamToOutputStream(stream, out);
		}
	}
	
	private String getFileName(String url) {
		String fileName = url;
		if (fileName.endsWith(CoreConstants.SLASH)) {
			fileName = fileName.substring(0, fileName.lastIndexOf(CoreConstants.SLASH));
		}
		if (fileName.indexOf(CoreConstants.SLASH) != -1) {
			fileName = fileName.substring(fileName.lastIndexOf(CoreConstants.SLASH) + 1);
		}
		return fileName;
	}
	
	private File getZippedContents(IWSlideService repository) throws IOException {
		String fileName = getFileName(url).concat(".zip");
		Collection<RepositoryItem> itemsToZip = new ArrayList<RepositoryItem>();
		
		long start = System.currentTimeMillis();	//	TODO
		
		WebdavResource folder = repository.getWebdavResourceAuthenticatedAsRoot(url);
		addItemsOfFolder(folder, itemsToZip);
		LOGGER.info("Items to zip: " + itemsToZip);
		
		long end = System.currentTimeMillis();		//	TODO
		LOGGER.info("Items to zip resolved in: " + (end - start) + " ms.");
		
		File zippedContents = FileUtil.getZippedFiles(itemsToZip, fileName, false, true);
		if (zippedContents == null) {
			return null;
		}
		
		IWContext iwc = CoreUtil.getIWContext();
		if (iwc != null) {
			setAsDownload(iwc, zippedContents.getName(), Long.valueOf(zippedContents.length()).intValue());
		}
		
		return zippedContents;
	}
	
	@SuppressWarnings("unchecked")
	private void addItemsOfFolder(WebdavResource folder, Collection<RepositoryItem> itemsToZip) throws IOException {
		List<WebdavResource> resources = Collections.list(folder.getChildResources().getResources());
		if (ListUtil.isEmpty(resources)) {
			return;
		}
		
		for (WebdavResource resource: resources) {
			if (resource.isCollection()) {
				String currentDirectory = resource.toString();
				long start = System.currentTimeMillis();	//	TODO
				LOGGER.info("Adding items from a directory: " + currentDirectory);
				addItemsOfFolder(resource, itemsToZip);
				long end = System.currentTimeMillis();		//	TODO
				LOGGER.info("Items of " + currentDirectory + " added in: " + (end - start) + " ms.");
			} else {
				itemsToZip.add(new WebDAVItem(resource, url));
			}
		}
	}

	private IWSlideService getRepository() throws IBOLookupException {
		return IBOLookup.getServiceInstance(IWMainApplication.getDefaultIWApplicationContext(), IWSlideService.class);
	}
}