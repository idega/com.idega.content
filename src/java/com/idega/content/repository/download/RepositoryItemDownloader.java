package com.idega.content.repository.download;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.ArrayList;
import java.util.Collection;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jcr.Node;
import javax.jcr.NodeIterator;
import javax.jcr.RepositoryException;
import javax.servlet.http.HttpServletRequest;

import org.apache.jackrabbit.JcrConstants;

import com.idega.business.IBOLookupException;
import com.idega.content.presentation.WebDAVListManagedBean;
import com.idega.core.file.util.MimeTypeUtil;
import com.idega.io.DownloadWriter;
import com.idega.jackrabbit.bean.JackrabbitRepositoryItem;
import com.idega.presentation.IWContext;
import com.idega.repository.RepositoryService;
import com.idega.repository.bean.RepositoryItem;
import com.idega.user.data.bean.User;
import com.idega.util.CoreConstants;
import com.idega.util.CoreUtil;
import com.idega.util.FileUtil;
import com.idega.util.IOUtil;
import com.idega.util.StringUtil;
import com.idega.util.expression.ELUtil;

public class RepositoryItemDownloader extends DownloadWriter {

	public static final String PARAMETER_URL = WebDAVListManagedBean.PARAMETER_WEB_DAV_URL;
	private static final Logger LOGGER = Logger.getLogger(RepositoryItemDownloader.class.getName());

	private String url, mimeType;

	private boolean folder, allowAnonymous;

	public String getUrl() {
		return url;
	}

	public void setUrl(String url) {
		this.url = url;
	}

	public boolean isFolder() {
		return folder;
	}

	public void setFolder(boolean folder) {
		this.folder = folder;
	}

	public void setMimeType(String mimeType) {
		this.mimeType = mimeType;
	}

	public boolean isAllowAnonymous() {
		return allowAnonymous;
	}

	public void setAllowAnonymous(boolean allowAnonymous) {
		this.allowAnonymous = allowAnonymous;
	}

	@Override
	public String getMimeType() {
		return mimeType;
	}

	@Override
	public void init(HttpServletRequest req, IWContext iwc) {
		allowAnonymous = iwc.isParameterSet("allowAnonymous") ? Boolean.valueOf(iwc.getParameter("allowAnonymous")) : allowAnonymous;

		if (!allowAnonymous && !iwc.isLoggedOn() && !iwc.isSuperAdmin())
			return;

		url = iwc.getParameter(WebDAVListManagedBean.PARAMETER_WEB_DAV_URL);
		url = url == null ? iwc.getParameter(WebDAVListManagedBean.PARAMETER_WEB_DAV_URL) : url;
		folder = Boolean.valueOf(iwc.getParameter(WebDAVListManagedBean.PARAMETER_IS_FOLDER));
		mimeType = folder ? MimeTypeUtil.MIME_TYPE_ZIP : MimeTypeUtil.resolveMimeTypeFromFileName(url);
	}

	@Override
	public void writeTo(OutputStream out) throws IOException {
		RepositoryService repository = getRepository();
		try {
			repository = getRepository();
		} catch (IBOLookupException e) {
			LOGGER.log(Level.SEVERE, "Error getting repository service!", e);
		}

		if (folder) {
			User user = CoreUtil.getIWContext().getLoggedInUser();
			//	ZIP the contents of the folder and write to the output stream
			File zippedContents = getZippedContents(repository, user);
			try {
				FileUtil.streamToOutputStream(new FileInputStream(zippedContents), out);
			} finally {
				if (zippedContents != null) {
					zippedContents.delete();
				}
			}
		} else {
			//	Writing the contents of selected file to the output stream
			InputStream stream = null;
			try {
				stream = repository.getInputStream(url);
			} catch (RepositoryException e) {
				e.printStackTrace();
			}
			if (stream == null)
				return;

			Boolean success = Boolean.TRUE;
			IWContext iwc = CoreUtil.getIWContext();
			try {
				String fileName = getFileName(url);
				byte[] bytes = IOUtil.getBytesFromInputStream(stream);
				setAsDownload(iwc, fileName, bytes == null ? 0 : bytes.length);
				stream = new ByteArrayInputStream(bytes);
				FileUtil.streamToOutputStream(stream, out);
			} catch (IOException e) {
				IOUtil.close(stream);
				success = Boolean.FALSE;
				LOGGER.log(Level.WARNING, "Error downloading file: " + url, e);
			}

			if (success) {
				out.flush();
				IOUtil.closeOutputStream(out);
				return;
			}

			super.writeTo(out);
		}
	}

	private String getFileName(String url) {
		if (StringUtil.isEmpty(url))
			return CoreConstants.EMPTY;

		String fileName = url;
		if (fileName.endsWith(CoreConstants.SLASH)) {
			fileName = fileName.substring(0, fileName.lastIndexOf(CoreConstants.SLASH));
		}
		if (fileName.indexOf(CoreConstants.SLASH) != -1) {
			fileName = fileName.substring(fileName.lastIndexOf(CoreConstants.SLASH) + 1);
		}
		return fileName;
	}

	private File getZippedContents(RepositoryService repository, User user) throws IOException {
		Node folder = null;
		try {
			folder = repository.getNodeAsRootUser(url);
		} catch (RepositoryException e) {
			e.printStackTrace();
		}
		if (folder == null)
			return null;

		String fileName = getFileName(url).concat(".zip");
		Collection<RepositoryItem> itemsToZip = new ArrayList<RepositoryItem>();
		try {
			addItemsOfFolder(folder, itemsToZip, user);
		} catch (RepositoryException e) {
			LOGGER.log(Level.WARNING, "Error adding items of " + folder + " to a ZIP file", e);
		}

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

	private void addItemsOfFolder(Node folder, Collection<RepositoryItem> itemsToZip, User user) throws IOException, RepositoryException {
		NodeIterator nodeIterator = folder.getNodes();
		if (nodeIterator == null)
			return;

		for (; nodeIterator.hasNext();) {
			Node node = nodeIterator.nextNode();

			if (node.hasProperty(JcrConstants.NT_FOLDER)) {
				addItemsOfFolder(node, itemsToZip, user);
			} else if (node.hasProperty(JcrConstants.NT_FILE)) {
				itemsToZip.add(new JackrabbitRepositoryItem(url, user));
			} else {
				LOGGER.warning("Node " + node + " is not a folder nor a file");
			}
		}
	}

	private RepositoryService getRepository() throws IBOLookupException {
		return ELUtil.getInstance().getBean(RepositoryService.class);
	}
}