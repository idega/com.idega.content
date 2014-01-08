package com.idega.content.repository.bean;

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.List;

import org.apache.webdav.lib.WebdavResource;
import org.apache.webdav.lib.WebdavResources;

import com.idega.repository.bean.RepositoryItem;
import com.idega.util.CoreConstants;
import com.idega.util.StringUtil;

public class WebDAVItem implements RepositoryItem {

	private String name, folder;
	private WebdavResource resource;

	public WebDAVItem(WebdavResource resource) {
		this(resource, null);
	}

	public WebDAVItem(WebdavResource resource, String folder) {
		this.resource = resource;
		this.folder = folder;
	}

	@Override
	public InputStream getInputStream() throws IOException {
		return resource.getMethodData();
	}

	@Override
	public String getName() {
		if (StringUtil.isEmpty(name)) {
			String name = resource.getName();
			if (name == null) {
				name = "unknown";
			}

			if (name.endsWith(CoreConstants.SLASH)) {
				name = name.substring(0, name.lastIndexOf(CoreConstants.SLASH));
			}
			if (name.startsWith(CoreConstants.WEBDAV_SERVLET_URI)) {
				name = name.replace(CoreConstants.WEBDAV_SERVLET_URI, CoreConstants.EMPTY);
			}
			if (!StringUtil.isEmpty(folder) && name.startsWith(folder)) {
				name = name.replace(folder, CoreConstants.EMPTY);
			}

			this.name = name;
		}

		return this.name;
	}

	@Override
	public long getLength() {
		return resource.getGetContentLength();
	}

	@Override
	public boolean delete() throws IOException {
		return resource.deleteMethod();
	}

	@Override
	public String toString() {
		return getClass().getSimpleName().concat(": ").concat(getName());
	}

	@Override
	public boolean isDirectory() {
		return resource.getIsCollection();
	}

	private List<WebDAVItem> children = null;

	@Override
	public List<WebDAVItem> getChildren() {
		if (children != null) {
			return children;
		}

		children = new ArrayList<WebDAVItem>();

		try {
			WebdavResources resources = resource.getChildResources();
			if (resources != null) {
				@SuppressWarnings("unchecked")
				Enumeration<WebdavResource> webDAVChildren = resources.getResources();
				if (webDAVChildren != null) {
					while (webDAVChildren.hasMoreElements()) {
						WebdavResource child = webDAVChildren.nextElement();
						if (child != null) {
							children.add(new WebDAVItem(child));
						}
					}
				}
			}
		} catch (Exception e) {
			e.printStackTrace();
		}

		return children;
	}
}