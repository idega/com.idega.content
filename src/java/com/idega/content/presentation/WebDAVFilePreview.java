/*
 * Created on 14.12.2004
 */
package com.idega.content.presentation;

import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;

import javax.faces.context.FacesContext;

import com.idega.repository.bean.RepositoryItem;
import com.idega.util.CoreConstants;
import com.idega.webface.WFFrame;
import com.idega.webface.WFToolbar;

/**
 * A "preview" for a webdav resource, adds an iframe to the page or possible a plugin...
 * @author <a href="mailto:eiki@idega.is">Eirikur S. Hrafnsson</a>
 */
public class WebDAVFilePreview extends ContentBlock {

	public static final String DEFAULT_STYLE_CLASS = "content_file_preview";

	@Override
	protected void initializeComponent(FacesContext context) {
		RepositoryItem resource = getRepositoryItem();

		if (resource != null) {
			String resourceName = resource.getName();
			try {
				resourceName = URLEncoder.encode(resourceName, CoreConstants.ENCODING_UTF8);
			} catch (UnsupportedEncodingException e) {
			}

			String path = resource.getParentPath() + CoreConstants.SLASH + resourceName;
			if (!path.startsWith(CoreConstants.WEBDAV_SERVLET_URI))
				path = CoreConstants.WEBDAV_SERVLET_URI + path;

			WFFrame frame = new WFFrame(resourceName, path);
			frame.setStyleClass(DEFAULT_STYLE_CLASS);
			frame.setToolbar(new WFToolbar());
			this.getChildren().add(frame);
		}
	}
}