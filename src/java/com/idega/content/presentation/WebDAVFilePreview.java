/*
 * Created on 14.12.2004
 */
package com.idega.content.presentation;

import javax.faces.component.html.HtmlOutputLink;
import com.idega.presentation.Table;
import com.idega.slide.util.WebdavExtendedResource;
import com.idega.util.FileUtil;
import com.idega.util.IWTimestamp;
import com.idega.webface.WFFrame;
import com.idega.webface.WFUtil;

/**
 * A "preview" for a webdav resource, adds an iframe to the page or possible a plugin...
 * @author <a href="mailto:eiki@idega.is">Eirikur S. Hrafnsson</a>
 */
public class WebDAVFilePreview extends ContentBlock {

	protected void initializeContent() {
		WebdavExtendedResource resource = getWebdavExtendedResource();
		
		String filePath = resource.getPath();
		if (resource != null) {
			String resourceName = resource.getDisplayName();
			int row = 1;
			
			Table table = new Table();
			table.setId(this.getId() + "_table");
			table.setWidth("100%");
						
			HtmlOutputLink link = new HtmlOutputLink();
			link.setValue(resource.getPath());
			link.setStyleClass("wf_listlink");
			link.setId(getId() + "_dl");
			link.getChildren().add(getBundle().getLocalizedText("download_view"));
			
			table.add(getText("document_name"), 1, ++row);
			table.add(WFUtil.getText(resourceName,"wf_listtext"), 2, row);
			table.add(link, 2, ++row);
			table.add(getText("size"), 1, ++row);
			table.add(WFUtil.getText(FileUtil.getHumanReadableSize(resource.getGetContentLength()),"wf_listtext"), 2, row);
			table.add(getText("content_type"), 1, ++row);
			table.add(WFUtil.getText(resource.getGetContentType(),"wf_listtext"), 2, row);
			
			table.add(getText("creation_date"), 1, ++row);
			table.add(WFUtil.getText(new IWTimestamp(resource.getCreationDate()).toString(),"wf_listtext"), 2, row);
			
			table.add(getText("modification_date"), 1, ++row);
			table.add(WFUtil.getText(new IWTimestamp(resource.getGetLastModified()).toString(),"wf_listtext"), 2, row);
			
			this.getChildren().add(table);
			
			//this.getChildren().add(new HorizontalRule());
			
			WFFrame frame = new WFFrame(resourceName,filePath);
			frame.setToolbarEmbeddedInTitlebar(false);
			this.getChildren().add(frame);
			
//			String mimeType = resource.getGetContentType();
//			MimeTypeUtil util = MimeTypeUtil.getInstance();
//			
//			if( (mimeType==null) || util.isFolder(mimeType)){
//				//
//			}
//			else if(util.isWord(mimeType)){
//				//use extractor
//			}
//			else if(util.isExcel(mimeType)){
//				//use extractor
//			}
//			else if(util.isPowerPoint(mimeType)){
//				//use extractor
//			}
//			else if(util.isPDF(mimeType)){
//				//use extractor
//			}
//			else if(util.isImage(mimeType)){
//				HtmlGraphicImage image = new HtmlGraphicImage();
//				//todo fuck this shit up
//				image.setUrl(IWMainApplication.getDefaultIWMainApplication().getURIFromURL(filePath));
//				this.getChildren().add(image);
//			}
//			else if(util.isDocument(mimeType)){
//				//iconURLorURIWithFileName =  (withContext)?getIconURLForIconFileName(DEFAULT_ICON_FILE_NAME_DOCUMENT):getIconURIForIconFileName(DEFAULT_ICON_FILE_NAME_DOCUMENT);
//			}
//			else if(util.isCompressed(mimeType)){
//				//iconURLorURIWithFileName =  (withContext)?getIconURLForIconFileName(DEFAULT_ICON_FILE_NAME_COMPRESSED):getIconURIForIconFileName(DEFAULT_ICON_FILE_NAME_COMPRESSED);
//			}
//			else if(util.isAudio(mimeType)){
//				//iconURLorURIWithFileName =  (withContext)?getIconURLForIconFileName(DEFAULT_ICON_FILE_NAME_AUDIO):getIconURIForIconFileName(DEFAULT_ICON_FILE_NAME_AUDIO);
//			}
//			else if(util.isVector(mimeType)){
//				//iconURLorURIWithFileName =  (withContext)?getIconURLForIconFileName(DEFAULT_ICON_FILE_NAME_VECTOR):getIconURIForIconFileName(DEFAULT_ICON_FILE_NAME_VECTOR);
//			}
//			else if(util.isVideo(mimeType)){
//				//iconURLorURIWithFileName =  (withContext)?getIconURLForIconFileName(DEFAULT_ICON_FILE_NAME_VIDEO):getIconURIForIconFileName(DEFAULT_ICON_FILE_NAME_VIDEO);
//			}
//			else{
//				//everything else is a binary
//				//(util.isBinary(mimeType)){
//				//iconURLorURIWithFileName =  (withContext)?getIconURLForIconFileName(DEFAULT_ICON_FILE_NAME_BINARY):getIconURIForIconFileName(DEFAULT_ICON_FILE_NAME_BINARY);
//				WFFrame frame = new WFFrame("Preview",filePath);
//				this.getChildren().add(frame);
//			}
		}
	}
}