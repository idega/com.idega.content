package com.idega.content.business;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.InputStream;

import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.context.annotation.Scope;
import org.springframework.stereotype.Repository;

import com.idega.core.business.DefaultSpringBean;
import com.idega.core.file.util.MimeTypeUtil;
import com.idega.graphics.image.business.ImageResizer;
import com.idega.util.CoreConstants;
import com.idega.util.StringUtil;
import com.idega.util.expression.ELUtil;

@Repository(ThumbnailService.BEAN_NAME)
@Scope(BeanDefinition.SCOPE_SINGLETON)
public class ThumbnailService extends DefaultSpringBean {

	public static final String BEAN_NAME = "thumbnailService";

	public static final int THUMBNAIL_SMALL = 5;
	public static final int THUMBNAIL_MEDIUM = 10;

	private static final String THUMBNAILS_FOLDER_NAME = "idega_thumbnails";

	private int getSize(int size){
		if ((size > 0) && (size <= THUMBNAIL_MEDIUM))
			return size;

		return THUMBNAIL_MEDIUM;
	}

	private int[] get2dDimensions(int thumbnailSize){
		int size = thumbnailSize * 10;
		int[] dimensions = {size,size};
		return dimensions;
	}

	/**
	 * Method that gets thumbnail uri if it exists or creates if it does not exists.
	 * @param filePath - path to file which thumbnail will be displayed
	 * @param thumbnailSize - size identifier for thumbnail
	 * @param iwc - {@link com.idega.presentation.IWContext}
	 * @return uri to thunbnail or empty string if file does not exists
	 * @throws Exception if something goes wrong
	 */
	public String getThumbnail(String filePath, int thumbnailSize) throws Exception {
		if (StringUtil.isEmpty(filePath))
			return CoreConstants.EMPTY;

		thumbnailSize = getSize(thumbnailSize);
		String mimeType = MimeTypeUtil.resolveMimeTypeFromFileName(filePath);
		if (StringUtil.isEmpty(mimeType))
			return getUnknownThumbnailUri(thumbnailSize);

		if (mimeType.toLowerCase().contains("image")) {
			String thumbnailPath = getImageThumbnailUri(filePath, thumbnailSize, mimeType);
			if (!thumbnailPath.startsWith(CoreConstants.SLASH)) {
				thumbnailPath = CoreConstants.SLASH + thumbnailPath;
			}
			if (!thumbnailPath.startsWith(CoreConstants.WEBDAV_SERVLET_URI)) {
				thumbnailPath = CoreConstants.WEBDAV_SERVLET_URI + thumbnailPath;
			}
			return thumbnailPath;
		}

		return getUnknownThumbnailUri(thumbnailSize);
	}

	private String getUnknownThumbnailUri(int thumbnailSize) {
		//TODO: not implemented
		return CoreConstants.EMPTY;
	}

	private String getImageThumbnailUri(String filePath,int thumbnailSize,String mimeType) throws Exception{
		String thumbnailPath = getThumBnailPath(filePath, thumbnailSize);
		boolean exists = getRepositoryService().getExistence(thumbnailPath);
		if (exists)
			return thumbnailPath;

		return generateImageThumbnail(filePath, thumbnailPath, thumbnailSize, mimeType);
	}

	private String generateImageThumbnail(String filePath, String thumbnailPath, int thumbnailSize, String mimeType) throws Exception {
		int[] dimensions = get2dDimensions(thumbnailSize);
		ByteArrayOutputStream byteArrayOutputStream = new ByteArrayOutputStream();
		InputStream input = getRepositoryService().getInputStreamAsRoot(filePath);
		byteArrayOutputStream = (ByteArrayOutputStream) ELUtil.getInstance().getBean(ImageResizer.class)
				.getScaledImage(dimensions[0], dimensions[1], input, getImageType(mimeType), byteArrayOutputStream);
		InputStream image = new ByteArrayInputStream(byteArrayOutputStream.toByteArray());
		getRepositoryService().uploadFile(thumbnailPath.substring(0, thumbnailPath.lastIndexOf(CoreConstants.SLASH) + 1), getFileName(thumbnailPath),
				mimeType, image);
		return thumbnailPath;
	}

	private String getImageType(String mimeType){
		return mimeType.substring(mimeType.indexOf(CoreConstants.SLASH)+1, mimeType.length() );
	}
	private String getThumbnailsFolder(String filePath){
		return filePath.substring(0,filePath.lastIndexOf(CoreConstants.SLASH)) + CoreConstants.SLASH + THUMBNAILS_FOLDER_NAME;
	}

	private String getThumBnailPath(String filePath,int thumbnailSize){
		return new StringBuilder(getThumbnailsFolder(filePath)).append(CoreConstants.SLASH)
				.append(thumbnailSize).append(CoreConstants.UNDER).append(getFileName(filePath)).toString();
	}
	private String getFileName(String filePath){
		if(!filePath.contains(CoreConstants.SLASH)){
			return filePath;
		}
		return filePath.substring(filePath.lastIndexOf(CoreConstants.SLASH) + 1, filePath.length());
	}
}
