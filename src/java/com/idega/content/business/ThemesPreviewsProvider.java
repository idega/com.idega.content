package com.idega.content.business;

import java.util.List;

import com.idega.business.IBOService;
import com.idega.slide.business.IWSlideService;
import com.idega.graphics.PreviewGenerator;

public interface ThemesPreviewsProvider extends IBOService {

	/**
	 * @see com.idega.content.business.ThemesPreviewsProviderBean#getJpegImgType()
	 */
	public String getJpegImgType();
	
	/**
	 * @see com.idega.content.business.ThemesPreviewsProviderBean#getPngImgType()
	 */
	public String getPngImgType();

	/**
	 * @see com.idega.content.business.ThemesPreviewsProviderBean#getResourcePath()
	 */
	public String getResourcePath();

	/**
	 * @see com.idega.content.business.ThemesPreviewsProviderBean#getSlideService()
	 */
	public IWSlideService getSlideService();

	/**
	 * @see com.idega.content.business.ThemesPreviewsProviderBean#getThemesPath()
	 */
	public String getThemesPath();

	/**
	 * @see com.idega.content.business.ThemesPreviewsProviderBean#getThemesPreviewPath()
	 */
	public String getThemesPreviewPath();

	/**
	 * @see com.idega.content.business.ThemesPreviewsProviderBean#getThemesPreviewsInfo()
	 */
	public String getThemesPreviewsInfo();

	/**
	 * @see com.idega.content.business.ThemesPreviewsProviderBean#getFileName(java.lang.String)
	 */
	public String getFileName(String uri);

	/**
	 * @see com.idega.content.business.ThemesPreviewsProviderBean#getFileType(java.lang.String)
	 */
	public String getFileType(String uri);

	/**
	 * @see com.idega.content.business.ThemesPreviewsProviderBean#getFiles(java.lang.String)
	 */
	public List getFiles(String folderURI);
	
	/**
	 * @see com.idega.content.business.ThemesPreviewsProviderBean#getWebRootWithoutContent(java.lang.String)
	 */
	public String getWebRootWithoutContent(String webRoot);
	
	/**
	 * @see com.idega.content.business.ThemesPreviewsProviderBean#getFullWebRoot()
	 */
	public String getFullWebRoot();
	
	/**
	 * @see com.idega.content.business.ThemesPreviewsProviderBean#getPagePreview()
	 */
	public String getPagePreview();
	
	/**
	 * @see com.idega.content.business.ThemesPreviewsProviderBean#getPreviewGenerator()
	 */
	public PreviewGenerator getPreviewGenerator();
	
}