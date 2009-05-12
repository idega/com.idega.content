package com.idega.content.themes.business;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationEvent;
import org.springframework.context.ApplicationListener;
import org.springframework.context.annotation.Scope;
import org.springframework.stereotype.Service;

import com.idega.idegaweb.IWMainSlideStartedEvent;

/**
 * Installs basic themes on Slide startup
 * @author valdas
 *
 */

@Scope("singleton")
@Service
public class ThemesInstaller implements ApplicationListener {

	private ThemesEngine themesEngine = null;
	
	private boolean processHasStarted = false;
	
	public void onApplicationEvent(ApplicationEvent event) {
		if (!(event instanceof IWMainSlideStartedEvent)) {
			return;
		}
		
		if (processHasStarted) {
			return;
		}
		processHasStarted = true;

		processHasStarted = installOrActivateThemes((IWMainSlideStartedEvent) event);
	}
	
	private synchronized boolean installOrActivateThemes(IWMainSlideStartedEvent event) {
//		IWMainApplication iwma = event.getIWMA();
//		Thread themesInstaller = new Thread(new ThemesInstallerRunner(getThemesEngine(), iwma));
//		themesInstaller.start();
		return true;
	}
	
	/*private class ThemesInstallerRunner implements Runnable {
		private IWMainApplication iwma = null;
		private ThemesEngine themesEngine = null;
		
		private ThemesInstallerRunner(ThemesEngine themesEngine, IWMainApplication iwma) {
			this.themesEngine = themesEngine;
			this.iwma = iwma;
		}
		
		public void run() {
			activateThemes();	//	TODO: for now not auto installing themes
			
			/*IWMainApplicationSettings settings = iwma.getSettings();
			if (settings == null) {
				activateThemes();
				return;
			}
			
			boolean themesAlreadyInstalled = settings.getBoolean(ContentConstants.BASIC_THEMES_ADDED_PROPERTY, false);
			if (themesAlreadyInstalled) {
				activateThemes();
				return;
			}
			
			ThemesHelper helper = ThemesHelper.getInstance(true);
			helper.searchForThemes();
			Collection<Theme> allThemes = helper.getAllThemes();
			if (allThemes != null && !allThemes.isEmpty()) {
				//	There some themes already - not importing basic themes
				activateThemes();
				return;
			}
		
			WebDAVUploadBean uploadBean = null;
			try {
				uploadBean = (WebDAVUploadBean) WFUtil.getBeanInstance(WebDAVUpload.BEAN_ID);
			} catch(Exception e) {
				uploadBean = new WebDAVUploadBean();
			}

			IWSlideService slide = helper.getSlideService(iwma.getIWApplicationContext());
			if (slide == null) {
				return;
			}
			
			//	TODO: get file from URL
			IWBundle bundle = iwma.getBundle(ContentConstants.IW_BUNDLE_IDENTIFIER);
			File themesPackage = IWBundleResourceFilter.copyResourceFromJarToWebapp(iwma, bundle.getVirtualPathWithFileNameString("themes/basic_themes.zip"));
			if (themesPackage == null) {
				return;
			}
			
			InputStream stream = null;
			try {
				stream = new BufferedInputStream(new FileInputStream(themesPackage));
			} catch(Exception e) {
				e.printStackTrace();
			}
			if (stream == null) {
				return;
			}
			
			uploadBean.setUploadFilePath(ThemesConstants.THEMES_PATH);
			try {
				boolean selfInstallingResult = uploadBean.uploadZipFile(true, "Basic_themes", stream, slide);
				if (selfInstallingResult) {
					selfInstallingResult = activateThemes();
				}
				settings.setProperty(ContentConstants.BASIC_THEMES_ADDED_PROPERTY, String.valueOf(selfInstallingResult));
			} catch(Exception e) {
				e.printStackTrace();
			} finally {
				helper.closeInputStream(stream);
			}
		}
		
		private boolean activateThemes() {
			if (themesEngine == null) {
				return false;
			}
			
			return themesEngine.getThemes() == null ? false : true;
		}
	}*/

	public ThemesEngine getThemesEngine() {
		return themesEngine;
	}

	@Autowired
	public void setThemesEngine(ThemesEngine themesEngine) {
		this.themesEngine = themesEngine;
	}

}
