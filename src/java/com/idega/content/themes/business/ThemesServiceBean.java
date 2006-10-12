package com.idega.content.themes.business;

import java.rmi.RemoteException;

import java.util.Iterator;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.slide.event.ContentEvent;

import com.idega.business.IBOLookupException;
import com.idega.business.IBOServiceBean;
import com.idega.content.themes.helpers.ThemeInfo;
import com.idega.content.themes.helpers.ThemesConstants;
import com.idega.content.themes.helpers.ThemesHelper;
import com.idega.core.builder.business.BuilderService;
import com.idega.core.builder.data.ICPage;
import com.idega.core.builder.data.ICPageHome;
import com.idega.core.file.data.ICFile;
import com.idega.core.file.data.ICFileHome;
import com.idega.slide.business.IWContentEvent;
import com.idega.slide.business.IWSlideChangeListener;

public class ThemesServiceBean extends IBOServiceBean implements ThemesService, IWSlideChangeListener{

	private static final long serialVersionUID = -1765120426660957585L;
	private static final Log log = LogFactory.getLog(ThemesServiceBean.class);
	
	private ThemesHelper helper = ThemesHelper.getInstance();

	public void onSlideChange(IWContentEvent idegaWebContentEvent) {
		String uri = idegaWebContentEvent.getContentEvent().getUri();
		if (uri.indexOf(ThemesConstants.THEMES_PATH) != -1) { // If proccesing theme
			if (ContentEvent.REMOVE.equals(idegaWebContentEvent.getMethod())) {
				if (helper.isCorrectFile(uri)) {
					Iterator <ThemeInfo> allThemes = helper.getThemesCollection().iterator();
					boolean foundTheme = false;
					ThemeInfo theme = null;
					while (allThemes.hasNext() && !foundTheme) {
						theme = allThemes.next();
						if (uri.equals(theme.getLinkToSkeleton())) {
							foundTheme = true;
						}
					}
					if (foundTheme && !theme.isLocked()) {
						String themeID = theme.getThemeId();
						helper.removeTheme(uri, themeID);
					}
				}
			}
			else {
				if (helper.isCorrectFile(uri) && isNewTheme(uri)) {
					helper.getThemesLoader().loadTheme(uri, true);
					//TODO: proceed creating IBPage
				}
			}
		}
	}
	
	private boolean isNewTheme(String uri) {
		if (helper.existTheme(uri)) {
			return false;
		}
		helper.addUriToTheme(uri);
		return true;
	}
	
	public ICPageHome getICPageHome() throws RemoteException {
		ICPageHome sHome = (ICPageHome) getIDOHome(ICPage.class);
		return sHome;
	}
	
	public ICFileHome getICFileHome() throws RemoteException {
		ICFileHome sHome = (ICFileHome) getIDOHome(ICFile.class);
		return sHome;
	}
	
	
	public BuilderService getBuilderService(){
		try {
			return (BuilderService) this.getServiceInstance(BuilderService.class);
		} catch (IBOLookupException e) {
			log.error(e);
		}
		return null;
		
	}
}