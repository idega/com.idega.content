package com.idega.content.themes.business;

import java.rmi.RemoteException;

import java.util.ArrayList;
import java.util.List;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.apache.slide.event.ContentEvent;

import com.idega.business.IBOLookupException;
import com.idega.business.IBOServiceBean;
import com.idega.content.themes.helpers.Theme;
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

	public void onSlideChange(IWContentEvent idegaWebContentEvent) {
		String uri = idegaWebContentEvent.getContentEvent().getUri();
		if (uri.indexOf(ThemesConstants.THEMES_PATH) == -1) { // If not proccesing theme
			return;
		}
		if (ContentEvent.REMOVE.equals(idegaWebContentEvent.getMethod())) {
			if (ThemesHelper.getInstance().isCorrectFile(uri)) {
				List <Theme> themes = new ArrayList<Theme>(ThemesHelper.getInstance().getThemesCollection());
				boolean foundTheme = false;
				Theme theme = null;
				for (int i = 0; (i < themes.size() && !foundTheme); i++) {
					theme = themes.get(i);
					if (uri.equals(ThemesHelper.getInstance().decodeUrl(theme.getLinkToSkeleton()))) {
						foundTheme = true;
					}
				}
				if (foundTheme && !theme.isLocked()) {
					String themeID = theme.getThemeId();
					ThemesHelper.getInstance().removeTheme(uri, themeID);
				}
			}
		}
		else {
			if (ThemesHelper.getInstance().isCorrectFile(uri) && isNewTheme(uri) && !ThemesHelper.getInstance().isCreatedManually(uri)) {
				ThemesHelper.getInstance().getThemesLoader().loadTheme(uri, ThemesHelper.getInstance().urlEncode(uri), true);
				//TODO: proceed creating IBPage
			}
		}
	}
	
	private boolean isNewTheme(String uri) {
		if (ThemesHelper.getInstance().existTheme(uri)) {
			return false;
		}
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