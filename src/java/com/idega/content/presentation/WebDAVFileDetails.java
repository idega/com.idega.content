/*
 * Created on 14.12.2004
 */
package com.idega.content.presentation;

import java.io.IOException;
import java.util.Iterator;
import java.util.List;
import javax.faces.component.UIComponent;
import javax.faces.component.html.HtmlCommandButton;
import javax.faces.component.html.HtmlGraphicImage;
import javax.faces.component.html.HtmlOutputLink;
import javax.faces.component.html.HtmlOutputText;
import javax.faces.event.AbortProcessingException;
import javax.faces.event.ActionEvent;
import javax.faces.event.ActionListener;
import com.idega.business.IBOLookup;
import com.idega.idegaweb.IWMainApplication;
import com.idega.presentation.IWContext;
import com.idega.presentation.Table;
import com.idega.presentation.text.DownloadLink;
import com.idega.slide.business.IWSlideSession;
import com.idega.slide.util.VersionHelper;
import com.idega.slide.util.WebdavExtendedResource;
import com.idega.slide.util.WebdavResourceVersion;
import com.idega.util.FileUtil;
import com.idega.util.IWTimestamp;
import com.idega.util.Timer;
import com.idega.webface.WFUtil;

/**
 * @author gimmi
 */
public class WebDAVFileDetails extends ContentBlock implements ActionListener {

	private static String ACTION = "wdf_ac";
	private static String ACTION_TOGGLE_LOCK = "togLock";
	private static String ACTION_CHECK_OUT = "checkout";
	private static String ACTION_CHECK_IN = "checkin";
	private static String ACTION_UNCHECK_OUT = "uncheckout";
	
	
	private static final String PARAMETER_RESOURCE_PATH = "wfd_prp";
	
	protected void initializeContent() {
		
		WebdavExtendedResource resource = getWebdavExtendedResource();
		String userName = null;
		
		if (resource != null) {
			try {
				// Making sure all properties are set
				resource.setProperties();
				userName = getIWSlideSession().getUserFullName();
			}
			catch (IOException e) {
				e.printStackTrace();
			}

			String resourceName = resource.getDisplayName();
			int row = 1;
			
			Table table = new Table();
			table.setId(this.getId() + "_table");
//			table.setWidth("100%");
//			table.setBorder(1);
			
			table.mergeCells(1, row, 2, row);
			HtmlOutputText docDetails = getBundle().getLocalizedText("document_details");
			docDetails.setStyleClass("wf_text");
			table.add(docDetails);
				
			HtmlOutputLink link = new HtmlOutputLink();
			link.setValue(resource.getEncodedPath());
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
			
			table.add(getText("locked_unlocked"), 1, ++row);
			if (resource.isLocked()) {
				HtmlGraphicImage lock = new HtmlGraphicImage();
				lock.setUrl(IWMainApplication.getDefaultIWMainApplication().getURIFromURL(WFUtil.getContentBundle().getResourcesVirtualPath())+"/images/locked.gif");
				lock.setId(this.getId()+"_lock");
				lock.setHeight("16");// sizes that make sense 16/32/64/128
				lock.setStyle("alignment:bottom");
				table.add(lock, 2, row);
				table.add(getText("locked", "wf_listtext"), 2, row);
			} else {
				table.add(getText("unlocked", "wf_listtext"), 2, row);
			}
			table.add(WFUtil.getText("  - "), 2, row);
			
			HtmlCommandButton lockToggler = new HtmlCommandButton();
			lockToggler.setId(getId()+"_lockTogg");
			lockToggler.getAttributes().put(PARAMETER_RESOURCE_PATH, resource.getPath());
			if (resource.isLocked()) {
				getBundle().getLocalizedUIComponent("unlock", lockToggler);
//				lockToggler.setValue("Unlock");
			} else {
				getBundle().getLocalizedUIComponent("lock", lockToggler);
//				lockToggler.setValue("Lock");
			}
			lockToggler.setStyleClass("wf_listlink");
			lockToggler.setActionListener(WFUtil.createMethodBinding("#{contentviewerbean.processAction}", new Class[]{ActionEvent.class}));
			lockToggler.getAttributes().put(ACTION, ACTION_TOGGLE_LOCK);
			table.add(lockToggler, 2, row);


			table.add(getText("checkout_status"), 1, ++row);
			String checkedOut = resource.getCheckedOut();
			if (checkedOut == null || "".equals(checkedOut)) {
				table.add(getText("not_checked_out", "wf_listtext"), 2, row);
			} else {
				table.add(getText("checked_out", "wf_listtext"), 2, row);
				table.add(WFUtil.getText(" ("+checkedOut.substring(checkedOut.lastIndexOf("/")+1)+")","wf_listtext"), 2, row);
			}
			table.add(WFUtil.getText("  - "), 2, row);
			
			HtmlCommandButton checker = new HtmlCommandButton();
			checker.setId(getId()+"_check");
			checker.getAttributes().put(PARAMETER_RESOURCE_PATH, resource.getPath());
			if (checkedOut == null || "".equals(checkedOut)) {
				getBundle().getLocalizedUIComponent("check_out", checker);
//				checker.setValue("Check out");
				checker.getAttributes().put(ACTION, ACTION_CHECK_OUT);
			} else if (VersionHelper.hasUserCheckedOutResource(resource, userName)) {
				// Checka if current user has file checked out, or not...
//				getBundle().getLocalizedUIComponent("check_out", checker);
				getBundle().getLocalizedUIComponent("uncheck_out", checker);
//			checker.setValue("Check out");
			checker.getAttributes().put(ACTION, ACTION_UNCHECK_OUT);
			} else {
				checker.setValue("Check in/uncheck something...");
			}
			checker.setStyleClass("wf_listlink");
			checker.setActionListener(WFUtil.createMethodBinding("#{contentviewerbean.processAction}", new Class[]{ActionEvent.class}));
			table.add(checker, 2, row);
			
			
			//Then add the version table
			Table vTable = getVersionReportTable(resource);
			
			++row;
			table.mergeCells(1, row, 2, row);
			table.add(vTable, 1, row);
			this.getChildren().add(table);
		}
	}

	protected Table getVersionReportTable(WebdavExtendedResource resource) {
		Timer timer = new Timer();
		timer.start();
		List versions = VersionHelper.getAllVersions(resource);
		Table vTable = new Table(8,versions.size()+1);
		vTable.setId(vTable.getId() + "_ver");
		//setListStyleClass("wf_list");
		vTable.setRowStyleClass(1,"wf_listheading");
		vTable.setStyleClass("wf_listtable");
		//vTable.setHeaderClass("wf_listheading");
		
		int vRow = 1;
		int vColumn = 1;
		vTable.add(getBundle().getLocalizedText("version"), vColumn, vRow);
		vTable.add(getBundle().getLocalizedText("download"), ++vColumn, vRow);
		vTable.add(getBundle().getLocalizedText("user"), ++vColumn, vRow);
		vTable.add(getBundle().getLocalizedText("comment"), ++vColumn, vRow);
		vTable.add(getBundle().getLocalizedText("checkout"), ++vColumn, vRow);
		vTable.add(getBundle().getLocalizedText("checkin"), ++vColumn, vRow);
		vTable.add(getBundle().getLocalizedText("last_modified"), ++vColumn, vRow);

		
		if (!versions.isEmpty()) {
			Iterator iter = versions.iterator();
			while (iter.hasNext()) {
				vColumn = 0;
				WebdavResourceVersion version = (WebdavResourceVersion) iter.next();
				++vRow;
			
				if(vRow%2==0){
					vTable.setRowStyleClass(vRow,"wf_listevenrow");
				}
				else{
					vTable.setRowStyleClass(vRow,"wf_listoddrow");
				}
				
				
				//vTable.add(resourceName, 1, vRow);
				String versionName = version.getVersionName();
								
				vTable.add(WFUtil.getText(versionName,"wf_listtext"), ++vColumn, vRow);
				DownloadLink versionPath = new DownloadLink("Download");
				
				versionPath.setId("dl_"+vRow);
				versionPath.setStyleClass("wf_listlink");
				String url = version.getURL();
				versionPath.setRelativeFilePath(url);
				//so we have a sensable name for the file!
				String fileName = "v"+versionName.replace('.','_')+"-"+resource.getDisplayName();
				versionPath.setAlternativeFileName(fileName);
				//versionPath.getChildren().add(WFUtil.getText("Download/View"));
				vTable.add(versionPath, ++vColumn, vRow);
				vTable.add(WFUtil.getText(version.getCreatorDisplayName(),"wf_listtext"), ++vColumn, vRow);
				vTable.add(WFUtil.getText(version.getComment(),"wf_listtext"), ++vColumn, vRow);
				vTable.add(WFUtil.getText(version.getCheckedOut(),"wf_listtext"), ++vColumn, vRow);
				vTable.add(WFUtil.getText(version.getCheckedIn(),"wf_listtext"), ++vColumn, vRow);
				vTable.add(WFUtil.getText(version.getLastModified(),"wf_listtext"), ++vColumn, vRow);
			}
		}
		
		++vRow;
		vTable.add("Creation time", 3, vRow);
		vTable.add(timer.getTimeString(), 4, vRow);
		
		timer.stop();
		
		return vTable;
	}

	private void toggleLock(WebdavExtendedResource resource) {
		if (resource.isLocked()) {
			VersionHelper.unlock(resource);
		} else {
			VersionHelper.lock(resource);
		}
		super.refreshList();
	}
	
	
	public void processAction(ActionEvent event) throws AbortProcessingException {
		UIComponent comp = (UIComponent) event.getSource();
		String action = (String) comp.getAttributes().get(ACTION);
		String path = (String) comp.getAttributes().get(PARAMETER_RESOURCE_PATH);
		WebdavExtendedResource res = getWebdavExentededResource(path);
		try {
			if (ACTION_TOGGLE_LOCK.equals(action)) {
				if (res != null) {
					toggleLock(res);
				}
			} else if (ACTION_CHECK_OUT.equals(action)) {
				IWSlideSession ss = (IWSlideSession) IBOLookup.getSessionInstance(IWContext.getInstance(), IWSlideSession.class);
				VersionHelper.checkOut(res, ss.getUserFullName());
				refreshList();
			} else if (ACTION_CHECK_IN.equals(action)) {
				VersionHelper.checkIn(res);
				refreshList();
			} else if (ACTION_UNCHECK_OUT.equals(action)) {
				VersionHelper.unCheckOut(res);
				refreshList();
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
}