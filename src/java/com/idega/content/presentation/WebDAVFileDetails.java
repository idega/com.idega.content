/*
 * Created on 14.12.2004
 */
package com.idega.content.presentation;

import java.util.Iterator;
import java.util.List;

import javax.faces.component.UIComponent;
import javax.faces.component.html.HtmlCommandButton;
import javax.faces.component.html.HtmlCommandLink;
import javax.faces.component.html.HtmlGraphicImage;
import javax.faces.component.html.HtmlOutputLink;
import javax.faces.event.AbortProcessingException;
import javax.faces.event.ActionEvent;
import javax.faces.event.ActionListener;

import org.apache.webdav.lib.WebdavResource;

import com.idega.idegaweb.IWMainApplication;
import com.idega.presentation.Table;
import com.idega.presentation.text.DownloadLink;
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
		toggleLock(resource);
		
		if (resource != null) {
			String resourceName = resource.getDisplayName();
			int row = 1;
			
			Table table = new Table();
			table.setId(this.getId() + "_table");
			table.setWidth("100%");
			
			table.mergeCells(1, row, 2, row);
			table.add(WFUtil.getText("Document details"));
				
			HtmlOutputLink link = new HtmlOutputLink();
			link.setValue(resource.getEncodedPath());
			link.setStyleClass("wf_listlink");
			link.setId(getId() + "_dl");
			link.getChildren().add(WFUtil.getText("Download/View"));
			
			table.add(WFUtil.getText("Document name"), 1, ++row);
			table.add(WFUtil.getText(resourceName,"wf_listtext"), 2, row);
			table.add(link, 2, ++row);
			table.add(WFUtil.getText("Size"), 1, ++row);
			table.add(WFUtil.getText(FileUtil.getHumanReadableSize(resource.getGetContentLength()),"wf_listtext"), 2, row);
			table.add(WFUtil.getText("Content type"), 1, ++row);
			table.add(WFUtil.getText(resource.getGetContentType(),"wf_listtext"), 2, row);
			
			table.add(WFUtil.getText("Creation date"), 1, ++row);
			table.add(WFUtil.getText(new IWTimestamp(resource.getCreationDate()).toString(),"wf_listtext"), 2, row);
			
			table.add(WFUtil.getText("Modification date"), 1, ++row);
			table.add(WFUtil.getText(new IWTimestamp(resource.getGetLastModified()).toString(),"wf_listtext"), 2, row);
			
			table.add(WFUtil.getText("Locked/Unlocked"), 1, ++row);
			if (resource.isLocked()) {
				HtmlGraphicImage lock = new HtmlGraphicImage();
				lock.setUrl(IWMainApplication.getDefaultIWMainApplication().getURIFromURL(WFUtil.getContentBundle().getResourcesVirtualPath())+"/images/locked.gif");
				lock.setId(this.getId()+"_lock");
				lock.setHeight("16");// sizes that make sense 16/32/64/128
				lock.setStyle("alignment:bottom");
				table.add(lock, 2, row);
				table.add(WFUtil.getText("Locked","wf_listtext"), 2, row);
			} else {
				table.add(WFUtil.getText("Unlocked","wf_listtext"), 2, row);
			}
			table.add(WFUtil.getText("  - "), 2, row);
			
			HtmlCommandButton lockToggler = new HtmlCommandButton();
			lockToggler.setId(getId()+"_lockTogg");
			lockToggler.getAttributes().put(PARAMETER_RESOURCE_PATH, resource.getPath());
			if (resource.isLocked()) {
				//lockToggler.getChildren().add( WFUtil.getText("Change to unlocked"));
				lockToggler.setValue("Unlock");
			} else {
				//lockToggler.getChildren().add( WFUtil.getText("Change to locked"));
				lockToggler.setValue("Lock");
			}
			
			lockToggler.setStyleClass("wf_listlink");
			lockToggler.setActionListener(WFUtil.createMethodBinding("#{contentviewerbean.processAction}", new Class[]{ActionEvent.class}));
			lockToggler.getAttributes().put(ACTION, ACTION_TOGGLE_LOCK);
			table.add(lockToggler, 2, row);


			table.add(WFUtil.getText("Checkout status"), 1, ++row);
			if (resource.getCheckedOut() == null) {
				table.add(WFUtil.getText("Not checked out","wf_listtext"), 2, row);
			} else {
				table.add(WFUtil.getText("Checked out ("+resource.getCheckedOut()+")","wf_listtext"), 2, row);
			}
			table.add(WFUtil.getText("  - "), 2, row);
			HtmlCommandLink checker = new HtmlCommandLink();
			checker.setId(getId()+"_check");
			checker.getAttributes().put(PARAMETER_RESOURCE_PATH, resource.getPath());
			if (resource.getCheckedOut() == null) {
				checker.getChildren().add( WFUtil.getText("Check out"));
				checker.getAttributes().put(ACTION, ACTION_CHECK_OUT);
			} else {
				// Checka if current user has file checked out, or not...
				checker.getChildren().add( WFUtil.getText("Change to Locked"));
			}
			checker.setStyleClass("wf_listlink");
			checker.setActionListener(WFUtil.createMethodBinding("#{contentviewerbean.processAction}", new Class[]{ActionEvent.class}));
			table.add(checker, 2, row);
			
			
//			++row;
//			table.add(WFUtil.getText("Http URL"), 1, row);
//			table.add(WFUtil.getText(resource.getHttpURL().toString(),"wf_listtext"), 2, row);
			
//			table.add(WFUtil.getText("Owner"), 1, ++row);
//			table.add(WFUtil.getText(resource.getOwner(),"wf_listtext"), 2, row);

//			table.add(WFUtil.getText("Etag"), 1, ++row);
//			table.add(WFUtil.getText(resource.getGetEtag(),"wf_listtext"), 2, row);

			//Then add the version table
			Table vTable = getVersionReportTable(resource);
			
			++row;
			table.mergeCells(1, row, 2, row);
			table.add(vTable, 1, row);
			this.getChildren().add(table);
		}
	}

	protected Table getVersionReportTable(WebdavResource resource) {
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
		vTable.add("Version", vColumn, vRow);
		vTable.add("Download", ++vColumn, vRow);
		vTable.add("User", ++vColumn, vRow);
		vTable.add("Comment", ++vColumn, vRow);
		vTable.add("Checkout", ++vColumn, vRow);
		vTable.add("Checkin", ++vColumn, vRow);
		vTable.add("Last modified", ++vColumn, vRow);

		
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
				versionPath.setAlternativeFileName("v"+versionName.replace('.','_')+"-"+resource.getDisplayName());
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
		if (ACTION_TOGGLE_LOCK.equals(action)) {
			if (res != null) {
				toggleLock(res);
			}
		} else if (ACTION_CHECK_OUT.equals(action)) {
			VersionHelper.checkOut(res);
			refreshList();
		} else if (ACTION_CHECK_IN.equals(action)) {
			VersionHelper.checkIn(res);
			refreshList();
		} else if (ACTION_UNCHECK_OUT.equals(action)) {
			VersionHelper.unCheckOut(res);
			refreshList();
		}
	}
}