/*
 * Created on 14.12.2004
 */
package com.idega.content.presentation;

import java.util.Iterator;
import java.util.List;
import javax.faces.component.html.HtmlOutputLink;
import org.apache.webdav.lib.WebdavResource;
import com.idega.presentation.Table;
import com.idega.presentation.text.DownloadLink;
import com.idega.slide.util.VersionHelper;
import com.idega.slide.util.WebdavResourceVersion;
import com.idega.util.Timer;
import com.idega.webface.WFUtil;

/**
 * @author gimmi
 */
public class WebDAVFileDetails extends ContentBlock {

	protected void initializeContent() {
		this.setId(this.getId());
		WebdavResource resource = getWebdavResource();
		Table table = new Table();
		table.setId(this.getId() + "_table");
		table.setWidth("100%");
		int row = 1;
		table.mergeCells(1, row, 2, row);
		table.add(WFUtil.getText("Document details"));
		if (resource != null) {
			String resourceName = resource.getName();
			HtmlOutputLink link = new HtmlOutputLink();
			link.setValue(resource.getPath());
			link.setId(getId() + "_dl");
			link.getChildren().add(WFUtil.getText("Download"));
			++row;
			table.add(WFUtil.getText(resourceName), 1, row);
			table.add(link, 2, row);
			Timer timer = new Timer();
			timer.start();
			
			
			List versions = VersionHelper.getAllVersions(resource);
			Table vTable = new Table(8,versions.size()+1);
			vTable.setId(table.getId() + "_ver");
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
					vTable.add(versionName, ++vColumn, vRow);
					DownloadLink versionPath = new DownloadLink("Download/View");
					versionPath.setId("dl_"+vRow);
					String url = version.getURL();
					versionPath.setRelativeFilePath(url);
					//so we have a sensable name for the file!
					versionPath.setAlternativeFileName("v"+versionName.replace('.','_')+"-"+resourceName);
					//versionPath.getChildren().add(WFUtil.getText("Download/View"));
					vTable.add(versionPath, ++vColumn, vRow);
					vTable.add(version.getCreatorDisplayName(), ++vColumn, vRow);
					vTable.add(version.getComment(), ++vColumn, vRow);
					vTable.add(version.getCheckedOut(), ++vColumn, vRow);
					vTable.add(version.getCheckedIn(), ++vColumn, vRow);
					vTable.add(version.getLastModified(), ++vColumn, vRow);
				}
			}
			timer.stop();
			++vRow;
			vTable.add("Creation time", 3, vRow);
			vTable.add(timer.getTimeString(), 4, vRow);
			++row;
			table.mergeCells(1, row, 2, row);
			table.add(vTable, 1, row);
			this.getChildren().add(table);
		}
	}
}