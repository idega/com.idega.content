/*
 * Created on 14.12.2004
 */
package com.idega.content.presentation;

import java.util.Enumeration;

import javax.faces.component.html.HtmlOutputLink;

import org.apache.webdav.lib.BaseProperty;
import org.apache.webdav.lib.ResponseEntity;
import org.apache.webdav.lib.WebdavResource;

import com.idega.presentation.Table;
import com.idega.slide.util.VersionHelper;
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
		table.setId(this.getId()+"-table");
		int row = 1;
		table.mergeCells(1, row, 2, row);
		table.add(WFUtil.getText("Document details"));
		
		if (resource != null) {
			HtmlOutputLink link = new HtmlOutputLink();
			link.setValue(resource.getPath());
			link.getChildren().add(WFUtil.getText("Download/View"));

			++row;
			table.add(WFUtil.getText(resource.getName()), 1, row);
			table.add(link, 2, row);
			
			
			Timer timer = new Timer();
			timer.start();
			
			Table vTable = new Table();
			int vRow = 1;
			
			Enumeration enumer = VersionHelper.getAllVersions(resource);
			while (enumer.hasMoreElements()) {
				ResponseEntity element = (ResponseEntity) enumer.nextElement();
				Enumeration props = element.getProperties();
				++vRow;
				while (props.hasMoreElements()) {
					BaseProperty prop = (BaseProperty) props.nextElement();
					String propName = prop.getLocalName();
					int colToAdd = 1;
					String textToAdd = "";
					if (propName.equals(VersionHelper.PROPERTY_VERSION_NAME)) { // Version number
						colToAdd = 2;
						textToAdd = prop.getPropertyAsString();
					} else if (propName.equals(VersionHelper.PROPERTY_CREATOR_DISPLAYNAME)) { 
						colToAdd = 3;
						textToAdd = prop.getPropertyAsString();
						if (textToAdd.equals("unauthenticated")) {
							textToAdd = "NONE";
						}
					} else if (propName.equals(VersionHelper.PROPERTY_VERSION_COMMENT)) {
						colToAdd = 4;
						textToAdd = prop.getPropertyAsString();
					}
					vTable.add(textToAdd, colToAdd, vRow);
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
	

	
	
//	WebdavResources rs = VersionHelper.getAllVersions(resource);
//	Enumeration rsEnum = rs.getResources();
//
//
//	while (rsEnum.hasMoreElements()) {
//		WebdavResource enumR = (WebdavResource) rsEnum.nextElement();
//		
//		versionGrid.getChildren().add(WFUtil.getText(enumR.getDisplayName()));
//		versionGrid.getChildren().add(WFUtil.getText(enumR.getName()));
//	}


}
