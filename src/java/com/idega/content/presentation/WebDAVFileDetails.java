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
		table.setId(this.getId()+"_table");
		table.setBorder(1);
		table.setBorderColor("Blue");
		int row = 1;
		table.mergeCells(1, row, 2, row);
		table.add(WFUtil.getText("Document details"));
		
		if (resource != null) {
			String resourceName = resource.getName(); 

			HtmlOutputLink link = new HtmlOutputLink();
			link.setValue(resource.getPath());
			link.setId(getId()+"_dl");
			link.getChildren().add(WFUtil.getText("Download/View"));

			++row;
			table.add(WFUtil.getText(resourceName), 1, row);
			table.add(link, 2, row);
			
			
			Timer timer = new Timer();
			timer.start();
			
			Table vTable = new Table();
			vTable.setId(table.getId()+"_ver");
			vTable.setBorder(1);
			vTable.setBorderColor("Red");
			int vRow = 1;
			
			vTable.add("Version", 2, vRow);
			vTable.add("User", 3, vRow);
			vTable.add("Comment", 4, vRow);
			
			Enumeration enumer = VersionHelper.getAllVersions(resource);
			while (enumer.hasMoreElements()) {
				ResponseEntity element = (ResponseEntity) enumer.nextElement();
				Enumeration props = element.getProperties();
				++vRow;
				vTable.add(resourceName, 1, vRow);
				while (props.hasMoreElements()) {
					BaseProperty prop = (BaseProperty) props.nextElement();
					String propName = prop.getLocalName();
					int colToAdd = 1;
					if (propName.equals(VersionHelper.PROPERTY_VERSION_NAME)) { // Version number
						colToAdd = 2;
						vTable.setAlignment(colToAdd, vRow, Table.HORIZONTAL_ALIGN_CENTER);
					} else if (propName.equals(VersionHelper.PROPERTY_CREATOR_DISPLAYNAME)) { 
						colToAdd = 3;
					} else if (propName.equals(VersionHelper.PROPERTY_VERSION_COMMENT)) {
						colToAdd = 4;
					}
					vTable.add(prop.getPropertyAsString(), colToAdd, vRow);
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
