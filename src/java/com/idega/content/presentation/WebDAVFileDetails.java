/*
 * Created on 14.12.2004
 */
package com.idega.content.presentation;

import java.io.IOException;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.Set;

import javax.faces.component.html.HtmlOutputLink;
import javax.faces.component.html.HtmlPanelGrid;

import net.sourceforge.myfaces.component.html.ext.HtmlPanelGroup;

import org.apache.commons.httpclient.HttpException;
import org.apache.webdav.lib.WebdavResource;
import org.apache.webdav.lib.WebdavResources;

import com.idega.business.IBOLookup;
import com.idega.presentation.IWContext;
import com.idega.slide.authentication.AuthenticationBusiness;
import com.idega.slide.business.IWSlideSession;
import com.idega.slide.util.PropertyParser;
import com.idega.slide.util.VersionHelper;
import com.idega.webface.WFUtil;

/**
 * @author gimmi
 */
public class WebDAVFileDetails extends ContentBlock {

	protected void initializeContent() {
		this.setId(this.getId());

		WebdavResource resource = getWebdavResource();
		
		HtmlPanelGrid grid = new HtmlPanelGrid();
		grid.setId(this.getId()+"_grid");
		grid.setBorder(2);
		grid.setColumns(2);
		grid.getFacets().put("header", WFUtil.getText("Document Details"));

		
		if (resource != null) {
			grid.getChildren().add(WFUtil.getText(resource.getName()));
			HtmlOutputLink link = new HtmlOutputLink();
			link.setValue(resource.getPath());
			link.getChildren().add(WFUtil.getText("Download/View"));
			grid.getChildren().add(link);

				WebdavResources rs = VersionHelper.getAllVersions(resource);
				Enumeration rsEnum = rs.getResources();

				HtmlPanelGroup group = new HtmlPanelGroup();
				group.setId(grid.getId()+"_ver_g");
				HtmlPanelGrid versionGrid = new HtmlPanelGrid();
				versionGrid.setStyleClass("wf_listtable");
				versionGrid.setHeaderClass("wf_listheading");
				versionGrid.setRowClasses("wf_listoddrow,wf_listevenrow");
				versionGrid.setColumns(2);
				versionGrid.setId(grid.getId()+"_ver");
				versionGrid.getFacets().put("header", WFUtil.getText("Version history"));
				group.getChildren().add(versionGrid);
//						grid.getFacets().put("footer", group);

				grid.getChildren().add(group);
				
				while (rsEnum.hasMoreElements()) {
					WebdavResource enumR = (WebdavResource) rsEnum.nextElement();
					
					versionGrid.getChildren().add(WFUtil.getText(enumR.getDisplayName()));
					versionGrid.getChildren().add(WFUtil.getText(enumR.getName()));
				}

			this.getChildren().add(grid);
		} 
		
	}
	


}
