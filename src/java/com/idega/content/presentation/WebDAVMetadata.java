/*
 * $Id: WebDAVMetadata.java,v 1.1 2005/01/10 10:26:01 joakim Exp $
 *
 * Copyright (C) 2004 Idega. All Rights Reserved.
 *
 * This software is the proprietary information of Idega.
 * Use is subject to license terms.
 *
 */
package com.idega.content.presentation;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import javax.faces.component.UIInput;
import javax.faces.component.UISelectItems;
import javax.faces.component.html.HtmlCommandButton;
import javax.faces.component.html.HtmlSelectOneMenu;
import javax.faces.event.AbortProcessingException;
import javax.faces.event.ActionEvent;
import javax.faces.event.ActionListener;
import javax.faces.model.SelectItem;
import com.idega.presentation.IWContext;
import com.idega.presentation.Table;
import com.idega.util.Timer;
import com.idega.webface.WFContainer;
import com.idega.webface.WFResourceUtil;


/**
 * 
 * Last modified: $Date: 2005/01/10 10:26:01 $ by $Author: joakim $
 *
 * @author Joakim Johnson
 * @version $Revision: 1.1 $
 */
public class WebDAVMetadata implements ActionListener{
	
	private static final String NEW_VALUES_ID = "newValueID";
	private static final String ADD_ID = "addID";
	public static final ArrayList metadataType = new ArrayList();
	
	{
		metadataType.add("categories");
		metadataType.add("keywords");
		metadataType.add("publisher");
	}
	
	public WFContainer getMetadataTable() {
		WFResourceUtil localizer = WFResourceUtil.getResourceUtilContent();
		WFContainer mainContainer = new WFContainer();
//		mainContainer.setId(METADATA_ID);
		
		mainContainer.add(ContentBlock.getBundle().getLocalizedText("metadata"));

		Timer timer = new Timer();
		timer.start();
//		List versions = VersionHelper.getAllVersions(resource);

		Table metadataTable = new Table(3,2);
		metadataTable.setId(metadataTable.getId() + "_ver");
		metadataTable.setRowStyleClass(1,"wf_listheading");
		metadataTable.setStyleClass("wf_listtable");
		
		//Header
		int vRow = 1;
		int vColumn = 1;
		metadataTable.add(ContentBlock.getBundle().getLocalizedText("type"), vColumn, vRow);
		metadataTable.add(ContentBlock.getBundle().getLocalizedText("values"), ++vColumn, vRow);
//		metadataTable.add("", ++vColumn, vRow);
		vColumn = 1;
		vRow++;

		//Existing metadata
		
		
		//Add line
		ArrayList typesLeft = new ArrayList(metadataType);
		List l = new ArrayList();
		
		UIInput dropdown = new HtmlSelectOneMenu();
//		dropdown.setId(getUIComponentID(var, bindingNames[3]));

		Locale locale = IWContext.getInstance().getCurrentLocale();
		
		Iterator iter = typesLeft.iterator();
		while(iter.hasNext()) {
			String type = (String)iter.next();
			String label = ContentBlock.getBundle().getLocalizedString(type,locale);
			
			SelectItem item = new SelectItem(type, label, type, false);
			l.add(item);
		}

		UISelectItems sItems = new UISelectItems();
		sItems.setValue(l) ;
		dropdown.getChildren().add(sItems);
		
		metadataTable.add(dropdown, vColumn++, vRow);
		
//		HtmlInputText newValueInput = WFUtil.getInputText(NEW_VALUES_ID, "");		
//		newValueInput.setSize(40);
//		metadataTable.add(newValueInput);
		metadataTable.add(ContentBlock.getBundle().getLocalizedText("values"), vColumn++, vRow);
		
		HtmlCommandButton addButton = localizer.getButtonVB(ADD_ID, "add", this);
		metadataTable.add(addButton, vColumn++, vRow);

		/*
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
		*/
		
		mainContainer.add(metadataTable);
		return mainContainer;
	}

	/* (non-Javadoc)
	 * @see javax.faces.event.ActionListener#processAction(javax.faces.event.ActionEvent)
	 */
	public void processAction(ActionEvent arg0) throws AbortProcessingException {
		// TODO Auto-generated method stub
		
	}


}
