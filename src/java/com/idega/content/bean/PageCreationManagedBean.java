/*
 * $Id: PageCreationManagedBean.java,v 1.19 2008/02/22 08:20:52 valdas Exp $
 * Created on 2.5.2005
 *
 * Copyright (C) 2005 Idega Software hf. All Rights Reserved.
 *
 * This software is the proprietary information of Idega hf.
 * Use is subject to license terms.
 */
package com.idega.content.bean;

import java.rmi.RemoteException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import javax.ejb.FinderException;
import javax.faces.component.UICommand;
import javax.faces.event.AbortProcessingException;
import javax.faces.event.ActionEvent;
import javax.faces.event.ActionListener;
import javax.faces.model.SelectItem;

import org.apache.myfaces.custom.tree2.TreeNode;
import org.apache.myfaces.custom.tree2.TreeNodeBase;

import com.idega.content.business.ContentUtil;
import com.idega.content.themes.business.TemplatesLoader;
import com.idega.content.themes.helpers.business.ThemesConstants;
import com.idega.content.themes.helpers.business.ThemesHelper;
import com.idega.content.tree.PageTemplate;
import com.idega.core.accesscontrol.business.NotLoggedOnException;
import com.idega.core.builder.business.BuilderService;
import com.idega.core.builder.business.BuilderServiceFactory;
import com.idega.core.builder.data.ICPage;
import com.idega.core.builder.data.ICPageHome;
import com.idega.core.data.ICTreeNode;
import com.idega.data.IDOLookup;
import com.idega.data.IDOLookupException;
import com.idega.idegaweb.IWBundle;
import com.idega.idegaweb.IWMainApplication;
import com.idega.presentation.IWContext;
import com.idega.util.CoreConstants;
import com.idega.util.CoreUtil;
import com.idega.webface.WFTreeNode;

/**
 * 
 *  Last modified: $Date: 2008/02/22 08:20:52 $ by $Author: valdas $
 * 
 * @author <a href="mailto:gummi@idega.com">Gudmundur Agust Saemundsson</a>
 * @version $Revision: 1.19 $
 */
public class PageCreationManagedBean implements ActionListener {

	private String SELECT_ITEM_KEY_NO_TEMPLATE_SELECTED = "no_template_selected";
	private int pageSelectorTopNode = -1;
	
	private String selectedPageLocationIdentifier = null;
	private String selectedPageLocationName = "[Select page]";
	private String pageName = "Untitled";
	private static final String RELATIVE_LOCATION_BEFORE = "before";
	private String relativeLocation = RELATIVE_LOCATION_BEFORE;
	private String templateIdentifier = this.SELECT_ITEM_KEY_NO_TEMPLATE_SELECTED;
	private Map<String, PageTemplate> pageMap = null;
	
	/**
	 * 
	 */
	public PageCreationManagedBean() {
		super();
	}
	
	public TreeNode getPageSelectorTopNode() {
		IWContext iwc = CoreUtil.getIWContext();
		if (iwc == null) {
			return getEmptyNode();
		}
		
		BuilderService builderService = null;
		try {
			builderService = BuilderServiceFactory.getBuilderService(iwc);
		} catch (RemoteException e) {
			e.printStackTrace();
			return getEmptyNode();
		}
		
		if (this.pageSelectorTopNode == -1) {
			try {
				this.pageSelectorTopNode = builderService.getRootPageId();
			} catch (RemoteException e) {
				e.printStackTrace();
				return getEmptyNode();
			}
			
			if (pageSelectorTopNode < 0) {
				return getEmptyNode();
			}
		}
		
		int currentUserId = -1;
		try {
			currentUserId = iwc.getCurrentUserId();
		} catch (NotLoggedOnException nle) {
			nle.printStackTrace();
			return getEmptyNode();
		}
		
		WFTreeNode node = new WFTreeNode();
		List <ICTreeNode> topLevelPages = null;
		try {
			topLevelPages = new ArrayList<ICTreeNode>(builderService.getTopLevelPages(iwc));
		} catch(Exception e) {
			e.printStackTrace();
		}
		if (topLevelPages == null) {
			return node;
		}
		
		ICTreeNode startPage = null;
		ICTreeNode page = null;
		for (int i = 0; i < topLevelPages.size(); i++) {
			startPage = topLevelPages.get(i);
			if (ThemesConstants.MINUS_ONE.equals(startPage.getId()) || startPage.getId() == null) {
				// Do nothing, tree is empty
			}
			else {
				try {
					page = builderService.getPageTree(Integer.parseInt(startPage.getId()), currentUserId);
				} catch (NumberFormatException e) {
					e.printStackTrace();
				} catch (RemoteException e) {
					e.printStackTrace();
				}
				if (page != null) {
					node.addChild(page);
				}
			}
		}
		
		node = settingIconURIsAndTemplateFiles(node);
		return node;
	}
	
	private TreeNode getEmptyNode() {
		return new TreeNodeBase("type", "description", true);
	}
	
	private WFTreeNode settingIconURIsAndTemplateFiles(WFTreeNode node){
		node.setIconURI(getIconUriByPageType(node.getPageType()));
		node.setTemplateURI(getTemplateFileByPageType(node.getPageType()));
		List<WFTreeNode> nodeChildren = node.getChildren();
		if (nodeChildren != null)
			for (int i = 0; i < nodeChildren.size(); i++){
				nodeChildren.set(i, settingIconURIsAndTemplateFiles(nodeChildren.get(i)));
			}		
		return node;
	}
	private String getTemplateFileByPageType(String pageType){
		if (pageMap == null){
			IWContext iwc = CoreUtil.getIWContext();
			IWMainApplication iwma = iwc.getApplicationContext().getIWMainApplication();
			TemplatesLoader loader = TemplatesLoader.getInstance(iwma);
			pageMap = loader.getPageMap();
			if(pageMap.isEmpty()){
				loader.loadTemplatesFromBundles();
				pageMap = loader.getPageMap();
			}	
		}
	
		if(pageMap.get(pageType) != null)
			return pageMap.get(pageType).getTemplateFile();
		else {
			return "";
		}
	}
	private String getIconUriByPageType(String pageType){
		
		if (pageMap == null){
			IWContext iwc = CoreUtil.getIWContext();
			IWMainApplication iwma = iwc.getApplicationContext().getIWMainApplication();
			TemplatesLoader loader = TemplatesLoader.getInstance(iwma);
			pageMap = loader.getPageMap();
			if(pageMap.isEmpty()){
				loader.loadTemplatesFromBundles();
				pageMap = loader.getPageMap();
			}	
		}
		
		if(pageMap.get(pageType) != null)
			return pageMap.get(pageType).getIconFile();
		else {
			return CoreConstants.EMPTY;
		}
	}
	
	public String getResourceRealPath(){
		return ContentUtil.getBundle().getResourcesRealPath();
	}
	
	public IWBundle getCoreBundle(){
		IWContext iwc = CoreUtil.getIWContext();
		return iwc.getApplicationContext().getIWMainApplication().getCoreBundle();
	}
	
	/**
	 * @return Returns the pageName.
	 */
	public String getPageName() {
		return this.pageName;
	}
	/**
	 * @param pageName The pageName to set.
	 */
	public void setPageName(String pageName) {
		this.pageName = pageName;
	}
	/**
	 * @return Returns the relativeLocation.
	 */
	public String getRelativeLocation() {
		return this.relativeLocation;
	}
	/**
	 * @param relativeLocation The relativeLocation to set.
	 */
	public void setRelativeLocation(String relativeLocation) {
		this.relativeLocation = relativeLocation;
	}
	/**
	 * @return Returns the selectedPageLocationIdentifier.
	 */
	public String getSelectedPageLocationIdentifier() {
		return this.selectedPageLocationIdentifier;
	}
	/**
	 * @param selectedPageLocation The selectedPageLocationIdentifier to set.
	 */
	public void setSelectedPageLocationIdentifier(String selectedPageLocationIdentifier) {
		this.selectedPageLocationIdentifier = selectedPageLocationIdentifier;
	}

	/* (non-Javadoc)
	 * @see javax.faces.event.ActionListener#processAction(javax.faces.event.ActionEvent)
	 */
	public void processAction(ActionEvent actionEvent) throws AbortProcessingException {
		String componentID = actionEvent.getComponent().getId();
		System.out.println("Action "+componentID+" processed!!!!!");
		
		IWContext iwc = CoreUtil.getIWContext();
		UICommand command = (UICommand)actionEvent.getComponent();
		System.out.println("UICommand.action:"+command.getAction());
		System.out.println("UICommand.value:"+command.getValue());
		System.out.println("UICommand.attribute.action:"+command.getAttributes().get("action"));
		System.out.println("UICommand.action.expressionString:"+command.getAction().getExpressionString());
		if(componentID.equals("saveCommand")){
			savePage(iwc);
		} else {
			reset();
		}
		
	}
	
	public void reset(){
		System.out.println("Reset-Action processed!!!!!");
		this.selectedPageLocationIdentifier = null;
		this.selectedPageLocationName = "[Select page]";
		this.pageName = "Untitled";
		this.relativeLocation = RELATIVE_LOCATION_BEFORE;
		
	}

	/**
	 * @return
	 */
	public String getParentPageIdentifier() {
		return getSelectedPageLocationIdentifier();
	}

	public void savePage(IWContext iwc) {
		throw new RuntimeException("Function Disabled");
	}
	
	/**
	 * @return Returns the selectedPageLocationName.
	 */
	public String getSelectedPageLocationName() {
		return this.selectedPageLocationName;
	}
	/**
	 * @param selectedPageLocationName The selectedPageLocationName to set.
	 */
	public void setSelectedPageLocationName(String selectedPageLocationName) {
		this.selectedPageLocationName = selectedPageLocationName;
	}
	
	public List getSimpleTemplateSelectItemList(){
		List l = new ArrayList();
		l.add(new SelectItem(this.SELECT_ITEM_KEY_NO_TEMPLATE_SELECTED,"[Select Template]"));
		try {
			IWContext iwc = CoreUtil.getIWContext();
			
			Collection templates = ((ICPageHome) IDOLookup.getHome(ICPage.class)).findAllSimpleTemplates();
			for (Iterator iter = templates.iterator(); iter.hasNext();) {
				ICPage t = (ICPage) iter.next();
				l.add(new SelectItem(t.getPrimaryKey().toString(),t.getName(iwc.getCurrentLocale())));
			}
		}
		catch (IDOLookupException e) {
			e.printStackTrace();
		}
		catch (FinderException e) {
			e.printStackTrace();
		}
		
		return l;
	}
	
	/**
	 * @return Returns the templateIdentifier.
	 */
	public String getTemplateIdentifier() {
		return this.templateIdentifier;
	}
	/**
	 * @param templateIdentifier The templateIdentifier to set.
	 */
	public void setTemplateIdentifier(String templateIdentifier) {
		this.templateIdentifier = templateIdentifier;
	}
}
