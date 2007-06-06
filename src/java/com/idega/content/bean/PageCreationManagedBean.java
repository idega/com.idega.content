/*
 * $Id: PageCreationManagedBean.java,v 1.14 2007/06/06 11:01:32 justinas Exp $
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

import com.idega.content.business.ContentConstants;
import com.idega.content.business.ContentUtil;
import com.idega.content.themes.business.TemplatesLoader;
import com.idega.content.themes.helpers.ThemesConstants;
import com.idega.content.themes.helpers.ThemesHelper;
import com.idega.content.tree.PageTemplate;
import com.idega.core.accesscontrol.business.NotLoggedOnException;
import com.idega.core.builder.business.BuilderService;
import com.idega.core.builder.data.ICPage;
import com.idega.core.builder.data.ICPageHome;
import com.idega.core.cache.IWCacheManager2;
import com.idega.core.data.ICTreeNode;
import com.idega.data.IDOLookup;
import com.idega.data.IDOLookupException;
import com.idega.idegaweb.IWBundle;
import com.idega.idegaweb.IWMainApplication;
import com.idega.presentation.IWContext;
import com.idega.webface.WFTreeNode;

//import com.idega.builder.business;


/**
 * 
 *  Last modified: $Date: 2007/06/06 11:01:32 $ by $Author: justinas $
 * 
 * @author <a href="mailto:gummi@idega.com">Gudmundur Agust Saemundsson</a>
 * @version $Revision: 1.14 $
 */
public class PageCreationManagedBean implements ActionListener {

	private String SELECT_ITEM_KEY_NO_TEMPLATE_SELECTED = "no_template_selected";
	//private final String LINK_STYLE = "font-family:Arial,Helvetica,sans-serif;font-size:8pt;color:#000000;text-decoration:none;";
	//private static String DOMAIN_TREE_KEY="ic_domain_tree_";
	private int pageSelectorTopNode = -1;
	
	//private int pageSelectorTopNodes = -1;	
	
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
	
//	public TreeNode getPageSelectorTopNode(){
//	public TreeNode getPageSelectorTopNodeOld(){
//		try {
//			IWContext iwc = IWContext.getInstance();
//			BuilderService bservice = BuilderServiceFactory.getBuilderService(iwc);
//			if (this.pageSelectorTopNode == -1) {
//				this.pageSelectorTopNode = bservice.getRootPageId();
//			}
//			int currentUserId = -1;
//			try {
//				currentUserId = iwc.getCurrentUserId();
//			} catch (NotLoggedOnException nle) {
//			}
//			ICTreeNode node = bservice.getPageTree(this.pageSelectorTopNode, currentUserId);
//			return new WFTreeNode(node);
//		}
//		catch (UnavailableIWContext e) {
//			e.printStackTrace();
//		}
//		catch (RemoteException e) {
//			e.printStackTrace();
//		}
//				
//		return new TreeNodeBase("type","description",true);
//	}

	
	public TreeNode getPageSelectorTopNode() {
		IWContext iwc = IWContext.getInstance();
		if (iwc == null) {
			return new TreeNodeBase("type", "description", true);
		}
		BuilderService bservice = ThemesHelper.getInstance().getThemesService().getBuilderService();
		if (this.pageSelectorTopNode == -1) {
			try {
				this.pageSelectorTopNode = bservice.getRootPageId();
			} catch (RemoteException e) {
				e.printStackTrace();
			}
		}
		int currentUserId = -1;
		try {
			currentUserId = iwc.getCurrentUserId();
		} catch (NotLoggedOnException nle) {
			nle.printStackTrace();
		}
		List <ICTreeNode> topLevelPages = new ArrayList <ICTreeNode> (bservice.getTopLevelPages(iwc));
		WFTreeNode node = new WFTreeNode();
		ICTreeNode startPage = null;
		ICTreeNode page = null;
		for (int i = 0; i < topLevelPages.size(); i++) {
			startPage = topLevelPages.get(i);
			if (ThemesConstants.MINUS_ONE.equals(startPage.getId()) || startPage.getId() == null) {
				// Do nothing, tree is empty
			}
			else {
				try {
					page = bservice.getPageTree(Integer.parseInt(startPage.getId()), currentUserId);
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
			IWContext iwc = IWContext.getInstance();
			IWMainApplication iwma = iwc.getApplicationContext().getIWMainApplication();
			TemplatesLoader loader = new TemplatesLoader(iwma);
			pageMap = loader.getPageMap();
			if(pageMap.isEmpty()){
				loader.loadTemplatesFromBundles();
				pageMap = loader.getPageMap();
			}
//			IWContext iwc = IWContext.getInstance();
//			IWMainApplication iwma = iwc.getApplicationContext().getIWMainApplication();
//			TemplatesLoader loader = new TemplatesLoader(iwma);
//			loader.loadTemplatesFromBundles();
//			Map pageTemplatesFromCache = IWCacheManager2.getInstance(iwma).getCache(ContentConstants.PAGE_TYPES_CACHE_KEY);
//			pageMap = (Map <String, PageTemplate>)pageTemplatesFromCache.get(ContentConstants.PAGES_MAP_KEY);
			
//			IWContext iwc = IWContext.getInstance();
//			Map pageTemplatesFromCache = IWCacheManager2.getInstance(iwc.getApplicationContext().getIWMainApplication()).getCache(ContentConstants.PAGE_TYPES_CACHE_KEY);
//			
//			if (pageTemplatesFromCache.containsKey(ContentConstants.PAGES_MAP_KEY)){
//				pageMap = (Map <String, PageTemplate>)pageTemplatesFromCache.get(ContentConstants.PAGES_MAP_KEY);
//			}
//			else {
//				pageMap = new HashMap <String, PageTemplate> ();
//				pageTemplatesFromCache.put(ContentConstants.PAGES_MAP_KEY, pageMap);
//			}		
		}
	
		if(pageMap.get(pageType) != null)
			return pageMap.get(pageType).getTemplateFile();
		else {
			return "";
		}
	}
	private String getIconUriByPageType(String pageType){
		
		if (pageMap == null){
			IWContext iwc = IWContext.getInstance();
			IWMainApplication iwma = iwc.getApplicationContext().getIWMainApplication();
			TemplatesLoader loader = new TemplatesLoader(iwma);
			pageMap = loader.getPageMap();
			if(pageMap.isEmpty()){
				loader.loadTemplatesFromBundles();
				pageMap = loader.getPageMap();
			}
				
			
			
//			IWContext iwc = IWContext.getInstance();
//			IWMainApplication iwma = iwc.getApplicationContext().getIWMainApplication();
//			TemplatesLoader loader = new TemplatesLoader(iwma);
//			loader.loadTemplatesFromBundles();
//			Map pageTemplatesFromCache = IWCacheManager2.getInstance(iwma).getCache(ContentConstants.PAGE_TYPES_CACHE_KEY);
//			pageMap = (Map <String, PageTemplate>)pageTemplatesFromCache.get(ContentConstants.PAGES_MAP_KEY);
//			IWContext iwc = IWContext.getInstance();
//			Map pageTemplatesFromCache = IWCacheManager2.getInstance(iwc.getApplicationContext().getIWMainApplication()).getCache(ContentConstants.PAGE_TYPES_CACHE_KEY);
//			Element root = pageDocument.getRootElement();		
//			Collection siteRoot = root.getChildren();								
//			Iterator itr = siteRoot.iterator();
//			
//			if (pageTemplatesFromCache.containsKey(ContentConstants.PAGES_MAP_KEY)){
//				pageMap = (Map <String, PageTemplate>)pageTemplatesFromCache.get(ContentConstants.PAGES_MAP_KEY);
//			}
//			else {
//				pageMap = new HashMap <String, PageTemplate> ();
//				pageTemplatesFromCache.put(ContentConstants.PAGES_MAP_KEY, pageMap);
//			}		
//			while(itr.hasNext()){
//				Element current = (Element)itr.next();
//				PageTemplate page = new PageTemplate();
//				pageType = current.getAttributeValue("type");
//				page.setName(current.getAttributeValue("name"));
//				page.setType(pageType);
//				if (!current.getAttributeValue("iconfile").equals(""))
//					page.setIconFile(iwma.getApplicationContextURI() + current.getAttributeValue("iconfile"));
//				else
//					page.setIconFile("");
//				page.setTemplateFile(iwma.getApplicationContextURI() + current.getAttributeValue("templatefile"));
//				pageMap.put(pageType, page);
//			}			
		}
		
		if(pageMap.get(pageType) != null)
			return pageMap.get(pageType).getIconFile();
		else {
//			System.out.println("pageType "+pageType);
			return "";
		}
	}
	
	public String getResourceRealPath(){
		return ContentUtil.getBundle().getResourcesRealPath();
	}
	
	public IWBundle getCoreBundle(){
		IWContext iwc = IWContext.getInstance();
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
		
		IWContext iwc = IWContext.getInstance();
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
	
	/*
	 * Temporarily commented out because of dependency problem on com.idega.builder
	 * 
	public void savePage(IWContext iwc) {
		System.out.println("Save-Action processed!!!!!");
		System.out.println("pageSelectorTopNode: "+this.pageSelectorTopNode);
		System.out.println("selectedPageLocationIdentifier: "+this.selectedPageLocationIdentifier);
		System.out.println("selectedPageLocationName: "+this.selectedPageLocationName);
		System.out.println("pageName: "+this.pageName);
		System.out.println("relativeLocation: "+this.relativeLocation );
		

		String stringSourceMarkup = getPageSource();

		System.out.println("---------JSP Page----------");
		System.out.println(stringSourceMarkup);
		System.out.println("---------JSP Page Ends----------");

		if(!this.SELECT_ITEM_KEY_NO_TEMPLATE_SELECTED.equals(getTemplateIdentifier())){
			//Create new page
			String templateID = getTemplateIdentifier();
			createSimpleTemplatePage(iwc, getParentPageIdentifier(), getPageName(), templateID,getBuilderLogic().PAGE_FORMAT_JSP_1_2,stringSourceMarkup);
		}
	}
	
	private String getPageSource() {
		if(!this.SELECT_ITEM_KEY_NO_TEMPLATE_SELECTED.equals(getTemplateIdentifier())){
			return getBuilderLogic().getPageSource(getTemplateIdentifier());
		} else {
			return null;
		}
	}

	private String createSimpleTemplatePage(IWContext iwc, String parentPageId, String name, String templateKey, String format, String sourceMarkup) {
		int id=-1;
		if (parentPageId != null) {
			Map tree = PageTreeNode.getTree(iwc);
			id = IBPageHelper.getInstance().createNewPage(parentPageId, name, IBPageHelper.PAGE, templateKey, tree, iwc,IBPageHelper.SUBTYPE_SIMPLE_TEMPLATE_PAGE,format,sourceMarkup);
		}
		if(id != -1){
			return String.valueOf(id);
		}
		return null;
	}
	
	protected BuilderLogic getBuilderLogic(){
		return BuilderLogic.getInstance();
	}
	*/
	
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
			IWContext iwc = IWContext.getInstance();
			
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
