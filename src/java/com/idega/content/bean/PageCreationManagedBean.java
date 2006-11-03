/*
 * $Id: PageCreationManagedBean.java,v 1.7 2006/11/03 14:35:13 justinas Exp $
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

import javax.ejb.FinderException;
import javax.faces.component.UICommand;
import javax.faces.event.AbortProcessingException;
import javax.faces.event.ActionEvent;
import javax.faces.event.ActionListener;
import javax.faces.model.SelectItem;

import org.apache.myfaces.custom.tree2.TreeNode;
import org.apache.myfaces.custom.tree2.TreeNodeBase;

////import com.idega.builder.business.BuilderLogic;
//import com.idega.builder.business.DomainTree;
//import com.idega.builder.business.PageTreeNode;
//import com.idega.builder.business.BuilderLogic;
//import com.idega.builder.business.DomainTree;
import com.idega.content.business.ContentUtil;
import com.idega.core.accesscontrol.business.NotLoggedOnException;
import com.idega.core.builder.business.BuilderService;
import com.idega.core.builder.business.BuilderServiceFactory;
import com.idega.core.builder.data.ICDomain;
import com.idega.core.builder.data.ICPage;
import com.idega.core.builder.data.ICPageHome;
import com.idega.core.data.ICTreeNode;
import com.idega.core.data.ICTreeNodeAddable;
import com.idega.core.data.IWTreeNode;
import com.idega.data.IDOLookup;
import com.idega.data.IDOLookupException;
import com.idega.idegaweb.IWApplicationContext;
import com.idega.idegaweb.IWBundle;
import com.idega.idegaweb.UnavailableIWContext;
import com.idega.presentation.IWContext;
import com.idega.presentation.Page;
import com.idega.presentation.text.Link;
import com.idega.presentation.ui.TreeViewer;
import com.idega.webface.WFTreeNode;

//import com.idega.builder.business;


/**
 * 
 *  Last modified: $Date: 2006/11/03 14:35:13 $ by $Author: justinas $
 * 
 * @author <a href="mailto:gummi@idega.com">Gudmundur Agust Saemundsson</a>
 * @version $Revision: 1.7 $
 */
public class PageCreationManagedBean implements ActionListener {

	private String SELECT_ITEM_KEY_NO_TEMPLATE_SELECTED = "no_template_selected";
	private final String LINK_STYLE = "font-family:Arial,Helvetica,sans-serif;font-size:8pt;color:#000000;text-decoration:none;";
	private static String DOMAIN_TREE_KEY="ic_domain_tree_";
	private int pageSelectorTopNode = -1;
	
	private int pageSelectorTopNodes = -1;	
	
	private String selectedPageLocationIdentifier = null;
	private String selectedPageLocationName = "[Select page]";
	private String pageName = "Untitled";
	private static final String RELATIVE_LOCATION_BEFORE = "before";
	private String relativeLocation = RELATIVE_LOCATION_BEFORE;
	private String templateIdentifier = this.SELECT_ITEM_KEY_NO_TEMPLATE_SELECTED;
	
	
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

	
	public TreeNode getPageSelectorTopNode(){
		try {
			System.out.println("getPageSelectorTopNode");
		IWContext iwc = IWContext.getInstance();
		BuilderService bservice = BuilderServiceFactory.getBuilderService(iwc);
		ICDomain domain = BuilderServiceFactory.getBuilderService(iwc).getCurrentDomain();

		if (this.pageSelectorTopNode == -1) {
			this.pageSelectorTopNode = bservice.getRootPageId();
		}
		int currentUserId = -1;
		Collection coll = null;
		coll = BuilderServiceFactory.getBuilderService(iwc).getTopLevelPages(iwc);
//		coll = DomainTree.getDomainTree(iwc).getPagesNode().getChildren();//getStartPages(domain);
		Iterator it = coll.iterator();
		int id = domain.getStartPageID();
		WFTreeNode node = new WFTreeNode();
		
		while (it.hasNext()) {
			ICTreeNode startPage;
			try {
				startPage = (ICTreeNode)it.next();
				System.out.println("id = "+ startPage.getId());			
//				if(!startPage.getId().equals(Integer.toString(id))){
//					if(node == null){
//						node = (WFTreeNode)(bservice.getPageTree(bservice.getRootPageId(), currentUserId));
//						int childCount = node.getChildCount();
//						for (int i = 0; i < childCount; i++) {
//							System.out.println("removing "+i);
//							System.out.println("removing "+node.getChildren());
//							node.getChildren().remove(0);
//							
//							
//						}
//				 		System.out.println("after clear children = " + node.getChildCount());
//				 		System.out.println("after clear children2222 = " + node.getChildren().size());
//					}
//					node.
//					System.out.println("before node.getChildCount()"+node.getChildCount());
//					Collection col = bservice.getPageTree(Integer.parseInt(startPage.getId()), currentUserId);
System.out.println("childs before = "+node.getChildCount());
					node.addChild(bservice.getPageTree(Integer.parseInt(startPage.getId()), currentUserId));
					
System.out.println("node has children = "+(bservice.getPageTree(Integer.parseInt(startPage.getId()), currentUserId)).getChildCount());					
			} catch (Exception e) {
				// TODO: handle exception
				System.out.println("have exception");
				e.printStackTrace();
			}
		}
		try {
			currentUserId = iwc.getCurrentUserId();
		} catch (NotLoggedOnException nle) {}
		return node;
	}
	catch (UnavailableIWContext e) {
		e.printStackTrace();
	}
	catch (RemoteException e) {
		e.printStackTrace();
	}
			
	return new TreeNodeBase("type","description",true);
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
