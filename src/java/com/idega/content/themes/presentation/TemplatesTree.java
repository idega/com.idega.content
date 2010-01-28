package com.idega.content.themes.presentation;

import java.rmi.RemoteException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import com.idega.content.business.ContentConstants;
import com.idega.core.builder.business.BuilderService;
import com.idega.core.builder.data.ICPage;
import com.idega.core.builder.data.ICPageHome;
import com.idega.core.business.ICTreeNodeComparator;
import com.idega.core.data.ICTreeNode;
import com.idega.data.IDOLookup;
import com.idega.idegaweb.IWBundle;
import com.idega.idegaweb.IWResourceBundle;
import com.idega.presentation.Block;
import com.idega.presentation.IWContext;
import com.idega.presentation.Image;
import com.idega.presentation.Layer;
import com.idega.presentation.text.Heading2;
import com.idega.presentation.text.Heading4;
import com.idega.presentation.text.Link;
import com.idega.presentation.text.ListItem;
import com.idega.presentation.text.Lists;
import com.idega.presentation.ui.GenericButton;
import com.idega.util.CoreConstants;
import com.idega.util.ListUtil;

public class TemplatesTree extends Block {

	@SuppressWarnings("unchecked")
	@Override
	public void main(IWContext iwc) {
		Layer container = new Layer();
		container.setId("templatesTreeContainerInLucid");
		container.setStyleClass("templatesTreeContainerInLucidStyle");
		add(container);
		
		IWBundle iwb = getBundle(iwc);
		IWResourceBundle iwrb = iwb.getResourceBundle(iwc);
		
		BuilderService builder = null;
		try {
			builder = getBuilderService(iwc);
		} catch (RemoteException e) {
			e.printStackTrace();
		}
		if (builder == null) {
			container.add(new Heading2(iwrb.getLocalizedString("can_not_get_templates_tree", "Error: can not display templates.")));
			return;
		}
		
		Collection topLevelTemplates = builder.getTopLevelTemplates(iwc);
		if (ListUtil.isEmpty(topLevelTemplates)) {
			container.add(new Heading4(iwrb.getLocalizedString("there_are_no_templates", "There are no templates in system")));
			return;
		}
		
		Lists templates = new Lists();
		templates.setStyleClass("templatesTreeInLucidStyle");
		container.add(templates);
		
		Map<String, ICPage> templatesObjects = getTemplatesObjects(getPrimaryKeys(topLevelTemplates));
		
		addTemplatesToTree(templates, topLevelTemplates, iwc.getCurrentLocale(), iwb, templatesObjects);
		
		Layer buttons = new Layer();
		buttons.setStyleClass("webfaceButtonLayer");
		container.add(buttons);
		GenericButton createTemplate = new GenericButton("createChildTemplate", iwrb.getLocalizedString("create_child_template", "Create child template"));
		createTemplate.setStyleClass("createChildTemplateForCurrentTemplateButtonInLucidStyle");
		createTemplate.setTitle(iwrb.getLocalizedString("create_child_template_for_current_template", "Create child template for current template"));
		buttons.add(createTemplate);
	}
	
	private List<String> getPrimaryKeys(Collection<ICTreeNode> templates) {
		if (ListUtil.isEmpty(templates)) {
			return null;
		}
		
		List<String> primaryKeys = new ArrayList<String>(templates.size());
		for (ICTreeNode template: templates) {
			primaryKeys.add(template.getId());
		}
		return primaryKeys;
	}
	
	@SuppressWarnings("unchecked")
	private Map<String, ICPage> getTemplatesObjects(List<String> primaryKeys) {
		if (ListUtil.isEmpty(primaryKeys)) {
			return null;
		}
		
		try {
			ICPageHome pageHome = (ICPageHome) IDOLookup.getHome(ICPage.class);
			Collection<ICPage> objects = pageHome.findAllByPrimaryKeys(primaryKeys);
			if (ListUtil.isEmpty(objects)) {
				return null;
			}
			
			Map<String, ICPage> mappedObjects = new HashMap<String, ICPage>();
			for (ICPage templateObject: objects) {
				mappedObjects.put(templateObject.getId(), templateObject);
			}
			return mappedObjects;
		} catch (Exception e) {
			e.printStackTrace();
		}
		return null;
	}
	
	@SuppressWarnings("unchecked")
	private void addTemplatesToTree(Lists tree, Collection<ICTreeNode> templates, Locale l, IWBundle iwb, Map<String, ICPage> templatesObjects) {
		if (ListUtil.isEmpty(templates)) {
			return;
		}
		
		List<ICTreeNode> topTemplates = new ArrayList<ICTreeNode>(templates);
		Collections.sort(topTemplates, new ICTreeNodeComparator(l));
		
		String name = null;
		Collection templateChildren = null;
		String imageUri = iwb.getVirtualPathWithFileNameString("images/template.png");
		String folderImageUri = iwb.getVirtualPathWithFileNameString("images/folder_template.png");
		for (ICTreeNode template: topTemplates) {
			ICPage templateObject = templatesObjects == null ? null : templatesObjects.get(template.getId());
			if (templateObject == null) {
				continue;
			}
			if (!templateObject.isTemplate() || templateObject.getDeleted() || templateObject.isHidePageInMenu()) {
				continue;
			}
			
			name = null;
			templateChildren = null;
			
			name = template.getNodeName(l);
			if (name == null || CoreConstants.EMPTY.equals(name)) {
				name = template.getNodeName();
			}
			
			ListItem item = new ListItem();
			
			Image icon = new Image(imageUri);
			item.add(icon);
			
			Link templateName = new Link(name);
			templateName.setURL("javascript:void(0);");
			templateName.setStyleClass("templateNameInLucidTemplatesTreeStyle");
			templateName.setMarkupAttribute("templateid", template.getId());
			item.add(templateName);
			tree.add(item);
			
			templateChildren = template.getChildren();
			if (!ListUtil.isEmpty(templateChildren)) {
				icon.setURL(folderImageUri);
				
				Lists newTree = new Lists();
				item.add(newTree);
				
				addTemplatesToTree(newTree, templateChildren, l, iwb, getTemplatesObjects(getPrimaryKeys(templateChildren)));
			}
		}
	}
	
	@Override
	public String getBundleIdentifier() {
		return ContentConstants.IW_BUNDLE_IDENTIFIER;
	}
	
}