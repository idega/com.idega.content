package com.idega.content.tree;

import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.SortedMap;

import javax.faces.component.html.HtmlOutputLink;
import javax.faces.component.html.HtmlOutputText;
import javax.faces.context.FacesContext;

import org.apache.myfaces.custom.tree2.TreeNode;
import org.jdom2.Document;
import org.jdom2.Element;
import org.jdom2.JDOMException;
import org.jdom2.input.SAXBuilder;

import com.idega.block.web2.presentation.Accordion;
import com.idega.content.themes.business.TemplatesLoader;
import com.idega.core.data.ICTreeNode;
import com.idega.core.data.IWTreeNode;
import com.idega.idegaweb.IWMainApplication;
import com.idega.presentation.IWBaseComponent;
import com.idega.presentation.IWContext;
import com.idega.presentation.Layer;
import com.idega.presentation.Script;
import com.idega.presentation.text.Text;
import com.idega.util.CoreConstants;
import com.idega.util.CoreUtil;
import com.idega.webface.IWTree;
import com.idega.webface.WFTreeNode;
import com.idega.webface.WFUtil;

public class SiteTemplatesViewer extends IWBaseComponent {

	private Map <String, PageTemplate> pageMap = null;
	private SortedMap <String, SiteTemplate> siteMap = null;

	public SiteTemplatesViewer() {
		super();
		FacesContext ctx = FacesContext.getCurrentInstance();
		IWMainApplication iwma = IWMainApplication.getIWMainApplication(ctx);
		pageMap = TemplatesLoader.getInstance(iwma).getPageTemplates();
		siteMap = TemplatesLoader.getInstance(iwma).getSiteTemplates();
	}

	@Override
	protected void initializeComponent(FacesContext context) {
		Iterator<String> itrKeySet = null;
		if (siteMap.keySet() != null) {
			itrKeySet = siteMap.keySet().iterator();
		}
		String mapKey = null;

		Layer container = new Layer();
		add(container);

		Accordion accordion = new Accordion("site_templates");
		accordion.setOnActiveScriptString("toggler.addClass('selectedToggler'); element.addClass('selectedStretch');");
		accordion.setOnBackgroundScriptString("toggler.removeClass('selectedToggler'); element.removeClass('selectedStretch');");
		container.add(accordion);

		int panelID = 0;
		Script script = new Script();

		for(Iterator<String> it = itrKeySet; it.hasNext();) {
			panelID++;
			mapKey = it.next();
			SiteTemplate currentSite = siteMap.get(mapKey);
			String panelName = mapKey;
			WFTreeNode rootNode = new WFTreeNode(new IWTreeNode(panelName));

			rootNode = getPage(currentSite, rootNode);
			rootNode = settingIconURIsAndTemplateFiles(rootNode);
			IWTree tree = new IWTree();
			tree.setValue(rootNode);
		    tree.setShowRootNode(false);
		    tree.setId("tree"+panelID);
		    tree.setShowLines(false);
		    tree.setVar("node");
		    tree.setRendererType("com.idega.webface.IWTree");
		    HtmlOutputLink linki = new HtmlOutputLink();
		    linki.setValue(CoreConstants.HASH);
		    linki.getAttributes().put("iconURI", "testValue");
		    HtmlOutputText texti = new HtmlOutputText();
		    texti.setValueBinding("value",WFUtil.createValueBinding("#{node.description}"));

		    linki.getChildren().add(texti);
		    tree.getFacets().put("IWTreeNode",linki);
		    tree.getAttributes().put("sourceTree", "true");

		    accordion.addPanel("panel"+panelID, new Text(panelName), tree);
		    script.addScriptLine(new StringBuffer("window.addEvent('domready', function() {appendIdOfTree('tree").append(panelID).append("');});").toString());
		}

		add(script);
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

	private void preparePageMap() {
		if (pageMap == null) {
			IWContext iwc = CoreUtil.getIWContext();
			IWMainApplication iwma = iwc.getApplicationContext().getIWMainApplication();
			TemplatesLoader loader = TemplatesLoader.getInstance(iwma);
			pageMap = loader.getPageMap();
			if(pageMap.isEmpty()){
				loader.loadTemplatesFromBundles();
				pageMap = loader.getPageMap();
			}
		}
	}

	private String getTemplateFileByPageType(String pageType){
		preparePageMap();

		if (pageMap.get(pageType) == null) {
			return CoreConstants.EMPTY;
		}
		return pageMap.get(pageType).getTemplateFile();
	}

	private String getIconUriByPageType(String pageType){
		preparePageMap();

		if (pageMap.get(pageType) == null) {
			return CoreConstants.EMPTY;
		}
		return pageMap.get(pageType).getIconFile();
	}

	public TreeNode getTree(IWTreeNode rootNode){
		ICTreeNode icnode = rootNode;
		return new WFTreeNode(icnode);
	}

	public WFTreeNode getPage(SiteTemplate currElement, WFTreeNode currNode){
		Iterator<SiteTemplate> itr = (currElement.getChildStructure()).iterator();
		String pageType = null;
		String iconFile = null;
		String templateFile = null;
		while(itr.hasNext()){
			SiteTemplate current = itr.next();

			WFTreeNode newNode = new WFTreeNode(new IWTreeNode(current.getName()));
			pageType = current.getType();
			newNode.setPageType(pageType);

			iconFile = current.getIconFile();
			templateFile = current.getTemplateFile();

			PageTemplate pageTemplate = pageMap.get(pageType);
			if ((iconFile == null) && (pageType != null)){
				if (pageTemplate != null)
					iconFile = pageMap.get(pageType).getIconFile();
			}
			if ((templateFile == null) && (pageType != null))
				if (pageTemplate != null)
					templateFile = pageMap.get(pageType).getTemplateFile();
			newNode.setIconURI(iconFile);
			newNode.setTemplateURI(templateFile);

			if(!current.getChildStructure().isEmpty()){
				newNode = getPage(current, newNode);
			}
			currNode.addChild(newNode);
		}
		return currNode;
	}

	public Document getXMLDocument(String link) {
		URL url = null;
		try {
			url = new URL(link);
		} catch (MalformedURLException e) {
			return null;
		}
		SAXBuilder builder = new SAXBuilder();
		Document document = null;
		try {
			document = builder.build(url);
		} catch (JDOMException e) {
			return null;
		} catch (IOException e) {
			return null;
		}
		return document;
	}

	public SiteTemplate getNode(Element currElement){

		String pageName = null;
		String pageType = null;
		String iconFile = null;
		String templateFile = null;
		SiteTemplate currNode = new SiteTemplate();
		pageType = currElement.getAttributeValue("type");
		currNode.setType(pageType);
		pageName = currElement.getAttributeValue("name");
		currNode.setName(pageName);

		iconFile = currElement.getAttributeValue("iconfile");
		templateFile = currElement.getAttributeValue("templatefile");
		if ((iconFile == null) && (pageType != null))
			iconFile = pageMap.get(pageType).getIconFile();
		if ((templateFile == null) && (pageType != null))
			templateFile = pageMap.get(pageType).getTemplateFile();
		currNode.setIconFile(iconFile);
		currNode.setTemplateFile(templateFile);
		for (Iterator<Element> it = currElement.getChildren().iterator(); it.hasNext();)
			currNode.addChild(getNode(it.next()));

		return currNode;
	}
}
