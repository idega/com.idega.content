package com.idega.content.presentation;

import java.rmi.RemoteException;
import java.util.List;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;

import com.idega.business.IBOLookup;
import com.idega.business.IBOLookupException;
import com.idega.content.business.CommentsEngine;
import com.idega.content.business.ContentConstants;
import com.idega.content.business.ContentUtil;
import com.idega.content.themes.helpers.ThemesHelper;
import com.idega.core.accesscontrol.business.NotLoggedOnException;
import com.idega.core.builder.business.BuilderService;
import com.idega.core.builder.business.BuilderServiceFactory;
import com.idega.presentation.IWContext;
import com.idega.presentation.Image;
import com.idega.presentation.Script;
import com.idega.presentation.text.Link;
import com.idega.presentation.text.Text;
import com.idega.presentation.ui.CheckBox;
import com.idega.webface.WFDivision;

public class ContentItemComments extends ContentBlock {
	
	private String cacheKey = null;
	private String styleClass = "content_item_comments_style";
	private String linkToComments = null;
	private String parentModuleClassName = "com.idega.block.article.component.ArticleItemViewer";
	
	private boolean showCommentsForAllUsers = false;
	private boolean showCommentsList = false;
	private boolean isForumPage = false;
	
	private static final String COMMENTS_BLOCK_ID = "comments_block";
	
	public ContentItemComments(String cacheKey, String linkToComments, String parentModuleClassName, boolean showCommentsList,
			boolean isForumPage, boolean showCommentsForAllUsers) {
		this(cacheKey, linkToComments, showCommentsList, isForumPage, "content_item_comments_style", parentModuleClassName,
				showCommentsForAllUsers);
	}
	
	public ContentItemComments(String cacheKey, String linkToComments, boolean showCommentsList, boolean isForumPage,
			String styleClass, String parentModuleClassName, boolean showCommentsForAllUsers) {
		super();
		this.cacheKey = cacheKey;
		this.linkToComments = linkToComments;
		this.showCommentsList = showCommentsList;
		this.isForumPage = isForumPage;
		this.styleClass = styleClass;
		this.parentModuleClassName = parentModuleClassName;
		this.showCommentsForAllUsers = showCommentsForAllUsers;
	}

	@Override
	protected void initializeComponent(FacesContext context) {
		IWContext iwc = IWContext.getIWContext(context);
		
		if (!ContentUtil.hasContentEditorRoles(iwc) && !showCommentsForAllUsers) {
			return;
		}
		
		WFDivision container = new WFDivision();
		container.setId(COMMENTS_BLOCK_ID);
		container.setStyleClass(styleClass);
		
		int commentsCount = getCommentsCount(iwc);
		StringBuffer linkToAtomFeedImage = new StringBuffer(ContentUtil.getBundle().getResourcesPath());
		linkToAtomFeedImage.append("/images/feed.png");
		
		// JavaScript
		Script script = new Script();
		script.addScriptLine("setComponentCacheKey('"+cacheKey+"');");
		script.addScriptLine("setPostedLabel('"+ContentUtil.getBundle().getLocalizedString("posted")+"');");
		script.addScriptLine("setCommentsLoadingMessage('"+ContentUtil.getBundle().getLocalizedString("loading_comments")+"');");
		script.addScriptLine("setLinkToComments('"+linkToComments+"');");
		script.addScriptLine("setCommentsAtomLinkTitle('"+ContentUtil.getBundle().getLocalizedString("atom_feed")+"');");
		script.addScriptLine("setCommentsAtomsServer('"+ThemesHelper.getInstance().getFullServerName(iwc) + "/content');");
		script.addScriptLine("setLinkToAtomFeedImage('"+linkToAtomFeedImage.toString()+"');");
		script.addScriptLine("setAddNotificationText('"+ContentUtil.getBundle().getLocalizedString("need_send_notification")+"');");
		script.addScriptLine("setYesText('"+ContentUtil.getBundle().getLocalizedString("yes")+"');");
		script.addScriptLine("setNoText('"+ContentUtil.getBundle().getLocalizedString("no")+"');");
		script.addScriptLine("setEnterEmailText('"+ContentUtil.getBundle().getLocalizedString("enter_email_text")+"');");
		if (commentsCount > 0) {
			script.addScriptLine("setAddedLinkToAtomInBody(true);");
			script.addScriptLine("addAtomLinkInHeader();");
		}
		script.addScriptLine("setActiveReverseAjax();");
		if (showCommentsList) {
			script.addScriptLine("getAllArticleComments('"+linkToComments+"');");
		}
		container.add(script);
		
		// Enable comments container
		addEnableCommentsCheckboxContainer(iwc, container);
		
		// Comments label
		WFDivision articleComments = new WFDivision();
		articleComments.setId("article_comments_link_label_container");
		StringBuffer comments = new StringBuffer(ContentUtil.getBundle().getLocalizedString("comments"));
		comments.append(ContentConstants.SPACE).append("(<span id='contentItemCount' class='contentItemCountStyle'>");
		comments.append(commentsCount).append("</span>)");
		Link commentsLabel = new Link(comments.toString(), "#showCommentsList");
		commentsLabel.setOnClick("getCommentsList()");
		articleComments.add(commentsLabel);
		
		// Simple space
		Text emptyText = new Text(ContentConstants.SPACE);
		articleComments.add(emptyText);
		
		// Link - Atom feed
		if (commentsCount > 0) {
			Image atom = new Image(linkToAtomFeedImage.toString(), ContentUtil.getBundle().getLocalizedString("atom_feed"));
			Link linkToFeed = new Link();
			linkToFeed.setId("article_comments_link_to_feed");
			linkToFeed.setImage(atom);
			linkToFeed.setURL(ThemesHelper.getInstance().getFullServerName(iwc) + ContentConstants.CONTENT + linkToComments);
			articleComments.add(linkToFeed);
		}
		container.add(articleComments);
		
		// Add comment block
		container.add(getAddCommentBlock(iwc));
		
		this.add(container);
	}
	
	private void addEnableCommentsCheckboxContainer(IWContext iwc, WFDivision container) {
		if (iwc == null || container == null) {
			return;
		}
		if (!ContentUtil.hasContentEditorRoles(iwc)) {
			return;
		}
		String pageKey = getThisPageKey(iwc);
		if (pageKey == null) {
			return;
		}
		BuilderService service = null;
		try {
			service = BuilderServiceFactory.getBuilderService(iwc);
		} catch (RemoteException e) {
			e.printStackTrace();
			return;
		}
		List<String> moduleIds = service.getModuleId(pageKey, parentModuleClassName);
		if (moduleIds == null) {
			return;
		}
		if (moduleIds.size() == 0) {
			return;
		}
		WFDivision showCommentsContainer = new WFDivision();
		CheckBox enableCheckBox = new CheckBox("enableComments");
		enableCheckBox.setOnClick("enableComments(this.checked, '"+pageKey+"', '"+moduleIds.get(0)+"', 'showCommentsForAllUsers');");
		enableCheckBox.setChecked(isShowCommentsForAllUsers());
		Text enableText = new Text(ContentUtil.getBundle().getLocalizedString("enable_comments"));
		showCommentsContainer.add(enableText);
		showCommentsContainer.add(enableCheckBox);
		container.add(showCommentsContainer);
	}
	
	private int getCommentsCount(IWContext iwc) {
		CommentsEngine comments = null;
		try {
			comments = (CommentsEngine) IBOLookup.getServiceInstance(iwc, CommentsEngine.class);
		} catch (IBOLookupException e) {
			e.printStackTrace();
			return 0;
		}
		try {
			return comments.getCommentsCount(linkToComments);
		} catch (RemoteException e) {
			e.printStackTrace();
			return 0;
		}
	}
	
	private UIComponent getAddCommentBlock(IWContext iwc) {
		WFDivision addComments = new WFDivision();
		addComments.setId("add_comment_block");
		Link l = new Link(ContentUtil.getBundle().getLocalizedString("add_your_comment"), "#" + addComments.getId());
		String user = ContentUtil.getBundle().getLocalizedString("principal_name");
		String subject = ContentUtil.getBundle().getLocalizedString("subject");
		String comment = ContentUtil.getBundle().getLocalizedString("comment");
		String posted = ContentUtil.getBundle().getLocalizedString("posted");
		String send = ContentUtil.getBundle().getLocalizedString("send");
		String sending = ContentUtil.getBundle().getLocalizedString("sending");
		String separator = "', '";
		String loggedUser = null;
		try {
			loggedUser = iwc.getCurrentUser().getName();
		} catch (NotLoggedOnException e) {
			loggedUser = ContentUtil.getBundle().getLocalizedString("anonymous");
		}
		StringBuffer action = new StringBuffer("addCommentPanel('").append(addComments.getId()).append(separator);
		action.append(linkToComments).append(separator).append(user).append(separator).append(subject).append(separator);
		action.append(comment).append(separator).append(posted).append(separator).append(send).append(separator);
		action.append(sending).append(separator).append(loggedUser).append(separator);
		action.append(ContentUtil.getBundle().getLocalizedString("email")).append(separator);
		action.append(ContentUtil.getBundle().getLocalizedString("comment_form")).append("',").append(isForumPage).append(")");
		l.setOnClick(action.toString());
		addComments.add(l);
		return addComments;
	}
	
	public Object saveState(FacesContext context) {
		Object values[] = new Object[8];
		values[0] = super.saveState(context);
		values[1] = linkToComments;
		values[2] = styleClass;
		values[3] = showCommentsList;
		values[4] = isForumPage;
		values[5] = cacheKey;
		values[6] = parentModuleClassName;
		values[7] = showCommentsForAllUsers;
		return values;
	}

	public void restoreState(FacesContext context, Object state) {
		Object values[] = (Object[]) state;
		super.restoreState(context, values[0]);
		linkToComments = values[1].toString();
		styleClass = values[2].toString();
		showCommentsList = (Boolean) values[3];
		isForumPage = (Boolean) values[4];
		cacheKey = values[5].toString();
		parentModuleClassName = values[6].toString();
		showCommentsForAllUsers = (Boolean) values[7];
	}

	public String getLinkToComments() {
		return linkToComments;
	}

	public void setLinkToComments(String linkToComments) {
		this.linkToComments = linkToComments;
	}

	public String getStyleClass() {
		return styleClass;
	}

	public void setStyleClass(String styleClass) {
		this.styleClass = styleClass;
	}

	public boolean isForumPage() {
		return isForumPage;
	}

	public void setForumPage(boolean isForumPage) {
		this.isForumPage = isForumPage;
	}

	public boolean isShowCommentsList() {
		return showCommentsList;
	}

	public void setShowCommentsList(boolean showCommentsList) {
		this.showCommentsList = showCommentsList;
	}
	
	private String getThisPageKey(IWContext iwc) {
		if (iwc == null) {
			return null;
		}
		int id = iwc.getCurrentIBPageID();
		String pageKey = null;
		try {
			pageKey = String.valueOf(id);
		} catch (NumberFormatException e) {
			e.printStackTrace();
			return null;
		}
		return pageKey;
	}

	public String getParentModuleClassName() {
		return parentModuleClassName;
	}

	public void setParentModuleClassName(String parentModuleClassName) {
		this.parentModuleClassName = parentModuleClassName;
	}

	public boolean isShowCommentsForAllUsers() {
		return showCommentsForAllUsers;
	}

	public void setShowCommentsForAllUsers(boolean showCommentsForAllUsers) {
		this.showCommentsForAllUsers = showCommentsForAllUsers;
	}
	
}