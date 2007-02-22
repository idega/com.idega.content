package com.idega.content.presentation;

import java.rmi.RemoteException;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;

import com.idega.business.IBOLookup;
import com.idega.business.IBOLookupException;
import com.idega.content.business.CommentsEngine;
import com.idega.content.business.ContentConstants;
import com.idega.content.business.ContentUtil;
import com.idega.content.themes.helpers.ThemesHelper;
import com.idega.core.accesscontrol.business.NotLoggedOnException;
import com.idega.presentation.IWContext;
import com.idega.presentation.Script;
import com.idega.presentation.text.Link;
import com.idega.webface.WFDivision;

public class ContentItemComments extends ContentBlock {
	
	private String styleClass = "content_item_comments_style";
	private String linkToComments = null;
	
	private boolean showCommentsList = false;
	private boolean isForumPage = false;
	
	private static final String COMMENTS_BLOCK_ID = "comments_block";
	
	public ContentItemComments(String linkToComments, boolean showCommentsList) {
		this(linkToComments, showCommentsList, false, "content_item_comments_style");
	}
	
	public ContentItemComments(String linkToComments, boolean showCommentsList, boolean isForumPage) {
		this(linkToComments, showCommentsList, isForumPage, "content_item_comments_style");
	}
	
	public ContentItemComments(String linkToComments, boolean showCommentsList, boolean isForumPage, String styleClass) {
		super();
		this.linkToComments = linkToComments;
		this.showCommentsList = showCommentsList;
		this.isForumPage = isForumPage;
		this.styleClass = styleClass;
	}

	@Override
	protected void initializeComponent(FacesContext context) {
		IWContext iwc = IWContext.getIWContext(context);
		
		WFDivision container = new WFDivision();
		container.setId(COMMENTS_BLOCK_ID);
		container.setStyleClass(styleClass);
		
		// Comments label
		int commentsCount = getCommentsCount(iwc);
		StringBuffer comments = new StringBuffer(ContentUtil.getBundle().getLocalizedString("comments"));
		comments.append(ContentConstants.SPACE).append("(<span id='contentItemCount' class='contentItemCountStyle'>");
		comments.append(commentsCount).append("</span>)");
		Link commentsLabel = new Link(comments.toString(), "#showCommentsList");
		commentsLabel.setOnClick("getCommentsList()");
		container.add(commentsLabel);
		
		// Add comment block
		container.add(getAddCommentBlock(iwc));
		
		// Comments list will be generated with DWR & JavaScript
		Script script = new Script();
		script.addScriptLine("setPostedLabel('"+ContentUtil.getBundle().getLocalizedString("posted")+"');");
		script.addScriptLine("setCommentsLoadingMessage('"+ContentUtil.getBundle().getLocalizedString("loading_comments")+"');");
		script.addScriptLine("setLinkToComments('"+linkToComments+"');");
		script.addScriptLine("setActiveReverseAjax();");
		script.addScriptLine("setCommentsAtomLinkTitle('"+ContentUtil.getBundle().getLocalizedString("atom_feed")+"');");
		script.addScriptLine("setCommentsAtomsServer('"+ThemesHelper.getInstance().getFullServerName(iwc) + "/content');");
		if (commentsCount > 0) {
			script.addScriptLine("addAtomLinkForComments();");
		}
		script.addScriptLine("getCommentsCount();");
		if (showCommentsList) {
			script.addScriptLine("getAllArticleComments('"+linkToComments+"');");
		}
		
		container.add(script);
		
		this.add(container);
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
		Object values[] = new Object[5];
		values[0] = super.saveState(context);
		values[1] = linkToComments;
		values[2] = styleClass;
		values[3] = showCommentsList;
		values[4] = isForumPage;
		return values;
	}

	public void restoreState(FacesContext context, Object state) {
		Object values[] = (Object[]) state;
		super.restoreState(context, values[0]);
		linkToComments = values[1].toString();
		styleClass = values[2].toString();
		showCommentsList = (Boolean) values[3];
		isForumPage = (Boolean) values[4];
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
	
}