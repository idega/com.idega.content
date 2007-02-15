package com.idega.content.presentation;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;

import com.idega.block.rss.business.RSSBusiness;
import com.idega.business.IBOLookup;
import com.idega.business.IBOLookupException;
import com.idega.content.business.ContentConstants;
import com.idega.content.business.ContentUtil;
import com.idega.content.themes.helpers.ThemesHelper;
import com.idega.core.accesscontrol.business.NotLoggedOnException;
import com.idega.presentation.IWContext;
import com.idega.presentation.Script;
import com.idega.presentation.text.Link;
import com.idega.webface.WFDivision;
import com.sun.syndication.feed.synd.SyndFeed;

public class ContentItemComments extends ContentBlock {
	
	private String styleClass = "content_item_comments_style";
	private String linkToComments = null;
	
	private boolean showCommentsList = false;
	
	protected static final String COMMENTS_BLOCK_ID = "comments_block";
	
	private static final String ZERO_COMMENTS = "0";
	
	public ContentItemComments(String linkToComments, boolean showCommentsList) {
		super();
		this.linkToComments = linkToComments;
		this.showCommentsList = showCommentsList;
	}
	
	public ContentItemComments(String linkToComments, boolean showCommentsList, String styleClass) {
		super();
		this.linkToComments = linkToComments;
		this.showCommentsList = showCommentsList;
		this.styleClass = styleClass;
	}

	@Override
	protected void initializeComponent(FacesContext context) {
		IWContext iwc = IWContext.getIWContext(context);
		
		WFDivision container = new WFDivision();
		container.setId(COMMENTS_BLOCK_ID);
		container.setStyleClass(styleClass);
		
		SyndFeed commentsFeed = getCommentsFeed(iwc);
		
		// Comments label
		StringBuffer comments = new StringBuffer(ContentUtil.getBundle().getLocalizedString("comments"));
		comments.append(ContentConstants.SPACE).append("(<span id='contentItemCount' class='contentItemCountStyle'>");
		comments.append(getCommentsCount(commentsFeed)).append("</span>)");
		Link commentsLabel = new Link(comments.toString(), "#showCommentsList");
		commentsLabel.setOnClick("showCommentsList()");
		container.add(commentsLabel);
		
		// Add comment block
		container.add(getAddCommentBlock(iwc));
		
		// Comments list will be generated with DWR & JavaScript
		Script script = new Script();
//		script.addScriptLine("setCommentsTimeOut(60000);"); // Every minute will check for new comments
		script.addScriptLine("setPostedLabel('"+ContentUtil.getBundle().getLocalizedString("posted")+"');");
		script.addScriptLine("setCommentsLoadingMessage('"+ContentUtil.getBundle().getLocalizedString("loading_comments")+"');");
		script.addScriptLine("setLinkToComments('"+linkToComments+"');");
		if (showCommentsList) {
			script.addScriptLine("getComments('"+linkToComments+"');");
		}
		container.add(script);
		
		this.add(container);
	}
	
	private SyndFeed getCommentsFeed(IWContext iwc) {
		if (linkToComments == null) {
			return null;
		}
		ThemesHelper helper = ThemesHelper.getInstance();
		if (!helper.existFileInSlide(linkToComments)) {
			return null;
		}
		
		RSSBusiness rss = null;
		try {
			rss = (RSSBusiness) IBOLookup.getServiceInstance(iwc, RSSBusiness.class);
		} catch (IBOLookupException e) {
			e.printStackTrace();
			return null;
		}
		return rss.getFeed(helper.getFullWebRoot() + linkToComments);
	}
	
	private String getCommentsCount(SyndFeed comments) {
		if (comments == null) {
			return ZERO_COMMENTS;
		}
		if (comments.getEntries() == null) {
			return ZERO_COMMENTS;
		}
		
		return String.valueOf(comments.getEntries().size());
	}
	
	private UIComponent getAddCommentBlock(IWContext iwc) {
		WFDivision addComments = new WFDivision();
		addComments.setId("add_comment_block");
		Link l = new Link(ContentUtil.getBundle().getLocalizedString("add_your_comment"), "#" + addComments.getId());
		String user = ContentUtil.getBundle().getLocalizedString("user");
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
		action.append(sending).append(separator).append(loggedUser).append("')");
		l.setOnClick(action.toString());
		addComments.add(l);
		return addComments;
	}
	
	public Object saveState(FacesContext context) {
		Object values[] = new Object[3];
		values[0] = super.saveState(context);
		values[1] = linkToComments;
		values[2] = styleClass;
		return values;
	}

	public void restoreState(FacesContext context, Object state) {
		Object values[] = (Object[]) state;
		super.restoreState(context, values[0]);
		linkToComments = values[1].toString();
		styleClass = values[2].toString();
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

}
