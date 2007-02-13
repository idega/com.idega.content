package com.idega.content.presentation;

import java.util.List;

import javax.faces.component.UIComponent;
import javax.faces.context.FacesContext;

import org.apache.myfaces.custom.htmlTag.HtmlTag;

import com.idega.block.rss.business.RSSBusiness;
import com.idega.business.IBOLookup;
import com.idega.business.IBOLookupException;
import com.idega.content.business.ContentConstants;
import com.idega.content.business.ContentUtil;
import com.idega.content.themes.helpers.ThemesHelper;
import com.idega.core.accesscontrol.business.NotLoggedOnException;
import com.idega.presentation.IWContext;
import com.idega.presentation.Span;
import com.idega.presentation.text.Link;
import com.idega.presentation.text.ListItem;
import com.idega.presentation.text.Lists;
import com.idega.presentation.text.Paragraph;
import com.idega.presentation.text.Text;
import com.idega.webface.WFDivision;
import com.sun.syndication.feed.synd.SyndContent;
import com.sun.syndication.feed.synd.SyndEntry;
import com.sun.syndication.feed.synd.SyndFeed;

public class ContentItemComments extends ContentBlock {
	
	private String styleClass = "content_item_comments_style";
	private String linkToComments = null;
	
	private boolean existCommentsFile = false;
	
	protected static final String COMMENTS_BLOCK_ID = "comments_block";
	private static final String COMMENTS_BLOCK_LIST_ID = "comments_block_list";
	
	private static final String DL_VALUE = "dl";
	private static final String DT_VALUE = "dt";
	private static final String DD_VALUE = "dd";
	
	private static final String ZERO_COMMENTS = "0";
	
	public ContentItemComments(String linkToComments) {
		super();
		this.linkToComments = linkToComments;
	}
	
	public ContentItemComments(String linkToComments, String styleClass) {
		super();
		this.linkToComments = linkToComments;
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
		Text commentsLabel = new Text(comments.toString());
		container.add(commentsLabel);
		
		// Add comment block
		container.add(getAddCommentBlock(iwc));
		
		// Comments list
		container.add(getCommentsList(commentsFeed));
		
		this.add(container);
	}
	
	private UIComponent getCommentsList(SyndFeed comments) {
		Lists container = new Lists();
		container.setListOrdered(true);
		container.setId(COMMENTS_BLOCK_LIST_ID);
		if (comments == null) {
			return container;
		}
		List entries = comments.getEntries();
		if (entries == null) {
			return container;
		}
		
		Object o = null;
		SyndEntry entry = null;
		ListItem item = null;
		HtmlTag dlTag = null;
		HtmlTag dtTag = null;
		HtmlTag ddTag = null;
		Span s = null;
		Paragraph p = null;
		for (int i = 0; i < entries.size(); i++) {
			o = entries.get(i);
			if (o instanceof SyndEntry) {
				item = new ListItem();
				
				dlTag = new HtmlTag();
				dlTag.setValue(DL_VALUE);
				entry = (SyndEntry) o;
				
				// Author
				dtTag = new HtmlTag();
				dtTag.setValue(DT_VALUE);
				s = new Span();
				s.add(new Text(entry.getAuthor()));
				dtTag.getChildren().add(s);
				dlTag.getChildren().add(dtTag);
				
				// Subject
				ddTag = new HtmlTag();
				ddTag.setValue(DD_VALUE);
				s = new Span();
				s.add(new Text(entry.getTitle()));
				ddTag.getChildren().add(s);
				dlTag.getChildren().add(ddTag);
				
				// Comment
				ddTag = new HtmlTag();
				ddTag.setValue(DD_VALUE);
				p = new Paragraph();
				SyndContent content = (SyndContent) entry.getContents().get(0);
				p.add(new Text(content.getValue()));
				ddTag.getChildren().add(p);
				dlTag.getChildren().add(ddTag);
				
				// Posted
				ddTag = new HtmlTag();
				ddTag.setValue(DD_VALUE);
				s = new Span();
				s.add(new Text("Posted: " + comments.getPublishedDate().toString()));
				ddTag.getChildren().add(s);
				dlTag.getChildren().add(ddTag);
				
				item.add(dlTag);
				container.add(item);
			}
		}
		
		return container;
	}
	
	private SyndFeed getCommentsFeed(IWContext iwc) {
		if (linkToComments == null) {
			return null;
		}
		ThemesHelper helper = ThemesHelper.getInstance();
		if (!helper.existFileInSlide(linkToComments)) {
			return null;
		}
		existCommentsFile = true;
		
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
		action.append(sending).append(separator).append(loggedUser).append(separator).append(existCommentsFile).append("')");
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
