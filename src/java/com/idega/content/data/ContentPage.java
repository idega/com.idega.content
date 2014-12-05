package com.idega.content.data;

import java.io.Serializable;

import javax.persistence.Cacheable;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.NamedQueries;
import javax.persistence.NamedQuery;
import javax.persistence.Table;

import org.hibernate.annotations.Index;

import com.idega.util.CoreConstants;

@NamedQueries({
	@NamedQuery(name = ContentPage.GET_BY_PAGE, query = "FROM ContentPage p where p.page = :" + ContentPage.PAGE_PROP + " ORDER BY p.position"),
	@NamedQuery(name = ContentPage.GET_BY_ID, query = "FROM ContentPage p where p.id = :" + ContentPage.ID_PROP),
	@NamedQuery(name = ContentPage.GET_LAST_POSITION, query = "SELECT max(p.position) FROM ContentPage p where p.page = :" + ContentPage.PAGE_PROP)
})
@Entity
@Table(name = "content_page")
@Cacheable
public class ContentPage implements Serializable {

	private static final long serialVersionUID = -8371097366822539511L;

	private static final String QUERY_PREFIX = "ContentPageEntity_";
	public static final String GET_BY_PAGE = QUERY_PREFIX + "GET_BY_PAGE";
	public static final String GET_BY_ID = QUERY_PREFIX + "GET_BY_ID";
	public static final String GET_LAST_POSITION = QUERY_PREFIX + "GET_LAST_POSITION";

	public static final String ID_PROP = "id";
	public static final String BODY_PROP = "body";
	public static final String HEADLINE_PROP = "headline";
	public static final String IS_URI_PROP = "isUri";
	public static final String POSITION_PROP = "uri";
	public static final String RESOURCES_PROP = "resources";
	public static final String PAGE_PROP = "page";

	@Id
	@GeneratedValue(strategy = GenerationType.AUTO)
	private Long id;

	@Column(name = "body", nullable = false, length = 1024 * 1024)
	private String body = CoreConstants.EMPTY;

	@Column(name = "headline", nullable = false)
	private String headline = CoreConstants.EMPTY;

	@Column(name = "is_uri", nullable = false)
	private Boolean uri;

	@Column(name = "position", nullable = false)
	private int position;

	@Index(name = "ContentPage_page_index")
	@Column(name = "page", nullable = false)
	private int page;

	@Column(name = "resources")
	private String resources;

	public Long getId() {
		return id;
	}

	public void setId(Long id) {
		this.id = id;
	}

	public String getBody() {
		return body;
	}

	public void setBody(String body) {
		this.body = body;
	}

	public String getHeadline() {
		return headline;
	}

	public void setHeadline(String headline) {
		this.headline = headline;
	}

	public Boolean getIsUri() {
		return uri == null ? Boolean.FALSE : uri;
	}

	public void setIsUri(Boolean uri) {
		this.uri = uri;
	}

	public int getPosition() {
		return position;
	}

	public void setPosition(int position) {
		this.position = position;
	}

	public int getPage() {
		return page;
	}

	public void setPage(int page) {
		this.page = page;
	}

	public String getResources() {
		return resources;
	}

	public void setResources(String resources) {
		this.resources = resources;
	}

}