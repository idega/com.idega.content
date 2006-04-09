/*
 * $Id: ContentItemCaseBean.java,v 1.2 2006/04/09 12:01:55 laddi Exp $
 *
 * Copyright (C) 2004 Idega. All Rights Reserved.
 *
 * This software is the proprietary information of Idega.
 * Use is subject to license terms.
 *
 */
package com.idega.content.bean;

import java.io.Serializable;
import java.sql.Timestamp;
import java.util.Date;

/**
 * Bean for idegaWeb content item cases.   
 * <p>
 * Last modified: $Date: 2006/04/09 12:01:55 $ by $Author: laddi $
 *
 * @author Anders Lindman
 * @version $Revision: 1.2 $
 */

public class ContentItemCaseBean implements Serializable, ContentItemCase {
	
	private int _caseId = 0;
	private String _caseCode = null;
	private String _caseStatus = null;
	private Date _createdTimestamp = null;
	private Date _publishedFromDate = null;
	private Date _publishedToDate = null;
	private int _versionedContentItemId = 0;
	
	/**
	 * Default constructor.
	 */
	public ContentItemCaseBean() {}
	
	/**
	 * Constructs a new content item case bean with the specified parameters. 
	 */
	public ContentItemCaseBean(
			int caseId,
			String caseStatus,
			Timestamp createdTimestamp,
			Timestamp publishedFromDate,
			Timestamp publishedToDate,
			int versionedContentItemId) {
		this._caseId = caseId;
		this._caseCode = CASE_CODE;
		this._caseStatus = caseStatus;
		this._createdTimestamp = createdTimestamp;
		this._publishedFromDate = publishedFromDate;
		this._publishedToDate = publishedToDate;
		this._versionedContentItemId = versionedContentItemId;
	}
		
	public int getCaseId() { return this._caseId; }
	public String getCaseCode() { return this._caseCode; }
	public String getCaseStatus() { return this._caseStatus; }
	public Date getCreatedTimestamp() { return this._createdTimestamp; }
	public Date getPublishedFromDate() { return this._publishedFromDate; }
	public Date getPublishedToDate() { return this._publishedToDate; }
	public int getVersionedContentItemId() { return this._versionedContentItemId; }

	public void setCaseId(int id) { this._caseId = id; } 
	public void setCaseCode(String s) { this._caseCode = s; }
	public void setCaseStatus(String s) { this._caseStatus = s; }
	public void setCreatedTimestamp(Date d) { this._createdTimestamp = d; }
	public void setPublishedFromDate(Date d) { this._publishedFromDate = d; }
	public void setPublishedToDate(Date d) { this._publishedToDate = d; }
}
