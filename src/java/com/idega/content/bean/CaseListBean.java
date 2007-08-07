/*
 * $Id: CaseListBean.java,v 1.7 2007/08/07 08:33:00 valdas Exp $
 *
 * Copyright (C) 2004 Idega. All Rights Reserved.
 *
 * This software is the proprietary information of Idega.
 * Use is subject to license terms.
 *
 */
package com.idega.content.bean;

import java.io.Serializable;
import java.util.Date;

import javax.faces.component.UIColumn;
import javax.faces.component.html.HtmlCommandLink;
import javax.faces.component.html.HtmlOutputText;
import javax.faces.event.ActionListener;
import javax.faces.model.DataModel;

import com.idega.content.business.ContentConstants;
import com.idega.webface.WFUtil;
import com.idega.webface.bean.WFListBean;
import com.idega.webface.model.WFDataModel;

/**
 * Bean for content item case list rows.   
 * <p>
 * Last modified: $Date: 2007/08/07 08:33:00 $ by $Author: valdas $
 *
 * @author Anders Lindman
 * @version $Revision: 1.7 $
 */

public class CaseListBean implements WFListBean, Serializable {

	public final static String CASE_ID = "case_id";
	
	private WFDataModel _dataModel = null;
	private ActionListener _caseLinkListener = null;

	private String _id = null;
	private String _description = null;
	private Date _created = null;
	private Date _lastModified = null;
	private String _author = null;
	private String _status = null;
	
	private String[] testColumnHeaders = {
		"description", 
		"created", 
		"last_modified",
		"author",
		"status"
	};				
	
	/**
	 * Default constructor.
	 */
	public CaseListBean() {}

	/**
	 * Constructs a new case list bean with the specified case link listener.
	 */
	public CaseListBean(ActionListener l) {
		setCaseLinkListener(l);
	}
	
	/**
	 * Constructs a new case list bean with the specified parameters. 
	 */
	public CaseListBean(String id, String description, Date created, Date lastModified, String author, String status) {
		this._id = id;
		this._description = description;
		this._created = created;
		this._lastModified = lastModified;
		this._author = author;
		this._status = status;
	}
		
	public String getId() { return this._id; }
	public String getDescription() { return this._description; }
	public Date getCreated() { return this._created; }
	public Date getLastModified() { return this._lastModified; }
	public String getAuthor() { return this._author; }
	public String getStatus() { return this._status; }

	public void setId(String s) { this._id = s; }
	public void setDescription(String s) { this._description = s; }
	public void setCreated(Date d) { this._created = d; }
	public void setLastModified(Date d) { this._lastModified = d; }
	public void setAuthor(String s) { this._author = s; }
	public void setStatus(String s) { this._status = s; }
	
	public ActionListener getCaseLinkListener() { return this._caseLinkListener; }
	public void setCaseLinkListener(ActionListener l) { this._caseLinkListener = l; }
	
	/**
	 * @see com.idega.webface.bean.WFListBean#updateDataModel() 
	 */
	public void updateDataModel(Integer start, Integer rows) {
		if (this._dataModel == null) {
			this._dataModel = new WFDataModel();
		}
		int availableRows=0;
		/*int availableRows = testDescriptions.length;
		int nrOfRows = rows.intValue();
		if (nrOfRows == 0) {
			nrOfRows = availableRows;
		}
		int maxRow = start.intValue() + nrOfRows;
		if (maxRow > availableRows) {
			maxRow = availableRows;
		}
		for (int i = start.intValue(); i < maxRow; i++) {
			CaseListBean c = new CaseListBean(String.valueOf(i), testDescriptions[i], testCreated[i], testLastModified[i], testAuthors[i], testStatus[i]);
			_dataModel.set(c, i);
		}*/
		this._dataModel.setRowCount(availableRows);
	}
	
	/**
	 * @see com.idega.webface.bean.WFListBean#createColumns() 
	 */
	public UIColumn[] createColumns(String var) {
		int cols = this.testColumnHeaders.length;
		UIColumn[] columns = new UIColumn[cols];

		for (int i = 0; i < cols; i++) {
			UIColumn c = new UIColumn();
			c.setHeader(WFUtil.getTextVB(ContentConstants.IW_BUNDLE_IDENTIFIER, this.testColumnHeaders[i]));
			columns[i] = c;
		}
		
		HtmlCommandLink l = WFUtil.getListLinkVB(var + ".description");
		l.setId(CASE_ID);
		l.addActionListener(this._caseLinkListener);
		WFUtil.addParameterVB(l, "id", var + ".id");
		columns[0].getChildren().add(l);
		HtmlOutputText t = WFUtil.getListTextVB(var + ".created");
		columns[1].getChildren().add(t);
		t = WFUtil.getListTextVB(var + ".lastModified");
		columns[2].getChildren().add(t);
		t = WFUtil.getListTextVB(var + ".author");
		columns[3].getChildren().add(t);
		t = WFUtil.getListTextVB(var + ".status");
		columns[4].getChildren().add(t);		
		
		return columns;
	}
	
	/**
	 * @see com.idega.webface.bean.WFListBean#getDataModel() 
	 */
	public DataModel getDataModel() {
		return this._dataModel;
	}
	
	/**
	 * @see com.idega.webface.bean.WFListBean#setDataModel() 
	 */
	public void setDataModel(DataModel dataModel) {
		this._dataModel = (WFDataModel) dataModel;
	}
}
