package com.idega.content.presentation;

import java.io.IOException;
import java.util.Vector;

import javax.faces.component.UIColumn;
import javax.faces.component.html.HtmlCommandLink;
import javax.faces.event.AbortProcessingException;
import javax.faces.event.ActionEvent;
import javax.faces.event.FacesListener;
import javax.faces.model.DataModel;

import org.apache.commons.httpclient.HttpException;
import org.apache.commons.httpclient.HttpURL;
import org.apache.webdav.lib.WebdavResource;

import com.idega.business.IBOLookup;
import com.idega.idegaweb.IWUserContext;
import com.idega.presentation.IWContext;
import com.idega.slide.business.IWSlideService;
import com.idega.webface.WFUtil;
import com.idega.webface.bean.WFListBean;
import com.idega.webface.model.WFDataModel;

import documentmanagementprototype2.WebDAVBean;

/**
 * A managed bean for the WebDAVList component
 * @author gimmi
 */
public class WebDAVListManagedBean implements WFListBean, FacesListener {
	
	public WebDAVListManagedBean() {
	}
	
	private WFDataModel dataModel = new WFDataModel();

	public DataModel getDataModel() {
		return dataModel;
	}
	
	public void setDataModel(DataModel model) {
		this.dataModel = (WFDataModel) model;
	}

	public UIColumn[] createColumns(String var) {
				
		HtmlCommandLink l = WFUtil.getLinkVB(null, var + ".name", null);
		//WFUtil.addParameterVB(l, "id", var + ".webDavUrl");
		UIColumn col = new UIColumn();
		col.setHeader(WFUtil.getText("Name"));
//		col.getChildren().add(WFUtil.getTextVB(var + ".name"));
		col.getChildren().add(l);
		
		UIColumn col2 = new UIColumn();
		col2.setHeader(WFUtil.getText("Created"));
		col2.getChildren().add(WFUtil.getTextVB(var + ".creationDate"));
		
		UIColumn col3 = new UIColumn();
		col3.setHeader(WFUtil.getText("Length"));
		col3.getChildren().add(WFUtil.getTextVB(var + ".length"));
		
		UIColumn col4 = new UIColumn();
		col4.setHeader(WFUtil.getText("Mime type"));
		col4.getChildren().add(WFUtil.getTextVB(var + ".mime"));
		
		UIColumn col5 = new UIColumn();
		col5.setHeader(WFUtil.getText("Folder"));
		col5.getChildren().add(WFUtil.getTextVB(var + ".isCollection"));
		
		UIColumn col6 = new UIColumn();
		col6.setHeader(WFUtil.getText("Last modified"));
		col6.getChildren().add(WFUtil.getTextVB(var + ".modifiedDate"));


		return new UIColumn[] { col, col2, col3, col4, col5, col6 };
	}

	/**
	 * Updates the datamodel, definded by WFList
	 * @param first Number of first element
	 * @param rows Total number of rows
	 */
	public void updateDataModel(Integer start, Integer rows) {
		if (dataModel == null) {
			dataModel = new WFDataModel();
		}

		WebDAVBean[] beans = getDavData();
		int availableRows = beans.length;
		
		int nrOfRows = rows.intValue();
		if (nrOfRows == 0) {
			nrOfRows = availableRows;
		}
		int maxRow = Math.min(start.intValue() + nrOfRows,availableRows);
		for (int i = start.intValue(); i < maxRow; i++) {
			dataModel.set(beans[i], i);
		}
		dataModel.setRowCount(availableRows);

	}	
	
	private WebDAVBean[] getDavData() {
		WebDAVBean[] data;
		try {

			HttpURL homeUrl = null;
			String webDavHttpURL = null;
			
			if (webDavHttpURL == null) {
				IWUserContext iwuc = IWContext.getInstance();			
				IWSlideService ss = (IWSlideService) IBOLookup.getServiceInstance(iwuc.getApplicationContext(), IWSlideService.class);
				homeUrl = ss.getWebdavServerURL();
			} else {
				homeUrl = new HttpURL(webDavHttpURL);
			}
			homeUrl.setUserinfo("root", "root");
			WebdavResource resource = new WebdavResource(homeUrl);
			if (resource.exists()) {
				data = getDirectoryListing(resource);

			} else {
				data = new WebDAVBean[] { new WebDAVBean(
						"Resource does not exist") };
			}
		} catch (HttpException ex) {
			data = new WebDAVBean[] { new WebDAVBean("Caught HttpException") };
		} catch (IOException ex) {
			data = new WebDAVBean[] { new WebDAVBean("Caught IOException") };
		} catch (NullPointerException ex) {
			StackTraceElement[] trace = ex.getStackTrace();
			String traceString = null;
			for (int i = 0; i < trace.length; i++) {
				traceString = traceString + trace[i].toString() + "    \n\r";
			}
			data = new WebDAVBean[] { new WebDAVBean("Nullpointer: "
					+ traceString) };
		}
		return data;
	}

	private WebDAVBean[] getDirectoryListing(WebdavResource resource)	throws IOException, HttpException {
		WebdavResource[] resources = resource.listWebdavResources();
		Vector v = new Vector();
		WebDAVBean bean;
		for (int i = 0; i < resources.length; i++) {
			if (!resources[i].getDisplayName().startsWith(".")) {
				bean = new WebDAVBean();
				bean.setName(resources[i].getDisplayName());
				bean.setIsCollection(resources[i].isCollection());
				bean.setLength(resources[i].getGetContentLength()); 
				bean.setModifiedDate(resources[i].getGetLastModified());
				bean.setMime(resources[i].getGetContentType());
				bean.setCreationDate(resources[i].getCreationDate());
				bean.setWebDavHttpURL(resources[i].getHttpURL().toString());
				System.out.println(i+" "+resources[i].getHttpURL());
//				bean.setParentList(this);
				v.add(bean);
			}
		}
		return (WebDAVBean[]) v.toArray(new WebDAVBean[]{});
	}

	public void processAction(ActionEvent actionEvent) throws AbortProcessingException {
		System.out.println("Event = "+actionEvent);
	}

}
