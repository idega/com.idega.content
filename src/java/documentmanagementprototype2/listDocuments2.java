/*
 * listDocuments2.java
 * 
 * Created on 14. oktÃƒÂ¯Ã‚Â¿Ã‚Â½ber 2004, 10:40 Copyright Roar
 */
package documentmanagementprototype2;

import java.io.IOException;

import javax.faces.FacesException;
import javax.faces.component.UIColumn;
import javax.faces.component.html.HtmlCommandButton;
import javax.faces.component.html.HtmlDataTable;
import javax.faces.component.html.HtmlForm;
import javax.faces.component.html.HtmlOutputText;
import javax.faces.component.html.HtmlPanelGrid;

import org.apache.commons.httpclient.HttpException;
import org.apache.commons.httpclient.HttpURL;
import org.apache.webdav.lib.WebdavResource;

import com.idega.business.IBOLookup;
import com.idega.idegaweb.IWUserContext;
import com.idega.presentation.IWContext;
import com.idega.slide.business.IWSlideService;

public class listDocuments2 extends AbstractSessionBean {
	// <editor-fold defaultstate="collapsed" desc="Creator-managed Component
	// Definition">

	private int __placeholder;

	private HtmlForm form1 = new HtmlForm();

	public HtmlForm getForm1() {
		return form1;
	}

	public void setForm1(HtmlForm hf) {
		this.form1 = hf;
	}

	private HtmlDataTable dataTable1 = new HtmlDataTable();

	public HtmlDataTable getDataTable1() {
		return dataTable1;
	}

	public void setDataTable1(HtmlDataTable hdt) {
		this.dataTable1 = hdt;
	}

	private UIColumn column1 = new UIColumn();

	public UIColumn getColumn1() {
		return column1;
	}

	public void setColumn1(UIColumn uic) {
		this.column1 = uic;
	}

	private HtmlOutputText outputText1 = new HtmlOutputText();

	public HtmlOutputText getOutputText1() {
		return outputText1;
	}

	public void setOutputText1(HtmlOutputText hot) {
		this.outputText1 = hot;
	}

	private HtmlOutputText outputText2 = new HtmlOutputText();

	public HtmlOutputText getOutputText2() {
		return outputText2;
	}

	public void setOutputText2(HtmlOutputText hot) {
		this.outputText2 = hot;
	}

	private UIColumn column2 = new UIColumn();

	public UIColumn getColumn2() {
		return column2;
	}

	public void setColumn2(UIColumn uic) {
		this.column2 = uic;
	}

	private HtmlOutputText outputText3 = new HtmlOutputText();

	public HtmlOutputText getOutputText3() {
		return outputText3;
	}

	public void setOutputText3(HtmlOutputText hot) {
		this.outputText3 = hot;
	}

	private HtmlOutputText outputText4 = new HtmlOutputText();

	public HtmlOutputText getOutputText4() {
		return outputText4;
	}

	public void setOutputText4(HtmlOutputText hot) {
		this.outputText4 = hot;
	}

	/*
	 * private RowSetDataModel dataTable1Model = new RowSetDataModel();
	 * 
	 * public RowSetDataModel getDataTable1Model() { return dataTable1Model; }
	 * 
	 * public void setDataTable1Model(RowSetDataModel rsdm) {
	 * this.dataTable1Model = rsdm; }
	 */
	private javax.faces.model.ArrayDataModel dataTable1Model = new javax.faces.model.ArrayDataModel();

	public javax.faces.model.ArrayDataModel getDataTable1Model() {
		return dataTable1Model;
	}

	public void setDataTable1Model(javax.faces.model.ArrayDataModel dm) {

		this.dataTable1Model = dm;
	}

	private UIColumn column3 = new UIColumn();

	public UIColumn getColumn3() {
		return column3;
	}

	public void setColumn3(UIColumn uic) {
		this.column3 = uic;
	}

	private HtmlOutputText outputText5 = new HtmlOutputText();

	public HtmlOutputText getOutputText5() {
		return outputText5;
	}

	public void setOutputText5(HtmlOutputText hot) {
		this.outputText5 = hot;
	}

	private HtmlOutputText outputText6 = new HtmlOutputText();

	public HtmlOutputText getOutputText6() {
		return outputText6;
	}

	public void setOutputText6(HtmlOutputText hot) {
		this.outputText6 = hot;
	}

	private UIColumn column4 = new UIColumn();

	public UIColumn getColumn4() {
		return column4;
	}

	public void setColumn4(UIColumn uic) {
		this.column4 = uic;
	}

	private HtmlOutputText outputText7 = new HtmlOutputText();

	public HtmlOutputText getOutputText7() {
		return outputText7;
	}

	public void setOutputText7(HtmlOutputText hot) {
		this.outputText7 = hot;
	}

	private HtmlOutputText outputText8 = new HtmlOutputText();

	public HtmlOutputText getOutputText8() {
		return outputText8;
	}

	public void setOutputText8(HtmlOutputText hot) {
		this.outputText8 = hot;
	}

	private UIColumn column5 = new UIColumn();

	public UIColumn getColumn5() {
		return column5;
	}

	public void setColumn5(UIColumn uic) {
		this.column5 = uic;
	}

	private HtmlOutputText outputText9 = new HtmlOutputText();

	public HtmlOutputText getOutputText9() {
		return outputText9;
	}

	public void setOutputText9(HtmlOutputText hot) {
		this.outputText9 = hot;
	}

	private HtmlOutputText outputText10 = new HtmlOutputText();

	public HtmlOutputText getOutputText10() {
		return outputText10;
	}

	public void setOutputText10(HtmlOutputText hot) {
		this.outputText10 = hot;
	}

	private HtmlPanelGrid gridPanel1 = new HtmlPanelGrid();

	public HtmlPanelGrid getGridPanel1() {
		return gridPanel1;
	}

	public void setGridPanel1(HtmlPanelGrid hpg) {
		this.gridPanel1 = hpg;
	}

	private HtmlCommandButton dataTable1HeaderFirstButton = new HtmlCommandButton();

	public HtmlCommandButton getDataTable1HeaderFirstButton() {
		return dataTable1HeaderFirstButton;
	}

	public void setDataTable1HeaderFirstButton(HtmlCommandButton hcb) {
		this.dataTable1HeaderFirstButton = hcb;
	}

	private HtmlCommandButton dataTable1HeaderPreviousButton = new HtmlCommandButton();

	public HtmlCommandButton getDataTable1HeaderPreviousButton() {
		return dataTable1HeaderPreviousButton;
	}

	public void setDataTable1HeaderPreviousButton(HtmlCommandButton hcb) {
		this.dataTable1HeaderPreviousButton = hcb;
	}

	private HtmlCommandButton dataTable1HeaderNextButton = new HtmlCommandButton();

	public HtmlCommandButton getDataTable1HeaderNextButton() {
		return dataTable1HeaderNextButton;
	}

	public void setDataTable1HeaderNextButton(HtmlCommandButton hcb) {
		this.dataTable1HeaderNextButton = hcb;
	}

	private HtmlCommandButton dataTable1HeaderLastButton = new HtmlCommandButton();

	public HtmlCommandButton getDataTable1HeaderLastButton() {
		return dataTable1HeaderLastButton;
	}

	public void setDataTable1HeaderLastButton(HtmlCommandButton hcb) {
		this.dataTable1HeaderLastButton = hcb;
	}

	// </editor-fold>
	public listDocuments2() {
		beforeRenderResponse();
		// <editor-fold defaultstate="collapsed" desc="Creator-managed Component
		// Initialization">
		try {
		} catch (Exception e) {
			log("listDocuments2 Initialization Failure", e);
			throw e instanceof javax.faces.FacesException ? (FacesException) e
					: new FacesException(e);
		}
		// <editor-fold defaultstate="collapsed" desc="Creator-managed Component
		// Initialization">

		// <editor-fold defaultstate="collapsed" desc="Creator-managed Component
		// Initialization">
		/*
		 * try { personRowSet.setDataSourceName("java:comp/env/jdbc/Travel");
		 * personRowSet.setCommand("SELECT * FROM TRAVEL.PERSON");
		 * dataTable1Model.setDataCacheKey("com.sun.datacache.listDocuments2.personRowSet");
		 * dataTable1Model.setRowSet(personRowSet);
		 * dataTable1Model.setSchemaName("TRAVEL");
		 * dataTable1Model.setTableName("PERSON"); } catch (Exception e) {
		 * log("listDocuments2 Initialization Failure", e); throw e instanceof
		 * javax.faces.FacesException ? (FacesException) e : new
		 * FacesException(e); }
		 */
		// </editor-fold>
		// Additional user provided initialization code
		// </editor-fold>
		// Additional user provided initialization code
		// </editor-fold>
		// Additional user provided initialization code
	}

	protected void beforeRenderResponse() {
		super.beforeRenderResponse();
		try {
			dataTable1Model.setWrappedData(getDavData());
		} catch (Exception e) {
			log("listDocuments2 Initialization Failure", e);
			throw e instanceof javax.faces.FacesException ? (FacesException) e
					: new FacesException(e);
		}

	}

	protected documentmanagementprototype2.WebDavTreeBean getWebDavTreeBean() {
		return (documentmanagementprototype2.WebDavTreeBean) getBean("WebDavTreeBean");
	}

	//    protected documentmanagementprototype2.ApplicationBean1
	// getApplicationBean1() {
	//        return
	// (documentmanagementprototype2.ApplicationBean1)getBean("ApplicationBean1");
	//    }

	protected documentmanagementprototype2.SessionBean1 getSessionBean1() {
		return (documentmanagementprototype2.SessionBean1) getBean("SessionBean1");
	}

	/**
	 * Bean cleanup.
	 */
	protected void afterRenderResponse() {
		//        personRowSet.close();
	}

	private WebDAVBean[] getDavData() {
		WebDAVBean[] data;
		try {
//			String uri = (String) getApplication().createMethodBinding(
//					"#{WebDavTree.getUri}", null)
//					.invoke(
//							javax.faces.context.FacesContext
//									.getCurrentInstance(), null);
//			if (uri == null) {
				//uri = "http://localhost:8082/slide/files";
//				IWUserContext iwuc = IWContext.getInstance();
//				ICFile2Slide slide = new ICFile2Slide(iwuc.getApplicationContext());
//				uri = slide.getWebdavServletURL(iwuc);
				
//				IWSlideService ss = (IWSlideService) IBOLookup.getServiceInstance(iwuc.getApplicationContext(), IWSlideService.class);
//				ss.getWebdavServerURL();
				
			//}
			IWUserContext iwuc = IWContext.getInstance();			
			IWSlideService ss = (IWSlideService) IBOLookup.getServiceInstance(iwuc.getApplicationContext(), IWSlideService.class);
			HttpURL homeUrl = ss.getWebdavServerURL();
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
		//        return new java.util.ArrayList(java.util.Arrays.asList(data));
	}

	private WebDAVBean[] getFileNameList(WebdavResource resource) {
		String[] d = resource.list();
		WebDAVBean[] data = new WebDAVBean[d.length];
		for (int i = 0; i < d.length; i++) {
			data[i] = new WebDAVBean(d[i]);
		}
		return data;
	}

	private WebDAVBean[] getDirectoryListing(WebdavResource resource)
			throws IOException, HttpException {
		WebdavResource[] resources = resource.listWebdavResources();
		WebDAVBean[] data = new WebDAVBean[resources.length];
		for (int i = 0; i < resources.length; i++) {
			data[i] = new WebDAVBean(resources[i].getDisplayName(),
					resources[i].isCollection(), resources[i]
							.getGetContentLength(), resources[i]
							.getGetLastModified(), resources[i]
							.getGetContentType());

		}
		return data;
	}

	public String dataTable1_firstPageAction() {
		dataTable1.setFirst(0);
		return null;
	}

	public String dataTable1_previousPageAction() {
		int first = dataTable1.getFirst() - dataTable1.getRows();
		if (first < 0) {
			first = 0;
		}
		dataTable1.setFirst(first);
		return null;
	}

	public String dataTable1_nextPageAction() {
		int first = dataTable1.getFirst() + dataTable1.getRows();
		dataTable1.setRowIndex(first);
		if (dataTable1.isRowAvailable()) {
			dataTable1.setFirst(first);
		}
		return null;
	}

	public String dataTable1_lastPageAction() {
		int first = dataTable1.getFirst();
		while (true) {
			dataTable1.setRowIndex(first + 1);
			if (dataTable1.isRowAvailable()) {
				first++;
			} else {
				break;
			}
		}
		dataTable1.setFirst(first - (first % dataTable1.getRows()));
		return null;
	}

	public void processAction(javax.faces.event.ActionEvent actionEvent)
			throws javax.faces.event.AbortProcessingException {
		Object source = actionEvent.getSource();

		java.lang.System.out.print("Action event received");

	}
}