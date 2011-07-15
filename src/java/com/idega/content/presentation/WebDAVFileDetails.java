/*
 * Created on 14.12.2004
 */
package com.idega.content.presentation;

import java.util.List;
import java.util.Locale;

import javax.faces.component.UIComponent;
import javax.faces.component.html.HtmlCommandButton;
import javax.faces.component.html.HtmlGraphicImage;
import javax.faces.component.html.HtmlOutputLink;
import javax.faces.component.html.HtmlOutputText;
import javax.faces.context.FacesContext;
import javax.faces.event.AbortProcessingException;
import javax.faces.event.ActionEvent;
import javax.faces.event.ActionListener;
import javax.jcr.RepositoryException;

import com.idega.content.business.ContentUtil;
import com.idega.idegaweb.IWMainApplication;
import com.idega.presentation.IWContext;
import com.idega.presentation.Table;
import com.idega.presentation.text.DownloadLink;
import com.idega.repository.bean.RepositoryItem;
import com.idega.repository.bean.RepositoryItemVersionInfo;
import com.idega.repository.jcr.JCRItem;
import com.idega.util.FileUtil;
import com.idega.util.IWTimestamp;
import com.idega.util.ListUtil;
import com.idega.util.Timer;
import com.idega.webface.WFUtil;

/**
 * @author gimmi
 */
public class WebDAVFileDetails extends ContentBlock implements ActionListener {

	private static String ACTION = "wdf_ac";
	private static String ACTION_TOGGLE_LOCK = "togLock";
	private static String ACTION_CHECK_OUT = "checkout";
	private static String ACTION_CHECK_IN = "checkin";
	private static String ACTION_UNCHECK_OUT = "uncheckout";
	private static final String PARAMETER_RESOURCE_PATH = "wfd_prp";
	private boolean detailed = true;
	private boolean useVersionControl = true;

	@Override
	protected void initializeComponent(FacesContext context) {
		JCRItem resource = getRepositoryItem();
//		String userName = null;

		if (resource == null)
			return;

//		try {
//			// Making sure all properties are set
//			resource.setProperties();
//			userName = getIWSlideSession().getUserFullName();
//		} catch(HttpException he){
//			if(he.getReasonCode()==WebdavStatus.SC_NOT_FOUND){
//				//escape out of the processing if the resource is not found
//				return;
//			} else{
//				he.printStackTrace();
//			}
//		} catch (IOException e) {
//			e.printStackTrace();
//		}

		String resourceName = resource.getName();
		int row = 1;

		Table table = new Table();
		table.setId(this.getId() + "_table");
//		table.setWidth("100%");
//		table.setBorder(1);

		HtmlOutputLink link = new HtmlOutputLink();
		link.setValue(resource.getEncodedPath());
		link.setStyleClass("wf_listlink");
		link.setId(getId() + "_dl");
		link.getChildren().add(getBundle().getLocalizedText("download_view"));

			table.add(getText("document_name"), 1, row);
			table.add(WFUtil.getText(resourceName,"wf_listtext"), 2, row);
			table.add(link, 2, ++row);
			table.add(getText("size"), 1, ++row);
			table.add(WFUtil.getText(FileUtil.getHumanReadableSize(resource.getLength()),"wf_listtext"), 2, row);
			table.add(getText("folder"), 1, ++row);
			HtmlOutputText loc = WFUtil.getTextVB("WebDAVListBean.webDAVPath");
			loc.setStyleClass("wf_listtext");
			table.add(loc, 2, row);
			table.add(getText("content_type"), 1, ++row);
			table.add(WFUtil.getText(resource.getMimeType(),"wf_listtext"), 2, row);

			Locale locale = IWContext.getInstance().getCurrentLocale();

			table.add(getText("creation_date"), 1, ++row);
			table.add(WFUtil.getText(new IWTimestamp(resource.getCreationDate()).getLocaleDateAndTime(locale, IWTimestamp.MEDIUM, IWTimestamp.MEDIUM),"wf_listtext"), 2, row);

			table.add(getText("modification_date"), 1, ++row);
			table.add(WFUtil.getText(new IWTimestamp(resource.getLastModified()).getLocaleDateAndTime(locale, IWTimestamp.MEDIUM, IWTimestamp.MEDIUM),"wf_listtext"), 2, row);
			if (this.detailed) {

				if (this.useVersionControl) {
					// Lock/Unlock
					table.add(getText("locked_unlocked"), 1, ++row);
					if (resource.isLocked()) {
						HtmlGraphicImage lock = new HtmlGraphicImage();
						lock.setUrl(IWMainApplication.getDefaultIWMainApplication().getURIFromURL(ContentUtil.getBundle().getResourcesVirtualPath())+"/images/locked.gif");
						lock.setId(this.getId()+"_lock");
						lock.setHeight("16");// sizes that make sense 16/32/64/128
						lock.setStyle("alignment:bottom");
						table.add(lock, 2, row);
						table.add(getText("locked", "wf_listtext"), 2, row);
					} else {
						table.add(getText("unlocked", "wf_listtext"), 2, row);
					}
					table.add(WFUtil.getText("  - "), 2, row);

					HtmlCommandButton lockToggler = new HtmlCommandButton();
					lockToggler.setId(getId()+"_lockTogg");
					lockToggler.getAttributes().put(PARAMETER_RESOURCE_PATH, resource.getPath());
					if (resource.isLocked()) {
						getBundle().getLocalizedUIComponent("unlock", lockToggler);
					} else {
						getBundle().getLocalizedUIComponent("lock", lockToggler);
					}

					lockToggler.setStyleClass("wf_listlink");
					lockToggler.addActionListener(WFUtil.getActionListener(context.getELContext(), "#{contentviewerbean.processAction}"));
					lockToggler.getAttributes().put(ACTION, ACTION_TOGGLE_LOCK);
					table.add(lockToggler, 2, row);
				}

				if (this.useVersionControl) {
					//Checkout/in
					table.add(getText("checkout_status"), 1, ++row);
					String checkedOut = resource.getCheckedOut();
					if (checkedOut == null || "".equals(checkedOut)) {
						table.add(getText("not_checked_out", "wf_listtext"), 2, row);
					} else {
						table.add(getText("checked_out", "wf_listtext"), 2, row);
						table.add(WFUtil.getText(" ("+checkedOut.substring(checkedOut.lastIndexOf("/")+1)+")","wf_listtext"), 2, row);
					}
					table.add(WFUtil.getText("  - "), 2, row);

					HtmlCommandButton checkOuter = new HtmlCommandButton();
					checkOuter.setId(getId()+"_check");
					checkOuter.getAttributes().put(PARAMETER_RESOURCE_PATH, resource.getPath());
					checkOuter.setStyleClass("wf_listlink");
					checkOuter.addActionListener(WFUtil.getActionListener(context.getELContext(), "#{contentviewerbean.processAction}"));

					if (checkedOut == null || "".equals(checkedOut)) {
						getBundle().getLocalizedUIComponent("check_out", checkOuter);
						checkOuter.getAttributes().put(ACTION, ACTION_CHECK_OUT);
						table.add(checkOuter, 2, row);
					} else {
						//	TODO
//						if (VersionHelper.hasUserCheckedOutResource(resource, userName)) {
//
//							getBundle().getLocalizedUIComponent("uncheck_out", checkOuter);
//							checkOuter.getAttributes().put(ACTION, ACTION_UNCHECK_OUT);
//							table.add(checkOuter, 2, row);
//
//							HtmlCommandButton checkInner = new HtmlCommandButton();
//							checkInner.setId(getId()+"_check");
//							checkInner.getAttributes().put(PARAMETER_RESOURCE_PATH, resource.getPath());
//							checkInner.setStyleClass("wf_listlink");
//							checkInner.setActionListener(WFUtil.createMethodBinding("#{contentviewerbean.processAction}", new Class[]{ActionEvent.class}));
//							getBundle().getLocalizedUIComponent("check_in", checkInner);
//							checkInner.getAttributes().put(ACTION, ACTION_CHECK_IN);
//
//							table.add(" ", 2, row);
//							table.add(checkInner, 2, row);
//
//						} else {
//							HtmlOutputText comm = new HtmlOutputText();
//							comm.setValue("("+VersionHelper.getCheckedOutName(resource)+")");
//							comm.setStyleClass("wf_listtext");
//							table.add(comm, 2, row);
//						}
					}
				}

				//Metadata
				WebDAVMetadata metadataUI = new WebDAVMetadata(getCurrentResourcePath());
				++row;
				table.mergeCells(1, row, 2, row);
				table.add(metadataUI, 1, row);

				//Categories
				WebDAVCategories categoriesUI = new WebDAVCategories(getCurrentResourcePath(), IWContext.getIWContext(context).getCurrentLocale().toString());
				categoriesUI.setId(this.getId()+"_categories");
				++row;
				table.mergeCells(1, row, 2, row);
				table.add(categoriesUI, 1, row);

				if (this.useVersionControl) {
					//Then add the version table
					Table vTable = getVersionReportTable(resource);

					++row;
					table.mergeCells(1, row, 2, row);
					table.add(vTable, 1, row);
				}
			}
			this.getChildren().add(table);
	}

	protected Table getVersionReportTable(RepositoryItem resource) {
		Locale locale = IWContext.getInstance().getCurrentLocale();
		Timer timer = new Timer();
		timer.start();

		List<RepositoryItemVersionInfo> versions = null;
		try {
			versions = getRepositoryService().getVersions(resource.getParentPath(), resource.getName());
		} catch (RepositoryException e) {
			e.printStackTrace();
		}
		if (ListUtil.isEmpty(versions))
			return new Table();

		Table vTable = new Table(8,versions.size()+1);
		vTable.setId(vTable.getId() + "_ver");
		vTable.setRowStyleClass(1,"wf_listheading");
		vTable.setStyleClass("wf_listtable");

		int vRow = 1;
		int vColumn = 1;
		vTable.add(getBundle().getLocalizedText("version"), vColumn, vRow);
		vTable.add(getBundle().getLocalizedText("download"), ++vColumn, vRow);
		vTable.add(getBundle().getLocalizedText("created_by"), ++vColumn, vRow);
		vTable.add(getBundle().getLocalizedText("comment"), ++vColumn, vRow);
		vTable.add(getBundle().getLocalizedText("checkout"), ++vColumn, vRow);
		vTable.add(getBundle().getLocalizedText("checkin"), ++vColumn, vRow);
		vTable.add(getBundle().getLocalizedText("last_modified"), ++vColumn, vRow);

		for (RepositoryItemVersionInfo version: versions) {
			vColumn = 0;
			++vRow;

			if(vRow%2==0){
				vTable.setRowStyleClass(vRow,"wf_listevenrow");
			}
			else{
				vTable.setRowStyleClass(vRow,"wf_listoddrow");
			}


			String versionName = version.getName();
			vTable.add(WFUtil.getText(versionName,"wf_listtext"), ++vColumn, vRow);

			DownloadLink versionPath = new DownloadLink(getBundle().getLocalizedString("download"));
			versionPath.setId("dl_"+vRow);
			versionPath.setStyleClass("wf_listlink");
			if (versionName != null) {
				String url = version.getPath();
				versionPath.setRelativeFilePath(url);
			}

			//so we have a sensable name for the file!
			if(versionName!=null){
				String fileName = "v"+versionName.replace('.','_')+"-"+resource.getName();
				versionPath.setAlternativeFileName(fileName);
			}
			//versionPath.getChildren().add(WFUtil.getText("Download/View"));
			vTable.add(versionPath, ++vColumn, vRow);
			vTable.add(WFUtil.getText(/*version.getCreatorDisplayName()*/"Version creator display name","wf_listtext"), ++vColumn, vRow);	//	TODO

			String tComment = version.getComment();	//	TODO
			HtmlOutputText comment = new HtmlOutputText();
			comment.setStyleClass("wf_listtext");
			if ("INITIAL VERSION.".equals(tComment)) {
				getBundle().getLocalizedUIComponent("initial_version", comment);
			} else {
				comment.setValue(tComment);
			}
			vTable.add(comment, ++vColumn, vRow);
//			vTable.add(WFUtil.getText(version.getComment(),"wf_listtext"), ++vColumn, vRow);
			vTable.add(WFUtil.getText(version.getCheckedOut(),"wf_listtext"), ++vColumn, vRow);	//	TODO
			vTable.add(WFUtil.getText(version.getCheckedIn(),"wf_listtext"), ++vColumn, vRow);	//	TODO
//			vTable.add(WFUtil.getText(version.getLastModified(),"wf_listtext"), ++vColumn, vRow);
			vTable.add(WFUtil.getText(new IWTimestamp(resource.getLastModified()).getLocaleDateAndTime(locale, IWTimestamp.MEDIUM, IWTimestamp.MEDIUM),"wf_listtext"), ++vColumn, vRow);
		}

		++vRow;
		vTable.add("Creation time", 3, vRow);
		vTable.add(timer.getTimeString(), 4, vRow);

		timer.stop();

		return vTable;
	}

	private void toggleLock(JCRItem resource) {
		if (resource.isLocked()) {
			resource.unlock();
		} else {
			resource.lock(true, false);
		}
		super.refreshList();
	}


	@Override
	public void processAction(ActionEvent event) throws AbortProcessingException {
		UIComponent comp = (UIComponent) event.getSource();
		String action = (String) comp.getAttributes().get(ACTION);
		String path = (String) comp.getAttributes().get(PARAMETER_RESOURCE_PATH);
		JCRItem res = getWebdavExentededResource(path);
		try {
			if (ACTION_TOGGLE_LOCK.equals(action)) {
				if (res != null) {
					toggleLock(res);
				}
			} else if (ACTION_CHECK_OUT.equals(action)) {
//				IWSlideSession ss = (IWSlideSession) IBOLookup.getSessionInstance(IWContext.getInstance(), IWSlideSession.class);
//				VersionHelper.checkOut(res, ss.getUserFullName());
				getRepositoryService().getVersionManager().checkout(path);
				refreshList();
			} else if (ACTION_CHECK_IN.equals(action)) {
//				VersionHelper.checkIn(res);
				getRepositoryService().getVersionManager().checkin(path);
				refreshList();
			} else if (ACTION_UNCHECK_OUT.equals(action)) {
//				VersionHelper.unCheckOut(res);
				res.unCheckOut();
				refreshList();
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	public void setDetailed(boolean detailed) {
		this.detailed = detailed;
	}

	public void setUseVersionControl(boolean useVersionControl) {
		this.useVersionControl = useVersionControl;
	}

	@Override
	public Object saveState(FacesContext ctx) {
		Object values[] = new Object[3];
		values[0] = super.saveState(ctx);
		values[1] = new Boolean(this.detailed);
		values[2] = new Boolean(this.useVersionControl);
		return values;
	}

	@Override
	public void restoreState(FacesContext ctx, Object state) {
		Object values[] = (Object[]) state;
		super.restoreState(ctx, values[0]);
		this.detailed = ((Boolean) values[1]).booleanValue();

		this.useVersionControl = ((Boolean) values[2]).booleanValue();
	}
}